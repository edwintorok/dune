open! Stdune

module Name = struct
  module T = Interned.Make(struct
      let initial_size = 16
      let resize_policy = Interned.Conservative
      let order = Interned.Natural
    end)()

  include T

  let of_string = make

  let opam_fn (t : t) = to_string t ^ ".opam"

  let meta_fn (t : t) = "META." ^ to_string t

  let version_fn (t : t) = to_string t ^ ".version"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let decode = Dune_lang.Decoder.(map string ~f:of_string)

  let encode t = Dune_lang.Encoder.(string (to_string t))

  let to_dyn t = Dyn.Encoder.string (to_string t)

  module Infix = Comparator.Operators(T)
end

module Version_source = struct
  type t =
    | Package
    | Project

  let to_dyn t =
    Dyn.Variant
      ((match t with
         | Package -> "Package"
         | Project -> "Project"),
       [])
end

module Dependency = struct
  module Op = struct
    type t =
      | Eq
      | Gte
      | Lte
      | Gt
      | Lt
      | Neq

    let map =
      [ "=", Eq
      ; ">=", Gte
      ; "<=", Lte
      ; ">", Gt
      ; "<", Lt
      ; "<>", Neq
      ]

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Eq -> string "Eq"
      | Gt -> string "Gt"
      | Gte -> string "Gte"
      | Lte -> string "Lte"
      | Lt -> string "Lt"
      | Neq -> string "Neq"

    let to_relop : t -> OpamParserTypes.relop = function
      | Eq -> `Eq
      | Gte -> `Geq
      | Lte -> `Leq
      | Gt -> `Gt
      | Lt -> `Lt
      | Neq -> `Neq

    let from_relop : OpamParserTypes.relop -> t = function
      | `Eq -> Eq
      | `Geq -> Gte
      | `Leq -> Lte
      | `Gt -> Gt
      | `Lt -> Lt
      | `Neq -> Neq

    let encode t = t |> to_relop |> OpamPrinter.relop |> Dune_lang.atom

  end

  module Constraint = struct
    module Var = struct
      type t =
        | QVar of string
        | Var of string

      let decode =
        let open Dune_lang.Decoder in
        let+ s = string in
        if String.is_prefix s ~prefix:":" then
          Var (String.drop s 1)
        else
          QVar s

      let encode =
        let open Dune_lang.Encoder in
        function
        | QVar s -> string s
        | Var s -> string (":" ^ s)

      let to_opam : t -> OpamParserTypes.value =
        let nopos = Opam_file.nopos in
        function
        | QVar x -> String (nopos, x)
        | Var x -> Ident (nopos, x)
    end

    type t =
      | Bvar of Var.t
      | Uop of Op.t * Var.t
      | And of t list
      | Or of t list

    let decode =
      let open Dune_lang.Decoder in
      let ops =
        List.map Op.map ~f:(fun (name, op) ->
          name, (let+ x = Var.decode in Uop (op, x)))
      in
      let ops =
        ("!=",
         let+ loc = loc in
         User_error.raise ~loc [ Pp.text "Use <> instead of !=" ])
        :: ops
      in
      fix begin fun t ->
        let logops =
          [ "and", (let+ x = repeat t in And x)
          ; "or", (let+ x = repeat t in Or x)
          ]
        in
        peek_exn >>= function
        | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
          let+ () = junk in
          Bvar (Var (String.drop s 1))
        | _ ->
          sum (ops @ logops)
      end

    let rec encode =
      let open Dune_lang.Encoder in
      function
      | Bvar var -> Var.encode var
      | Uop (op, var) -> pair Op.encode Var.encode (op, var)
      | And lst -> pair string (list encode) ("and", lst)
      | Or lst -> pair string (list encode) ("or", lst)

    let rec to_dyn =
      let open Dyn.Encoder in
      function
      | Bvar (QVar v) -> constr "Bvar" [Dyn.String v]
      | Bvar (Var v) -> constr "Bvar" [Dyn.String (":" ^ v)]
      | Uop (b, QVar v) -> constr "Uop" [Op.to_dyn b; Dyn.String v]
      | Uop (b, Var v) -> constr "Uop" [Op.to_dyn b; Dyn.String (":" ^ v)]
      | And t -> constr "And" (List.map ~f:to_dyn t)
      | Or t -> constr "Or" (List.map ~f:to_dyn t)
  end

  type t =
    { name : Name.t
    ; constraint_ : Constraint.t option
    }

  let decode =
    let open Dune_lang.Decoder in
    let constrained =
      let+ name = Name.decode
      and+ expr = Constraint.decode
      in
      { name
      ; constraint_ = Some expr
      }
    in
    if_list
      ~then_:(enter constrained)
      ~else_:(
        let+ name = Name.decode in
        { name
        ; constraint_ = None
        })

  let encode { name; constraint_ } =
    let open Dune_lang.Encoder in
    match constraint_ with
    | None -> Name.encode name
    | Some c -> pair Name.encode Constraint.encode (name, c)

  let rec opam_constraint : Constraint.t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    function
    | Bvar v -> Constraint.Var.to_opam v
    | Uop (op, v) ->
      Prefix_relop (nopos, Op.to_relop op, Constraint.Var.to_opam v)
    | And [c] -> opam_constraint c
    | And (c :: cs) ->
      Logop (nopos, `And, opam_constraint c, opam_constraint (And cs))
    | Or [c] -> opam_constraint c
    | Or (c :: cs) ->
      Logop (nopos, `Or, opam_constraint c, opam_constraint (And cs))
    | And []
    | Or [] -> Code_error.raise "opam_constraint" []

  let rec constraint_from_opam : OpamParserTypes.value -> Constraint.t =
    function
    | Prefix_relop (_, relop, String(_, x)) ->
      Uop(Op.from_relop relop, Constraint.Var.QVar x)
    | Prefix_relop (_, relop, Ident(_, x)) ->
      Uop(Op.from_relop relop, Constraint.Var.Var x)
    | Logop(_, `And, c, cs) ->
      (match constraint_from_opam cs with
      | And cs -> And(constraint_from_opam c :: cs)
      | _ -> assert false (* TODO: log *))
    | Logop(_, `Or, c, cs) ->
      (match constraint_from_opam cs with
       (* TODO: is And correct here? *)
      | And cs -> Or(constraint_from_opam c :: cs)
      | _ -> assert false (* TODO: log *))
    | String(_, x) -> Bvar (Constraint.Var.QVar x)
    | Ident(_, x) -> Bvar (Constraint.Var.Var x)
    | _ -> assert false (* TODO *)

  let opam_depend : t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    fun { name; constraint_ } ->
      let constraint_ = Option.map ~f:opam_constraint constraint_ in
      let pkg : OpamParserTypes.value =
        String (nopos, Name.to_string name) in
      match constraint_ with
      | None -> pkg
      | Some c -> Option (nopos, pkg, [c])

  let from_opam : OpamParserTypes.value -> t = function
    | String (_, s) -> { name = Name.of_string s; constraint_ = None }
    | Option (_, String (_, s), [ c ]) ->
      { name = Name.of_string s;
        constraint_ = Some (constraint_from_opam c) }
    | _ -> assert false (* TODO *)

  let to_dyn { name; constraint_ } =
    let open Dyn.Encoder in
    record
      [ "name", Name.to_dyn name
      ; "constr", Dyn.Option (Option.map ~f:Constraint.to_dyn constraint_)
      ]
end

module Kind = struct
  type has_opam = bool
  type t =
    | Dune of has_opam
    | Opam

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Dune b -> constr "Dune" [bool b]
    | Opam -> constr "Opam" []
end

type t =
  { name                   : Name.t
  ; loc                    : Loc.t
  ; synopsis               : string option
  ; description            : string option
  ; depends                : Dependency.t list
  ; conflicts              : Dependency.t list
  ; depopts                : Dependency.t list
  ; path                   : Path.Source.t
  ; version                : (string * Version_source.t) option
  ; kind                   : Kind.t
  ; tags                   : string list
  }

(* Package name are globally unique, so we can reasonably expect that
   there will always be only a single value of type [t] with a given
   name in memory. That's why we only hash the name. *)
let hash t = Name.hash t.name

let decode ~dir =
  let open Dune_lang.Decoder in
  fields @@
  let+ loc = loc
  and+ name = field "name" Name.decode
  and+ synopsis = field_o "synopsis" string
  and+ description = field_o "description" string
  and+ depends =
    field ~default:[] "depends" (repeat Dependency.decode)
  and+ conflicts =
    field ~default:[] "conflicts" (repeat Dependency.decode)
  and+ depopts =
    field ~default:[] "depopts" (repeat Dependency.decode)
  and+ tags = field "tags" (list string) ~default:[]
  in
  { name
  ; loc
  ; synopsis
  ; description
  ; depends
  ; conflicts
  ; depopts
  ; path = dir
  ; version = None
  ; kind = Dune false
  ; tags
  }

let to_dyn { name; path; version ; synopsis ; description
           ; depends ; conflicts ; depopts ; kind ; tags ; loc = _ } =
  let open Dyn.Encoder in
  record
    [ "name", Name.to_dyn name
    ; "path", Path.Source.to_dyn path
    ; "synopsis", option string synopsis
    ; "description", option string description
    ; "depends", list Dependency.to_dyn depends
    ; "conflicts", list Dependency.to_dyn conflicts
    ; "depopts", list Dependency.to_dyn depopts
    ; "kind", Kind.to_dyn kind
    ; "tags", list string tags
    ; "version",
      Option (Option.map version ~f:(fun (v, s) ->
        Dyn.Tuple [String v; Version_source.to_dyn s]))
    ]

let encode { name; path = _; version ; synopsis ; description
           ; depends ; conflicts ; depopts ; kind = _ ; tags ; loc = _ } =
  let version = match version with
    | Some (v, Version_source.Package) -> Some v
    | Some (_, Version_source.Project) | None -> None
  in
  Dune_lang.List (Dune_lang.atom "package" :: Dune_lang.Encoder.(record_fields
      [ field "name" Name.encode name
      ; field_o "version" string version
      ; field_o "synopsis" string synopsis
      ; field_o "description" string description
      ; field_l "depends" Dependency.encode depends
      ; field_l "conflicts" Dependency.encode conflicts
      ; field_l "depopts" Dependency.encode depopts
      ; field_l "tags" string tags
      ]))

let opam_file t = Path.Source.relative t.path (Name.opam_fn t.name)

let meta_file t = Path.Source.relative t.path (Name.meta_fn t.name)

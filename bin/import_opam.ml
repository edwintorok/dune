open Stdune

open Import

let doc = "Import opam files into a dune-project"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Loads opam fields and version constraints from opam files.
       This can be combined with $(b,dune external-lib-deps --update-project),
       to update the dependencies while honoring existing version constraints |}
  ; `Blocks Common.help_secs ]

let info = Term.info "import-opam" ~doc ~man

let term =
  let+ common = Common.term in
  Common.set_common common ~targets:[];
  let log = Log.create common in
  Dune.File_tree.load Path.Source.root
    ~warn_when_seeing_jbuild_file:true
    ~ancestor_vcs:None
  |> Dune.Opam_importer.import ~log


let command = term, info

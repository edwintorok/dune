open Stdune
open Import

let doc = "Sycnchronise opam dependencies from external-lib-deps"

let info = Term.info "sync-deps" ~doc ~man

let term =
  let common = Common.term in
  let log = Log.create common in
  Scheduler.go ~log ~common (fun () ->
    let* setup = Import.Main.setup ~log common ~external_lib_deps_mode:true in
    let solve target =
      let targets = Target.resolve_targets_exn ~log common setup [ target ] in
      let request = Target.request setup targets in
      Build_system.all_lib_deps ~request
    in
    let+ dev = solve "@all" in
    let+ with_test = solve "@runtest" in
    let+ default = solve "@install" in
    let package = Common.only_packages common |> Option.value_exn
                  |> Path.Set.min_elt |> Option.value_exn
    in
    let name = Package.Name.of_string package in
    let opamfile = Opam_file.load (name |> Package.opam_file) in
    let tree = Dune.File_tree.load Path.Source.root
      ~warn_when_seeing_jbuild_files:true
      ~ancestor_vcs:None in
    let root = File_tree.root tree in
    let dir = File_tree.Dir.path root in
    let files = File_tree.Dir.files root in
    let proj = Dune_project.load ~dir ~files
               |> Option.value ~default:(Lazy.force Dune_project.anonymous) in
    (* TODO: update deps not from opam here *)
    let proj = Dune_project.update_from_opam
                 (Package.Name.Map.singleton name opamfile) proj in
    let file = Dune_project.file proj in
    Log.infof log "Generating %s" (Path.Source.to_string_maybe_quoted file);
    let _:Dune_project.created_or_already_exist =
      Dune_project.update_project_file proj ~f in
    ()
    (*equest:(unit, unit) Build.t ->
Lib_deps_info.t Path.Source.Map.t Stdune.String.Map.t Fiber.t "@install" in*)

  )

let command = term, info

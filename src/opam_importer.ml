open! Stdune

let load_opam_file p =
  Opam_file.load (Path.source p.Package.path)

let import ~log tree =
  let root = File_tree.root tree in
  let dir = File_tree.Dir.path root in
  let files = File_tree.Dir.files root in
  let proj = Dune_project.load ~dir ~files
             |> Option.value ~default:(Lazy.force Dune_project.anonymous) in
  let f = proj |> Dune_project.packages |> Package.Name.Map.map ~f:load_opam_file
  |> Dune_project.update_from_opam in
  let file = Dune_project.file proj in
  Log.infof log "Generating %s" (Path.Source.to_string_maybe_quoted file);
  let _:Dune_project.created_or_already_exist =
    Dune_project.update_project_file proj ~f in
  ()

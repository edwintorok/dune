open! Stdune


let import ~log tree =
  let load_opam_file p =
    let path = Path.relative (Path.source p.Package.path)
                 (Package.Name.opam_fn p.Package.name) in
    Log.infof log "Loading opam file %s" (Path.to_string path);
    Opam_file.load path
  in
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

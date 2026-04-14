let () =
  let print_usage () =
    Printf.printf "Usage: %s <server | client>\n" Sys.argv.(0)
  in
  if Array.length Sys.argv < 2 then print_usage ()
  else
    match Sys.argv.(1) with
    | "server" -> Server.run_server ()
    | "client" -> Server.run_client ()
    | _ -> print_usage ()

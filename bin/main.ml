open Aloe

let load path =
  let content =
    try In_channel.with_open_text path In_channel.input_all with
    | Sys_error message ->
        prerr_endline ("IOError: " ^ message);
        exit 1
  in
  let output, _ = Interp.interp ~filename:path content Interp.initial_env in
  if String.length output > 0 && output <> "nil" then print_endline output

let () = if Array.length Sys.argv > 1 then load Sys.argv.(1) else () |> Repl.repl |> Lwt_main.run

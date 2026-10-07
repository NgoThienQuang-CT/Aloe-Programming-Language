open Aloe
open Lwt.Syntax

let prompt =
  LTerm_text.eval
    [ LTerm_text.S "aloe ";
      LTerm_text.B_fg LTerm_style.lgreen;
      LTerm_text.B_bold true;
      LTerm_text.S ">>";
      LTerm_text.E_bold;
      LTerm_text.E_fg;
      LTerm_text.S " "
    ]

class aloe_read_line ~term ~history =
  object (self)
    inherit LTerm_read_line.read_line ~history () as super
    inherit [Zed_string.t] LTerm_read_line.term term as super_term
    method show_box = false

    method! stylise last =
      let text, pos = super#stylise last in
      Highlight.highlight text;
      (text, pos)

    method! private exec ?(keys = []) =
      function
      | Accept :: actions when React.S.value self#mode = LTerm_read_line.Edition ->
          let txt = self#edit |> Zed_edit.text |> Zed_rope.to_string |> Zed_string.to_utf8 in
          let ctx = Input.check txt Input.empty_ctx in
          if Input.is_done ctx then super_term#exec ~keys (Accept :: actions)
          else begin
            self#insert (Uchar.of_char '\n');
            self#exec ~keys actions
          end
      | actions -> super_term#exec ~keys actions

    method! send_action action =
      match action with
      (* 1. Up arrow: move to previous line if not on the first line *)
      | History_prev when Zed_edit.line self#context > 0 ->
          LTerm_read_line.Edit (LTerm_edit.Zed Zed_edit.Prev_line) |> super#send_action
      (* 2. Down arrow: move to next line if not on the last line *)
      | History_next when Zed_edit.line self#context < Zed_lines.count (Zed_edit.lines self#edit) ->
          LTerm_read_line.Edit (LTerm_edit.Zed Zed_edit.Next_line) |> super#send_action
      | _ -> super#send_action action

    initializer
      self#set_prompt (React.S.const prompt);

      self#bind [ { control = false; meta = true; shift = false; code = Enter } ] [ Accept ];

      self#bind
        [ { control = false; meta = false; shift = false; code = Tab } ]
        [ Edit (LTerm_edit.Zed (Zed_edit.Insert_str (Zed_string.of_utf8 "  "))) ]
  end

let println term str =
  let* () = LTerm.fprintls term (LTerm_text.of_utf8 str) in
  LTerm.flush term

let rec loop env term history =
  let* opt_input =
    Lwt.catch
      (fun () ->
        let rl = new aloe_read_line ~term ~history:(LTerm_history.contents history) in
        let* input = rl#run in
        Lwt.return_some input )
      (function
        | Sys.Break ->
            let* () = println term "Interrupted." in
            Lwt.return (Some (Zed_string.empty ()))
        | LTerm_read_line.Interrupt ->
            let* () = println term "exit" in
            Lwt.return_none
        | exn -> Lwt.fail exn )
  in
  match opt_input with
  | None -> Lwt.return_unit
  | Some zed_input ->
      let input = Zed_string.to_utf8 zed_input in
      if String.is_empty (String.trim input) then loop env term history
      else begin
        LTerm_history.add history zed_input;
        let* () = LTerm.flush term in
        let* env' =
          try
            let output, new_env = Interp.interp input env in
            let* () =
              match output with
              | "nil" -> Lwt.return_unit
              | _ -> println term output
            in
            Lwt.return new_env
          with
          | Sys.Break -> Lwt.return env
        in
        loop env' term history
      end

let repl () =
  Sys.catch_break true;
  let* term = Lazy.force LTerm.stdout in
  loop Interp.initial_env term (LTerm_history.create [])

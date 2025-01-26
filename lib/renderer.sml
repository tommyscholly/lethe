structure Renderer =
struct
  open Chars
  (* line_content, start_column, end_column, msg *)
  datatype Line =
    SingleLine of string * int * int * string
  (* line_content, start_column, end_column *)
  | MultiLineStart of string * int * int
  (* line_content, start_column, end_column, msg *)
  | MultiLineEnd of string * int * int * string

  (* line, line_number, output color *)
  datatype Render =
    RenderLine of Line * int * string

  fun pad_left str n =
    if n = 0 then str else str ^ pad_left str (n - 1)

  (* returns both the padding for the line, and the underline *)
  fun line_padding line_no in_multi =
    let
      val line_no_padding = " " ^ Int.toString line_no ^ " "
      val multi_padding = if in_multi then " " ^ (#vbar Chars.unicode) else ""
      (* 3, 5 *)
      val line_padding = if in_multi then "   " else "     "

      val underline_vbar =
        (StringCvt.padLeft #" " (String.size line_no_padding) "")
        ^ (#vbar_gap Chars.unicode)
      val underline_padding = " "
    (* this probably can just be " " *)
    (* StringCvt.padLeft #" " (String.size info_padding - 3) "" *)
    in
      ( line_no_padding ^ (#vbar Chars.unicode) ^ multi_padding (* )^ "   " *)
      , underline_vbar ^ (if in_multi then multi_padding else underline_padding)
      , line_padding
      )
    end


  fun render_line (line: Line, line_number: int, color: string, in_multi: bool) :
    string =
    let
      val (line_padding, underline_padding, end_pad) =
        line_padding line_number in_multi
    in
      case line of
        SingleLine (content, start_col, end_col, msg) =>
          line_padding ^ end_pad ^ content ^ "\n" ^ underline_padding ^ end_pad
          ^ StringCvt.padLeft #" " (start_col - 1) ""
          ^
          Colors.with_this_color color
            (StringCvt.padLeft (#underline Chars.ascii) (end_col - start_col) ""
             ^ "  " ^ msg ^ "\n")
      | MultiLineStart (content, start_col, end_col) =>
          if in_multi then
            raise Fail "nested multiline"
          else
            let
            (* val colored_content = String.substring *)
            (*   (content, start_col - 1, end_col - start_col + 1) *)
            in
              line_padding ^ (" " ^ (#ltop Chars.unicode))
              ^ (#hbar Chars.unicode) ^ (#rarrow Chars.unicode) ^ " " ^ content
              ^ "\n"
            end
      | MultiLineEnd (content, start_col, end_col, msg) =>
          if not in_multi then
            raise Fail "multiline end without start"
          else
            line_padding ^ end_pad ^ content ^ "\n"
            ^
            (String.substring
               (underline_padding, 0, String.size underline_padding - 3))
            ^ (#lbot Chars.unicode) ^ (pad_left (#hbar Chars.unicode) 3)
            ^ (#rarrow Chars.unicode) ^ " " ^ (Colors.with_this_color color msg)
            ^ "\n"
    (* | _ => "todo" *)


    end

  fun render (lines: Render list) = print "todo"
end

val _ = print "Running renderer tests\n"
(* val rend = print *)
(*   ((Renderer.render_line *)
(*       (Renderer.SingleLine ("hello", 1, 5, "world"), 1, Colors.red, false)) *)
(*    ^ "\n") *)

val test_multi =
  Renderer.render_line
    (Renderer.MultiLineStart ("hello", 1, 5), 1, Colors.red, false)
  ^
  Renderer.render_line
    (Renderer.SingleLine ("world", 1, 5, "an error here"), 2, Colors.red, true)
  ^
  (Renderer.render_line
     ( Renderer.MultiLineEnd ("}", 1, 5, " expected <type>")
     , 3
     , Colors.red
     , true
     ))

val _ = print (test_multi ^ "\n")
val _ = print "render test done\n\n"

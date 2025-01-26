structure Renderer =
struct
  open Chars
  (* line_content, start_column, end_column, msg *)
  datatype Line =
    DiagnosticLine of string * int * int * string
  | NormalLine of string
  (* line_content, start_column, end_column *)
  | MultiLineStart of string * int * int
  (* start_column, end_column, msg *)
  | MultiLineEnd of int * int * string
  | SkipLine

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
      val line_padding = if in_multi then " " else "   "

      val underline_vbar = (StringCvt.padLeft #" " (String.size line_no_padding) "") ^ (#vbar_break Chars.unicode)
    in
      ( line_no_padding ^ (#vbar Chars.unicode) ^ multi_padding (* )^ "   " *)
      , underline_vbar ^ multi_padding
      , line_padding
      )
    end


  fun render_line (line: Line, line_number: int, color: string, in_multi: bool) : string =
    let
      val (line_padding, underline_padding, end_pad) = line_padding line_number in_multi
    in
      case line of
        SkipLine => underline_padding ^ "\n"
      | NormalLine content => line_padding ^ end_pad ^ content ^ "\n"
      | DiagnosticLine (content, start_col, end_col, msg) =>
          line_padding ^ end_pad ^ content ^ "\n" ^ underline_padding ^ end_pad
          ^ StringCvt.padLeft #" " (start_col - 1) ""
          ^
          Colors.with_this_color color
            (StringCvt.padLeft (#underline Chars.ascii) (end_col - start_col) "" ^ "  " ^ msg ^ "\n")
      | MultiLineStart (content, _, _) =>
          if in_multi then
            raise Fail "nested multiline"
          else
            (* val colored_content = String.substring *)
            (*   (content, start_col - 1, end_col - start_col + 1) *)
            line_padding ^ (" " ^ (#ltop Chars.unicode)) ^ (#hbar Chars.unicode) ^ content
            ^ "\n"
      | MultiLineEnd (_, end_col, msg) =>
          if not in_multi then
            raise Fail "multiline end without start"
          else
            (* (String.substring (line_padding, 0, String.size line_padding - 3)) ^ (#lcross Chars.unicode) *)
            (* ^ (#rarrow Chars.unicode) ^ (*end_pad ^ *) content ^ "\n" ^ *)
            (String.substring (underline_padding, 0, String.size
            underline_padding - 3)) ^ (#lbot Chars.unicode) 
            ^ (pad_left (#hbar Chars.unicode) end_col) ^ (#rbot Chars.unicode) ^
            " "^ (Colors.with_this_color color msg) ^ "\n"
    (* | _ => "todo" *)


    end

  fun render_rline (lines: Render list) (in_multi: bool) =
    case lines of
      [] => ""
    | (RenderLine (line, line_number, color)) :: rest =>
        render_line (line, line_number, color, in_multi)
        ^
        render_rline rest
          (case line of
             MultiLineStart _ => true
           | MultiLineEnd _ => false
           | _ => in_multi)

  fun render_report_header file =
    let val _ = print ("   " ^ (#ltop Chars.unicode) ^ (#hbar Chars.unicode))
    in print ("[" ^ file ^ "]:\n")
    end

  fun render_report_footer () =
    let val _ = print (pad_left (#hbar Chars.unicode) 2)
    in print ((#rbot Chars.unicode) ^ "\n")
    end

  fun render file lines =
    let
      val rendering = render_rline lines false
    in
      print ("   " ^ (#ltop Chars.unicode) ^ (#hbar Chars.unicode));
      print ("[" ^ file ^ "]:\n");

      print rendering;
      print (pad_left (#hbar Chars.unicode) 2);
      print ((#rbot Chars.unicode) ^ "\n")
    end
end

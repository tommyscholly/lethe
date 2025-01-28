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

  val padding_req = ref 0

  fun set_padding n = padding_req := n

  fun pad_left str n =
    if n = 0 then str else str ^ pad_left str (n - 1)

  (* returns both the padding for the line, and the underline *)
  fun line_padding line_no in_multi =
    let
      val line_no_padding = " " ^ Int.toString line_no ^ " "
      val line_no_padding = if String.size line_no_padding <
      !padding_req then
        pad_left " " (!padding_req - String.size line_no_padding - 1) ^
        line_no_padding
      else line_no_padding
      (* 1 *)
      val multi_padding = if in_multi then " " ^ (Chars.unicode Chars.Vbar) else ""
      (* 3, 5 *)
      val line_padding = if in_multi then " " else "   "

      val underline_vbar = (StringCvt.padLeft #" " (String.size
      line_no_padding) "") ^ (Chars.ascii Chars.VbarGap)
    in
      ( line_no_padding ^ (Chars.unicode Chars.Vbar) ^ multi_padding (* )^ "   " *)
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
            (pad_left (Chars.ascii Chars.Underline) (end_col - start_col) ^ "  " ^ msg ^ "\n")
      | MultiLineStart (content, _, _) =>
          if in_multi then
            raise Fail "nested multiline"
          else
            (* val colored_content = String.substring *)
            (*   (content, start_col - 1, end_col - start_col + 1) *)
            line_padding ^ (" " ^ (Chars.unicode Chars.LTop)) ^ (Chars.unicode Chars.Hbar) ^ content ^ "\n"
      | MultiLineEnd (_, end_col, msg) =>
          if not in_multi then
            raise Fail "multiline end without start"
          else
            (* (String.substring (line_padding, 0, String.size line_padding - 3)) ^ (#lcross Chars.unicode) *)
            (* ^ (#rarrow Chars.unicode) ^ (*end_pad ^ *) content ^ "\n" ^ *)
            (String.substring (underline_padding, 0, String.size underline_padding - 3)) ^ (Chars.unicode Chars.LBot)
            ^ (pad_left (Chars.unicode Chars.Hbar) end_col) ^ (Chars.unicode Chars.RBot) ^ " "
            ^ (Colors.with_this_color color msg) ^ "\n"
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

  fun render_report_header pad_length file =
    let val _ = print ((pad_left " " (pad_length - 1)) ^ (Chars.unicode Chars.LTop) ^ (Chars.unicode Chars.Hbar))
    in print (" " ^ file ^ ":\n")
    end

  fun render_report_footer padding_indent() =
    let val _ = print (pad_left (Chars.unicode Chars.Hbar)
    (padding_indent - 1))
    in print ((Chars.unicode Chars.RBot) ^ "\n")
    end

  fun render file lines =
    let
      val rendering = render_rline lines false
      val pad_length = Util.idx_of (Chars.unicode Chars.Vbar) rendering
    in
      (* print ("   " ^ (Chars.unicode Chars.LTop) ^ (Chars.unicode Chars.Hbar)); *)
      (* print ("[" ^ file ^ "]:\n"); *)
      render_report_header (Option.valOf pad_length) file;

      print rendering
    (* render_report_footer () *)
    (* print (pad_left (Chars.unicode Chars.Hbar) 2); *)
    (* print ((Chars.unicode Chars.RBot) ^ "\n") *)
    end
end

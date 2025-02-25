structure Report :> REPORT =
struct
  open Colors
  open Util
  open LineRange
  open Renderer

  type Position = int * int

  datatype LabelKind = NORMAL | ERROR | WARNING | INFO
  fun kind_to_color NORMAL = Colors.white
    | kind_to_color ERROR = Colors.red
    | kind_to_color WARNING = Colors.yellow
    | kind_to_color INFO = Colors.cyan

  type selection = {sel_start: int, sel_end: int, sel_msg: string}

  type label = {kind: LabelKind, msg: string, label_start: int, label_end: int}

  type t = {file: string, selection: selection option, labels: label list, span: (int * int) option}

  fun make_report (file: string) : t =
    {file = file, selection = NONE, labels = [], span = NONE}

  fun add_label (report: t) (kind: LabelKind) (msg: string) (label_start: int) (label_end: int) : t =
    let
      val span =
        case #span report of
          NONE => SOME (label_start, label_end)
        | SOME (s_start, s_end) => SOME (Int.min (s_start, label_start), Int.max (s_end, label_end))
    in
      { file = #file report
      , selection = #selection report
      , labels = #labels report @ [{kind = kind, msg = msg, label_start = label_start, label_end = label_end}]
      , span = span
      }
    end

  fun set_selection (report: t) (sel_start: int) (sel_end: int) (sel_msg: string) : t =
    let
      val span =
        case #span report of
          NONE => SOME (sel_start, sel_end)
        | SOME (s_start, s_end) => SOME (Int.min (s_start, sel_start), Int.max (s_end, sel_end))
    in
      { file = #file report
      , selection = SOME {sel_start = sel_start, sel_end = sel_end, sel_msg = sel_msg}
      , labels = #labels report
      , span = span
      }
    end

  fun report_lines (report: t) =
    let
      val file_text = read_to_string (#file report)
      val lines: LineRange.line_info list =
        case (#span report) of
          NONE => []
        | SOME (s_start, s_end) => LineRange.find_lines file_text s_start s_end
    in
      lines
    end

  fun labels_for_line (report: t) (line: LineRange.line_info) =
    let
      fun label_in_range (label: label) =
        not (#label_end label < #start_pos line orelse #label_start label > #end_pos line)
    in
      List.filter label_in_range (#labels report)
    end

  fun output_report (report: t) =
    let
      val lines: LineRange.line_info list = report_lines report
      (* val report_content = String.concat (List.map (fn (l: LineRange.line_info) => #content l ^ "\n") lines) *)

      val padding_ident = List.foldl (fn (l,
      acc) => Int.max (String.size (Int.toString (#line_number l)), acc))
      0 lines + 2
      val _ = Renderer.set_padding padding_ident

      val num_skipped = ref 0
      val in_selection = ref false
      val has_selection = Option.isSome (#selection report)
      (* val (span_start, span_end) = Option.valOf (#span report) *)

      fun process_line (line: LineRange.line_info) =
        let
          val labels: label list = labels_for_line report line
          val line_number = #line_number line
          val line_start = #start_pos line
          val line_end = #end_pos line
          val line_content = #content line

          fun display () =
            if List.null labels then
              let
                (* val line = *)
                (*   Renderer.render_line (Renderer.NormalLine line_content, line_number, Colors.white, !in_selection) *)
                val line = if !num_skipped < 2 then
                  Renderer.render_line (Renderer.SkipLine, line_number
                  ,Colors.white, false)
                else ""
              in
                if has_selection then
                  let
                    val sel_start = #sel_start (Option.valOf (#selection report))
                  in
                    if line_start <= sel_start andalso line_end >= sel_start then
                      ( print (Renderer.render_line
                          (Renderer.MultiLineStart (line_content, ~1, ~1), line_number, Colors.white, false))
                      ; in_selection := true
                      )
                    else
                      num_skipped := !num_skipped + 1;
                      print line
                  end
                else
                  num_skipped := !num_skipped + 1;
                  print line
              end
            else
              let
                val label: label = List.hd labels
                val diag = Renderer.DiagnosticLine
                  (line_content, #label_start label - line_start + 1, #label_end label - line_start + 1, #msg label)
                val line = Renderer.render_line (diag, line_number, kind_to_color (#kind label), !in_selection)
              in
                num_skipped := 0;
                if has_selection then
                  let
                    val sel_end = #sel_end (Option.valOf (#selection report))
                    val sel_msg = #sel_msg (Option.valOf (#selection report))
                    val selection_end = Renderer.MultiLineEnd (0, sel_end - line_start, sel_msg)
                  in
                    if line_start <= sel_end andalso line_end >= sel_end then
                      ( print line
                      ; print (Renderer.render_line (selection_end, line_number, Colors.white, true))
                      ; in_selection := false
                      )
                    else
                      print line
                  end
                else
                  print line
              end
        in
          display ()
        end
    in
        Renderer.render_report_header padding_ident (#file report);
        List.app process_line lines;
        Renderer.render_report_footer padding_ident ()
    end
end

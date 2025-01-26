structure Lethe :> LETHE =
struct
  open Chars
  open LineRange
  open Util
  open Colors
  open Renderer
  open Report

  datatype LabelKind = NORMAL | ERROR | WARNING | INFO
  fun kind_to_color NORMAL = Colors.white
    | kind_to_color ERROR = Colors.red
    | kind_to_color WARNING = Colors.yellow
    | kind_to_color INFO = Colors.cyan

  type label = {range_start: int, range_end: int, msg: string, kind: LabelKind}
  type t = label

  type block = {file: string, range_start: int, range_end: int, block_msg: string, labels: t list}

  (* the text field holds all the data between the lines *)
  type text_info = {text: string, line_start: int, line_end: int}

  fun create_label msg range_start range_end (kind: LabelKind) : t =
    {range_start = range_start, range_end = range_end, msg = msg, kind = kind}

  fun read_to_string file =
    let
      val stream = TextIO.openIn file
      val text = TextIO.inputAll stream
    in
      TextIO.closeIn stream;
      text
    end

  fun label_to_line file (label: t) =
    let
      val file_contents = read_to_string file
      val lines: LineRange.line_info list = LineRange.find_lines file_contents (#range_start label) (#range_end label)
      val color = kind_to_color (#kind label)
      val msg = (#msg label)
    in
      if List.length lines = 1 then
        let
          val line: LineRange.line_info = List.hd lines
          val line_start = #start_pos line
          val line_end = #end_pos line
          val line_length = line_end - line_start

          (* calculate positions relative to line start *)
          val raw_start = #range_start label - line_start
          val raw_end = #range_end label - line_start

          (* clamp values to line boundaries *)
          val start_pos = Int.max (0, Int.min (raw_start, line_length))
          val end_pos = Int.max (0, Int.min (raw_end, line_length))

          (* ensure end >= start *)
          val end_pos = Int.max (start_pos, end_pos) + 1
        in
          [RenderLine (DiagnosticLine (#content line, start_pos, end_pos, msg), #line_number line, color)]
        end
      else
        [ RenderLine (MultiLineStart (#content (List.hd lines), ~1, ~1), #line_number (List.hd lines), color)
        , RenderLine (MultiLineEnd (~1, ~1, msg), #line_number (List.last lines), color)
        ]
    end

  fun report_error file (labels: t list) error_text error_no =
    let
      (* val labels: t list = *)
      (*   Util.quickSort (fn (l1: t, l2: t) => Int.compare (#range_start l1, #range_start l2)) labels *)
      val lines = List.concat (List.map (label_to_line file) labels)
      fun compare_lines (RenderLine (_, l1, _), RenderLine (_, l2, _)) = Int.compare (l1, l2)
      val lines = Util.quickSort compare_lines lines
    in
      print "Error";
      (case error_no of
         SOME no => print (" [" ^ (Int.toString no ^ "]"))
       | NONE => ());
      print (": " ^ error_text ^ "\n");

      Renderer.render file lines;
      ()
    end

  fun report_block b = ()
  fun report_label file l =
    let val lines = label_to_line file l
    in Renderer.render file lines
    end

  fun create_block file range_start range_end block_msg =
    {file = file, range_start = range_start, range_end = range_end, block_msg = block_msg, labels = []}

  fun add_label {file: string, range_start: int, range_end: int, block_msg: string, labels: t list} (label: label) :
    block =
    let
      val new_range_end = Int.max (range_end, #range_end label)
      val new_range_start = Int.min (range_start, #range_start label)
    in
      { file = file
      , range_start = new_range_start
      , range_end = new_range_end
      , block_msg = block_msg
      , labels = (label :: labels)
      }
    end
end

(* val _ = Lethe.print_line "Hello, world!" 1 8 12 *)
(*   "consider making this not so generic" *)
fun print_range file range_start range_end =
  let
    val file_contents = Lethe.read_to_string file
    val lines = LineRange.find_lines file_contents range_start range_end
  in
    List.app (fn line => print (#content line ^ "\n")) lines
  end (* fun test_fizzbuzz () = *) (*   let *) (*     val label = Lethe.create_label "test/fizzbuzz.hs" 0 5 "This is a test" Lethe.NORMAL *) (*     val label2 = Lethe.create_label "test/fizzbuzz.hs" 23 30 "Another test" Lethe.ERROR *) (*   in *) (*     Lethe.report_error "test/fizzbuzz.hs" [label, label2] "Test Error" NONE *) (*   end *) (**) (* fun test_actual_error () = *) (*   let *) (*     val label = Lethe.create_label "test/fizzbuzz.hs" 43 48 "Defined here" Lethe.INFO *) (*     val err_label = Lethe.create_label "test/fizzbuzz.hs" 166 167 "Expected String, got Int" Lethe.ERROR *) (*     val full_error = Lethe.create_label "test/fizzbuzz.hs" 43 180 "Return Type Mismatch" Lethe.INFO *) (*   in *) (*     Lethe.report_error "test/fizzbuzz.hs" [label, err_label, full_error] "Return Type Mismatch" NONE *) (*   end *) (* val _ = test_fizzbuzz () *) (* val _ = test_actual_error () (* val _ = print_range "test/fizzbuzz.hs" 140 180 *) *) (* val _ = Lethe.report_label "test/fizzbuzz.hs" (Lethe.create_label "Expected String, got Int" 166 167 Lethe.ERROR) *)

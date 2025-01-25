(* Taken from https://github.com/zesterer/ariadne/blob/main/src/draw.rs#L56 *)
structure Chars =
struct
  type chars =
    { hbar: char
    , vbar: char
    , xbar: char
    , vbar_break: char
    , vbar_gap: char
    , uarrow: char
    , rarrow: char
    , ltop: char
    , mtop: char
    , rtop: char
    , lbot: char
    , mbot: char
    , rbot: char
    , lbox: char
    , rbox: char
    , lcross: char
    , rcross: char
    , underbar: char
    , underline: char
    }

  type unicode_chars =
    { hbar: string
    , vbar: string
    , xbar: string
    , vbar_break: string
    , vbar_gap: string
    , uarrow: string
    , rarrow: string
    , ltop: string
    , mtop: string
    , rtop: string
    , lbot: string
    , mbot: string
    , rbot: string
    , lbox: string
    , rbox: string
    , lcross: string
    , rcross: string
    , underbar: string
    , underline: string
    }

  val unicode: unicode_chars =
    { hbar = "─"
    , vbar = "│"
    , xbar = "┼"
    , vbar_break = "┆"
    , vbar_gap = "┆"
    , uarrow = "▲"
    , rarrow = "▶"
    , ltop = "╭"
    , mtop = "┬"
    , rtop = "╮"
    , lbot = "╰"
    , mbot = "┴"
    , rbot = "╯"
    , lbox = "["
    , rbox = "]"
    , lcross = "├"
    , rcross = "┤"
    , underbar = "┬"
    , underline = "─"
    }

  val ascii: chars =
    { hbar = #"-"
    , vbar = #"|"
    , xbar = #"+"
    , vbar_break = #"*"
    , vbar_gap = #":"
    , uarrow = #"^"
    , rarrow = #">"
    , ltop = #","
    , mtop = #"v"
    , rtop = #"."
    , lbot = #"`"
    , mbot = #"^"
    , rbot = #"'"
    , lbox = #"["
    , rbox = #"]"
    , lcross = #"|"
    , rcross = #"|"
    , underbar = #"|"
    , underline = #"^"
    }
end

structure Lethe =
struct
  open LineRange
  open Util
  open Colors

  datatype LabelKind = NORMAL | ERROR | WARNING | INFO

  fun kind_to_color NORMAL = Colors.white
    | kind_to_color ERROR = Colors.red
    | kind_to_color WARNING = Colors.yellow
    | kind_to_color INFO = Colors.cyan

  type label =
    { file: string
    , range_start: int
    , range_end: int
    , msg: string
    , kind: LabelKind
    }

  (* the text field holds all the data between the lines *)
  type text_info = {text: string, line_start: int, line_end: int}

  fun create_label file range_start range_end msg (kind: LabelKind) : label =
    { file = file
    , range_start = range_start
    , range_end = range_end
    , msg = msg
    , kind = kind
    }

  fun read_to_string file =
    let
      val stream = TextIO.openIn file
      val text = TextIO.inputAll stream
    in
      TextIO.closeIn stream;
      text
    end

  fun pad_left str n =
    if n = 0 then str else str ^ pad_left str (n - 1)

  fun print_line line_text line_no underline_start underline_end hint_msg color =
    let
      val line_no_padding = " " ^ Int.toString line_no ^ " "
      val info_padding = (#vbar Chars.unicode) ^ "   "

      val underline_bar =
        (StringCvt.padLeft #" " (String.size line_no_padding) "")
        ^ (#vbar_gap Chars.unicode)
      val underline_text =
        StringCvt.padLeft #" " ((String.size info_padding) - 3) ""
        ^
        Colors.with_this_color color
          (StringCvt.padLeft (#underline Chars.ascii)
             (underline_end - underline_start) "" ^ "  " ^ hint_msg)
    in
      print (line_no_padding ^ info_padding);
      print line_text;
      print "\n";
      print (underline_bar ^ underline_text);
      print "\n"
    end

  fun print_label {file, range_start, range_end, msg, kind} =
    let
      val file_contents = read_to_string file
      val lines = LineRange.find_lines file_contents range_start range_end
    in
      if List.length lines = 1 then
        let
          val line = List.hd lines
          val line_start = #start_pos line
          val line_end = #end_pos line
          val line_length = line_end - line_start

          (* calculate positions relative to line start *)
          val raw_start = range_start - line_start
          val raw_end = range_end - line_start

          (* clamp values to line boundaries *)
          val start_pos = Int.max (0, Int.min (raw_start, line_length))
          val end_pos = Int.max (0, Int.min (raw_end, line_length))

          (* ensure end >= start *)
          val end_pos = Int.max (start_pos, end_pos)
        in
          print_line (#content line) (#line_number line) start_pos end_pos msg
            (kind_to_color kind)
        end
      else
        print "todo"
    end

  fun report_error file (labels: label list) error_text error_no =
    let
      val labels: label list =
        Util.quickSort
          (fn (l1: label, l2: label) =>
             Int.compare (#range_start l1, #range_start l2)) labels
    in
      print "Error";
      (case error_no of
         SOME no => print (" [" ^ (Int.toString no ^ "]"))
       | NONE => ());
      print (": " ^ error_text ^ "\n");
      print ("   " ^ (#ltop Chars.unicode) ^ (#hbar Chars.unicode));
      (* print ("   " ^ "__" ); *)
      print ("[" ^ file ^ "]:\n");

      List.app print_label labels;
      (* print (StringCvt.padLeft  3 ""); *)
      print (pad_left (#hbar Chars.unicode) 2);
      (* print ((#hbar Chars.unicode) ^ (#hbar Chars.unicode) ^ (#hbar Chars.unicode)); *)
      (* print (StringCvt.padLeft #"_" 4 ""); *)
      print ((#rbot Chars.unicode) ^ "\n");
      ()
    end
end

(* val _ = Lethe.print_line "Hello, world!" 1 8 12 *)
(*   "consider making this not so generic" *)

fun test_fizzbuzz () =
  let
    val label =
      Lethe.create_label "test/fizzbuzz.hs" 0 6 "This is a test" Lethe.NORMAL
    val label2 =
      Lethe.create_label "test/fizzbuzz.hs" 23 31 "Another test" Lethe.ERROR
  in
    Lethe.report_error "test/fizzbuzz.hs" [label, label2] "Test Error" NONE
  end

val _ = test_fizzbuzz ()

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

  datatype label_kind = NORMAL | SEVERE | WARNING | INFO

  type label =
    { file: string
    , range_start: int
    , range_end: int
    , msg: string
    , kind: label_kind
    }

  (* the text field holds all the data between the lines *)
  type text_info = {text: string, line_start: int, line_end: int}

  fun create_label file range_start range_end msg (kind: label_kind) : label =
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

  fun print_line line_text line_no underline_start underline_end hint_msg =
    let
      val info_padding =
        " " ^ Int.toString line_no ^ " " ^ (Char.toString (#vbar Chars.ascii))
        ^ "   "
      val underline_text =
        StringCvt.padLeft #" " (String.size info_padding) ""
        ^ StringCvt.padLeft #" " (underline_start - 1) ""
        ^
        StringCvt.padLeft (#underline Chars.ascii)
          (underline_end - underline_start + 1) "" ^ "  " ^ hint_msg
    in
      print info_padding;
      print line_text;
      print "\n";
      print underline_text;
      print "\n"
    end

  fun print_label {file, range_start, range_end, msg, kind} =
    let
      val file_contents = read_to_string file
      val lines = LineRange.find_lines file_contents range_start range_end
    in
      print (msg ^ "\n")
    (* print (lines ^ "\n") *)
    end

  fun report_error file (labels: label list) error_text error_no =
    let
      val labels: label list =
        Util.quickSort
          (fn (l1: label, l2: label) =>
             Int.compare (#range_start l1, #range_start l2)) labels
    in
      print "Error ";
      (case error_no of
         SOME no => print ("[" ^ (Int.toString no ^ "] "))
       | NONE => ());
      print error_text;

      print ("Error in file " ^ file ^ "\n")
    end
end

val _ = Lethe.print_line "Hello, world!" 1 8 12
  "consider making this not so generic"

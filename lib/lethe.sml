structure Lethe :> LETHE =
struct
  open Report
  open Util

  fun output_error file range_start range_end msg =
    let
      val report = make_report file
      (* val report = set_selection report range_start range_end msg *)
      val report = add_label report ERROR msg range_start range_end
    in
      output_report report
    end

  fun line_no_to_char_idx (file: string) (line_no: int) : int =
    let
      val file_content = Util.read_to_string file
      fun count_chars pos line_count =
        if line_count = line_no then pos
        else if pos >= String.size file_content then pos
        else if String.sub (file_content, pos) = #"\n" then count_chars (pos + 1) (line_count + 1)
        else count_chars (pos + 1) line_count
    in
      count_chars 0 1
    end

  datatype ErrorKind =
  (* helpful msg, line number, position in the line *)
    SyntaxError of string * int * Report.Position
  | ParseError of string * int * Report.Position
  (* (expected, got), line number, position in the line *)
  | FunctionAppError of (string * string) * int * Report.Position

  (* file *)
  datatype Error =
    Error of ErrorKind * string

  fun error_to_string (Error (kind, file)) =
    case kind of
      SyntaxError (msg, line, (char_start, char_end)) =>
        "Syntax error: " ^ msg ^ " in " ^ file ^ " at line " ^ Int.toString line ^ " from " ^ Int.toString char_start
        ^ " to " ^ Int.toString char_end
    | ParseError (msg, line, (char_start, char_end)) =>
        "Parse error: " ^ msg ^ " in " ^ file ^ " at line " ^ Int.toString line ^ " from " ^ Int.toString char_start
        ^ " to " ^ Int.toString char_end
    | FunctionAppError ((expected, got), line, (char_start, char_end)) =>
        "Function application error: expected " ^ expected ^ " but got " ^ got ^ " in " ^ file ^ " at line "
        ^ Int.toString line ^ " from " ^ Int.toString char_start ^ " to " ^ Int.toString char_end

  (* returns a tuple of the line number, start position, and end position *)
  (* range is in the form line.start-line.end *)
  fun parse_range (range: string) : (int * int * int) =
    let
      val range_split = String.fields (fn c => c = #"-") range
      val line_start = String.fields (fn c => c = #".") (List.nth (range_split, 0))
      val start_line_no = Int.fromString (List.nth (line_start, 0))
      val char_start = Int.fromString (List.nth (line_start, 1))
      val line_end = String.fields (fn c => c = #".") (List.nth (range_split, 1))
      (* end_line_no *)
      val _ = Int.fromString (List.nth (line_end, 0))
      val char_end = Int.fromString (List.nth (line_end, 1))
    in
      (Option.valOf start_line_no, Option.valOf char_start, Option.valOf char_end)
    end

  (* collects errors from the error text *)
  fun collect_errors text =
    if String.size text = 0 then
      []
    else
      let
        val errors = Util.split_words text

        fun parse_error ("Error:" :: file :: range :: rs) =
              let
                val (line, char_start, char_end) = parse_range range
                val (error_info, rest) = Util.take_until "Error:" rs
              in
                case error_info of
                  ("Syntax" :: "error:" :: rs') =>
                    (SOME (Error (SyntaxError (String.concatWith " " rs', line, (char_start - 1, char_end - 1)), file))
                     :: (parse_error rest))
                | ("Function" :: "applied" :: "to" :: "incorrect" :: "argument."
                :: rs) => 
                    let
                      val expected = List.nth (rs, 1)
                      val got = List.nth (rs, 4)
                    in
                      (SOME (Error (FunctionAppError ((expected, got), line, (char_start - 1, char_end - 1)), file))
                       :: (parse_error rest))
                    end
                | _ => NONE :: (parse_error rest)
              end
          | parse_error _ = [NONE]

        fun partial ([]: (Error option) list) : Error list = []
          | partial (x :: xs) =
              case x of
                NONE => partial xs
              | SOME e => e :: partial xs
      in
        partial (parse_error errors)
      end

  fun quote_args (args: string list) : string list =
    let
      fun needs_quotes s = String.isSubstring " " s
      fun quote s =
        if needs_quotes s then "\"" ^ s ^ "\"" else s
    in
      map quote args
    end

  fun error_to_report (Error (kind, file)) =
    case kind of
      SyntaxError (msg, line, (char_start, char_end)) =>
        let
          val line_start = line_no_to_char_idx file line
          val char_start = char_start + line_start
          val char_end = char_end + line_start
        in
          output_error file char_start char_end ("Syntax error: " ^ msg)
        end
    | ParseError (msg, line, (char_start, char_end)) => raise Fail "Not implemented"
    | FunctionAppError ((expected, got), line, (char_start, char_end)) => 
        let
          val line_start = line_no_to_char_idx file line
          val char_start = char_start + line_start
          val char_end = char_end + line_start
        in
          output_error file char_start char_end ("Function application error: expected " ^ expected ^ " but got " ^ got)
        end

  (* this function sits onto of MLton, parsing any errors that *)
  fun monitor () =
    let
      val compile_args = CommandLine.arguments ()
      val compile_args = quote_args compile_args
      (* val _ = print (String.concatWith "\n" compile_args ^ "\n") *)
      val error_file = OS.FileSys.tmpName ()
      val _ = OS.Process.system ("mlton " ^ (String.concatWith " " compile_args) ^ " 2> " ^ error_file)
      val error_text = Util.read_to_string error_file
      val errors = collect_errors error_text
    in
      if List.null errors then print "No errors found\n" else List.app error_to_report errors;
      OS.FileSys.remove error_file
    end
end

val _ = Lethe.monitor ()

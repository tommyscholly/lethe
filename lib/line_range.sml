structure LineRange =
struct
  type line_info = {line_number: int, start_pos: int, end_pos: int, content: string}

  fun find_lines (text: string) (start_pos: int) (end_pos: int) : line_info list =
    let
      fun make_line_info (num, start_p, end_p, content) : line_info =
        {line_number = num, start_pos = start_p, end_pos = end_p, content = content}

      fun process_lines (pos, line_num, (acc: line_info list)) : line_info list =
        let
          fun find_newline p =
            if p >= String.size text then NONE
            else if String.sub (text, p) = #"\n" then SOME p
            else find_newline (p + 1)
        in
          case find_newline pos of
            NONE =>
              if pos < String.size text then
                make_line_info (line_num, pos, String.size text - 1, String.substring
                  (text, pos, String.size text - pos)) :: acc
              else
                acc
          | SOME newline_pos =>
              process_lines
                ( newline_pos + 1
                , line_num + 1
                , make_line_info (line_num, pos, newline_pos, String.substring (text, pos, newline_pos - pos)) :: acc
                )
        end

      val all_lines = List.rev (process_lines (0, 1, []))

      fun line_in_range (info: line_info) =
        not (#end_pos info < start_pos orelse #start_pos info > end_pos)
    in
      List.filter line_in_range all_lines
    end
end

structure Util =
struct
  fun read_to_string file =
    let
      val stream = TextIO.openIn file
      val text = TextIO.inputAll stream
    in
      TextIO.closeIn stream;
      text
    end

  fun quickSort _ [] = []
    | quickSort cmp (pivot :: rest) =
        let
          fun partition ([], less, greater) = (less, greater)
            | partition (x :: xs, less, greater) =
                case cmp (x, pivot) of
                  LESS => partition (xs, x :: less, greater)
                | _ => partition (xs, less, x :: greater)

          val (less, greater) = partition (rest, [], [])
        in
          quickSort cmp less @ [pivot] @ quickSort cmp greater
        end
end

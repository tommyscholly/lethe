structure Util =
struct
  fun quickSort cmp [] = []
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

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

  fun split_lines text =
    String.tokens (fn c => c = #"\n") text
  fun split_words text = String.tokens Char.isSpace text

  fun take_until (str: string) (ls: string list) =
    let
      fun take_until' (str: string) (ls: string list) (acc: string list) =
        case ls of
          [] => (acc, [])
        | x :: xs => if x = str then (acc, x :: xs) else take_until' str xs (acc @ [x])
    in
      take_until' str ls []
    end

  fun idx_of (tok: string) (text: string) : int option =
    let
      val tokens = split_words text
      fun idx_of' (tok: string) (tokens: string list) (idx: int) =
        case tokens of
          [] => NONE
        | x :: xs => if x = tok then SOME idx else idx_of' tok xs (idx + 1)
    in
      idx_of' tok tokens 0
    end

  fun quick_sort _ [] = []
    | quick_sort cmp (pivot :: rest) =
        let
          fun partition ([], less, greater) = (less, greater)
            | partition (x :: xs, less, greater) =
                case cmp (x, pivot) of
                  LESS => partition (xs, x :: less, greater)
                | _ => partition (xs, less, x :: greater)

          val (less, greater) = partition (rest, [], [])
        in
          quick_sort cmp less @ [pivot] @ quick_sort cmp greater
        end
end

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
end

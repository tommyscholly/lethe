structure Colors :> COLORS =
struct
  (* ANSI escape sequences for colors *)
  val red = "\027[31m"
  val green = "\027[32m"
  val yellow = "\027[33m"
  val blue = "\027[34m"
  val magenta = "\027[35m"
  val cyan = "\027[36m"
  val white = "\027[37m"
  val black = "\027[30m"
  val reset = "\027[0m"

  val colors = [red, green, yellow, blue, magenta, cyan, white, black]

  val current_pos = ref 0

  fun next_color () =
    let
      val color = List.nth (colors, !current_pos)
      val _ = current_pos := (!current_pos + 1) mod (List.length colors)
    in
      color
    end

  fun reset_cycle () = current_pos := 0

  fun with_color n text =
    List.nth (colors, n mod (List.length colors)) ^ text ^ reset

  fun with_this_color color text = color ^ text ^ reset

  fun with_next_color text =
    next_color () ^ text ^ reset
end

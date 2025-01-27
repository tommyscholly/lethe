(* Taken from https://github.com/zesterer/ariadne/blob/main/src/draw.rs#L56 *)
structure Chars =
struct
  datatype Char =
    Hbar
  | Vbar
  | Xbar
  | VbarBreak
  | VbarGap
  | UArrow
  | RArrow
  | LTop
  | MTop
  | RTop
  | LBot
  | MBot
  | RBot
  | LBox
  | RBox
  | LCross
  | RCross
  | Underbar
  | Underline

  fun ascii Hbar = "-"
    | ascii Vbar = "|"
    | ascii Xbar = "+"
    | ascii VbarBreak = "*"
    | ascii VbarGap = ":"
    | ascii UArrow = "^"
    | ascii RArrow = ">"
    | ascii LTop = ","
    | ascii MTop = "v"
    | ascii RTop = "."
    | ascii LBot = "`"
    | ascii MBot = "^"
    | ascii RBot = "'"
    | ascii LBox = "["
    | ascii RBox = "]"
    | ascii LCross = "|"
    | ascii RCross = "|"
    | ascii Underbar = "|"
    | ascii Underline = "^"

  fun unicode Hbar = "─"
    | unicode Vbar = "│"
    | unicode Xbar = "┼"
    | unicode VbarBreak = "┆"
    | unicode VbarGap = "┆"
    | unicode UArrow = "▲"
    | unicode RArrow = "▶"
    | unicode LTop = "╭"
    | unicode MTop = "┬"
    | unicode RTop = "╮"
    | unicode LBot = "╰"
    | unicode MBot = "┴"
    | unicode RBot = "╯"
    | unicode LBox = "["
    | unicode RBox = "]"
    | unicode LCross = "├"
    | unicode RCross = "┤"
    | unicode Underbar = "┬"
    | unicode Underline = "─"
end

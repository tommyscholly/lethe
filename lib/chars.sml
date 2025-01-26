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

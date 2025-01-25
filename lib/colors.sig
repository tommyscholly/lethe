signature COLORS =
sig
  val next_color: unit -> string
  val with_color: int -> string -> string
  val with_next_color: string -> string
  val with_this_color: string -> string -> string
  val reset_cycle: unit -> unit

  val red: string
  val yellow: string
  val green: string
  val reset: string
end

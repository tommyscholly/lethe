signature LETHE = sig 
  (* val output_error : string -> int -> int -> string -> unit *)
  val monitor : unit -> unit
  val line_no_to_char_idx : string -> int -> int
end

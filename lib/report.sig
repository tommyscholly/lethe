signature REPORT = sig
    type t
    datatype LabelKind = NORMAL | ERROR | WARNING | INFO

    val make_report : string -> t
    val process_report : t -> unit
    val set_selection : t -> int -> int -> string -> t
    val add_label : t -> LabelKind -> string -> int -> int -> t
end

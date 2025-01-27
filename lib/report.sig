signature REPORT = sig
    type t
    datatype LabelKind = NORMAL | ERROR | WARNING | INFO
    type Position = int * int

    val make_report : string -> t
    val output_report : t -> unit
    val set_selection : t -> int -> int -> string -> t
    val add_label : t -> LabelKind -> string -> int -> int -> t
end

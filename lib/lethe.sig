signature LETHE = sig 
  type label
  type block

  datatype LabelKind = NORMAL | ERROR | WARNING | INFO

  (* used to create an individual label, only spanning one line *)
  (* label_msg -> range_start -> range_end -> label_kind *)
  val create_label : string -> int -> int -> LabelKind -> label
  (* used to create a block, which may include labels inside of it *)
  (* file_name -> range_start -> range_end -> block_error_msg *)
  val create_block: string -> int -> int -> string -> block

  val add_label : block -> label -> block

  (* displays a label on the screen, within the specific file *)
  val report_label : string -> label -> unit
  (* displays a block on the screen *)
  val report_block : block -> unit
  (* val report_error : string -> labellist -> string -> int option -> unit *)

  val read_to_string : string -> string
end

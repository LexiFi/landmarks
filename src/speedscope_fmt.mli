(* Auto-generated from "speedscope_fmt.atd" by atdml. *)

type value_unit =
  | Bytes
  | Microseconds
  | Milliseconds
  | Nanoseconds
  | None_
  | Seconds

val value_unit_of_yojson : Yojson.Safe.t -> value_unit
val yojson_of_value_unit : value_unit -> Yojson.Safe.t
val value_unit_of_json : string -> value_unit
val json_of_value_unit : value_unit -> string

module Value_unit : sig
  type nonrec t = value_unit
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end

type sampled_profile = {
  type_: string;
  name: string;
  unit: value_unit;
  start_value: float;
  end_value: float;
  samples: int list list;
  weights: float list;
}

val create_sampled_profile : type_:string -> name:string -> unit:value_unit -> start_value:float -> end_value:float -> samples:int list list -> weights:float list -> unit -> sampled_profile
val sampled_profile_of_yojson : Yojson.Safe.t -> sampled_profile
val yojson_of_sampled_profile : sampled_profile -> Yojson.Safe.t
val sampled_profile_of_json : string -> sampled_profile
val json_of_sampled_profile : sampled_profile -> string

module Sampled_profile : sig
  type nonrec t = sampled_profile
  val create : type_:string -> name:string -> unit:value_unit -> start_value:float -> end_value:float -> samples:int list list -> weights:float list -> unit -> t
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end

type frame = {
  name: string;
  file: string option;
  line: int option;
  col: int option;
}

val create_frame : name:string -> ?file:string -> ?line:int -> ?col:int -> unit -> frame
val frame_of_yojson : Yojson.Safe.t -> frame
val yojson_of_frame : frame -> Yojson.Safe.t
val frame_of_json : string -> frame
val json_of_frame : frame -> string

module Frame : sig
  type nonrec t = frame
  val create : name:string -> ?file:string -> ?line:int -> ?col:int -> unit -> t
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end

type profile_shared = {
  frames: frame list;
}

val create_profile_shared : frames:frame list -> unit -> profile_shared
val profile_shared_of_yojson : Yojson.Safe.t -> profile_shared
val yojson_of_profile_shared : profile_shared -> Yojson.Safe.t
val profile_shared_of_json : string -> profile_shared
val json_of_profile_shared : profile_shared -> string

module Profile_shared : sig
  type nonrec t = profile_shared
  val create : frames:frame list -> unit -> t
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end

type file_format = {
  schema: string;
  name: string option;
  exporter: string option;
  active_profile_index: int option;
  profiles: sampled_profile list;
  shared: profile_shared;
}

val create_file_format : schema:string -> ?name:string -> ?exporter:string -> ?active_profile_index:int -> profiles:sampled_profile list -> shared:profile_shared -> unit -> file_format
val file_format_of_yojson : Yojson.Safe.t -> file_format
val yojson_of_file_format : file_format -> Yojson.Safe.t
val file_format_of_json : string -> file_format
val json_of_file_format : file_format -> string

module File_format : sig
  type nonrec t = file_format
  val create : schema:string -> ?name:string -> ?exporter:string -> ?active_profile_index:int -> profiles:sampled_profile list -> shared:profile_shared -> unit -> t
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end


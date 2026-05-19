(* Generated from speedscope_fmt.atd — do not edit by hand.
   To regenerate: atdml speedscope_fmt.atd *)

type value_unit =
  | Bytes
  | Microseconds
  | Milliseconds
  | Nanoseconds
  | None_
  | Seconds

type frame = {
  name : string;
  file : string option;
  line : int option;
  col  : int option;
}

type sampled_profile = {
  type_       : string;
  name        : string;
  unit_       : value_unit;
  start_value : float;
  end_value   : float;
  samples     : int list list;
  weights     : float list;
}

type shared = {
  frames : frame list;
}

type file_format = {
  dollar_schema        : string;
  name                 : string option;
  exporter             : string option;
  active_profile_index : int option;
  profiles             : sampled_profile list;
  shared               : shared;
}

val yojson_of_value_unit     : value_unit     -> Yojson.Safe.t
val yojson_of_frame          : frame          -> Yojson.Safe.t
val yojson_of_sampled_profile : sampled_profile -> Yojson.Safe.t
val yojson_of_shared         : shared         -> Yojson.Safe.t
val yojson_of_file_format    : file_format    -> Yojson.Safe.t

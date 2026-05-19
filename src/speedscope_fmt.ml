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

let yojson_of_value_unit = function
  | Bytes        -> `String "bytes"
  | Microseconds -> `String "microseconds"
  | Milliseconds -> `String "milliseconds"
  | Nanoseconds  -> `String "nanoseconds"
  | None_        -> `String "none"
  | Seconds      -> `String "seconds"

let yojson_of_frame {name; file; line; col} =
  let fields = ref [("name", `String name)] in
  (match file with Some v -> fields := ("file", `String v) :: !fields | None -> ());
  (match line with Some v -> fields := ("line", `Int v)    :: !fields | None -> ());
  (match col  with Some v -> fields := ("col",  `Int v)    :: !fields | None -> ());
  `Assoc (List.rev !fields)

let yojson_of_sampled_profile
    {type_; name; unit_; start_value; end_value; samples; weights} =
  `Assoc [
    "type",       `String type_;
    "name",       `String name;
    "unit",       yojson_of_value_unit unit_;
    "startValue", `Float start_value;
    "endValue",   `Float end_value;
    "samples",    `List (List.map (fun s -> `List (List.map (fun i -> `Int i) s)) samples);
    "weights",    `List (List.map (fun w -> `Float w) weights);
  ]

let yojson_of_shared {frames} =
  `Assoc ["frames", `List (List.map yojson_of_frame frames)]

let yojson_of_file_format
    {dollar_schema; name; exporter; active_profile_index; profiles; shared} =
  let opt k f = function Some v -> [k, f v] | None -> [] in
  `Assoc (
    ["$schema",  `String dollar_schema]
    @ opt "name"               (fun s -> `String s) name
    @ opt "exporter"           (fun s -> `String s) exporter
    @ opt "activeProfileIndex" (fun i -> `Int i)    active_profile_index
    @ ["profiles", `List (List.map yojson_of_sampled_profile profiles);
       "shared",   yojson_of_shared shared]
  )

(* Auto-generated from "speedscope_fmt.atd" by atdml. *)
[@@@ocaml.warning "-27-32-33-35-39"]

(* Inlined runtime — no external dependency needed. *)
module Atdml_runtime = struct
  (* Returns true iff the list has strictly more than [n] elements,
     without traversing past element n+1. *)
  let rec list_length_gt n = function
    | _ :: rest -> if n = 0 then true else list_length_gt (n - 1) rest
    | [] -> false

  module Yojson = struct
    let bad_type expected_type x =
      Printf.ksprintf failwith "expected %s, got: %s"
        expected_type (Yojson.Safe.to_string x)

    let bad_sum type_name x =
      Printf.ksprintf failwith "invalid variant for type '%s': %s"
        type_name (Yojson.Safe.to_string x)

    let missing_field type_name field_name =
      Printf.ksprintf failwith "missing field '%s' in object of type '%s'"
        field_name type_name

    let bool_of_yojson = function
      | `Bool b -> b
      | x -> bad_type "bool" x

    let yojson_of_bool b = `Bool b

    let int_of_yojson = function
      | `Int n -> n
      | x -> bad_type "int" x

    let yojson_of_int n = `Int n

    let float_of_yojson = function
      | `Float f -> f
      | `Int n -> Float.of_int n
      | x -> bad_type "float" x

    let yojson_of_float f = `Float f

    let string_of_yojson = function
      | `String s -> s
      | x -> bad_type "string" x

    let yojson_of_string s = `String s

    let unit_of_yojson = function
      | `Null -> ()
      | x -> bad_type "null" x

    let yojson_of_unit () = `Null

    let list_of_yojson f = function
      | `List xs -> List.map f xs
      | x -> bad_type "array" x

    let yojson_of_list f xs = `List (List.map f xs)

    let option_of_yojson f = function
      | `String "None" -> None
      | `List [`String "Some"; x] -> Some (f x)
      | x -> bad_type "option" x

    let yojson_of_option f = function
      | None -> `String "None"
      | Some x -> `List [`String "Some"; f x]

    let nullable_of_yojson f = function
      | `Null -> None
      | x -> Some (f x)

    let yojson_of_nullable f = function
      | None -> `Null
      | Some x -> f x

    let assoc_of_yojson f = function
      | `Assoc pairs -> List.map (fun (k, v) -> (k, f v)) pairs
      | x -> bad_type "object" x

    let yojson_of_assoc f xs =
      `Assoc (List.map (fun (k, v) -> (k, f v)) xs)
  end
end

(** Unit in which all profile values are expressed. *)
type value_unit =
  | Bytes
  | Microseconds
  | Milliseconds
  | Nanoseconds
  | None_
  | Seconds

let value_unit_of_yojson (x : Yojson.Safe.t) : value_unit =
  match x with
  | `String "bytes" -> Bytes
  | `String "microseconds" -> Microseconds
  | `String "milliseconds" -> Milliseconds
  | `String "nanoseconds" -> Nanoseconds
  | `String "none" -> None_
  | `String "seconds" -> Seconds
  | _ -> Atdml_runtime.Yojson.bad_sum "value_unit" x

let yojson_of_value_unit (x : value_unit) : Yojson.Safe.t =
  match x with
  | Bytes -> `String "bytes"
  | Microseconds -> `String "microseconds"
  | Milliseconds -> `String "milliseconds"
  | Nanoseconds -> `String "nanoseconds"
  | None_ -> `String "none"
  | Seconds -> `String "seconds"

let value_unit_of_json s =
  value_unit_of_yojson (Yojson.Safe.from_string s)

let json_of_value_unit x =
  Yojson.Safe.to_string (yojson_of_value_unit x)

module Value_unit = struct
  type nonrec t = value_unit
  let of_yojson = value_unit_of_yojson
  let to_yojson = yojson_of_value_unit
  let of_json = value_unit_of_json
  let to_json = json_of_value_unit
end

type sampled_profile = {
  type_: string;  (** Profile type discriminator; always 'sampled'. *)
  name: string;  (** Name of the profile. Typically a filename. *)
  unit: value_unit;  (** Unit in which all values in this profile are expressed. *)
  start_value: float;  (** The starting value. All values are relative to this. *)
  end_value: float;  (** The final value. Must be >= startValue. *)
  samples: int list list;  (** List of stacks; each is a list of frame indices. *)
  weights: float list;  (** Weight of each sample. Same length as samples. *)
}

let create_sampled_profile ~type_ ~name ~unit ~start_value ~end_value ~samples ~weights () : sampled_profile =
  { type_; name; unit; start_value; end_value; samples; weights }

let sampled_profile_of_yojson (x : Yojson.Safe.t) : sampled_profile =
  match x with
  | `Assoc fields ->
    (* Duplicate JSON keys: behavior is unspecified (RFC 8259 §4 says keys SHOULD
       be unique). Below the threshold, List.assoc_opt returns the first binding;
       above it, the hashtable returns the last. *)
    let assoc_ =
      if Atdml_runtime.list_length_gt 5 fields then
        let tbl = Hashtbl.create 16 in
        List.iter (fun (k, v) -> Hashtbl.add tbl k v) fields;
        (fun key -> Hashtbl.find_opt tbl key)
      else (fun key -> List.assoc_opt key fields)
    in
    let type_ =
      match assoc_ "type" with
      | Some v -> Atdml_runtime.Yojson.string_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "type"
    in
    let name =
      match assoc_ "name" with
      | Some v -> Atdml_runtime.Yojson.string_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "name"
    in
    let unit =
      match assoc_ "unit" with
      | Some v -> value_unit_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "unit"
    in
    let start_value =
      match assoc_ "startValue" with
      | Some v -> Atdml_runtime.Yojson.float_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "startValue"
    in
    let end_value =
      match assoc_ "endValue" with
      | Some v -> Atdml_runtime.Yojson.float_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "endValue"
    in
    let samples =
      match assoc_ "samples" with
      | Some v -> (Atdml_runtime.Yojson.list_of_yojson (Atdml_runtime.Yojson.list_of_yojson Atdml_runtime.Yojson.int_of_yojson)) v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "samples"
    in
    let weights =
      match assoc_ "weights" with
      | Some v -> (Atdml_runtime.Yojson.list_of_yojson Atdml_runtime.Yojson.float_of_yojson) v
      | None -> Atdml_runtime.Yojson.missing_field "sampled_profile" "weights"
    in
    { type_; name; unit; start_value; end_value; samples; weights }
  | _ -> Atdml_runtime.Yojson.bad_type "sampled_profile" x

let yojson_of_sampled_profile (x : sampled_profile) : Yojson.Safe.t =
  `Assoc (List.concat [
    [("type", Atdml_runtime.Yojson.yojson_of_string x.type_)];
    [("name", Atdml_runtime.Yojson.yojson_of_string x.name)];
    [("unit", yojson_of_value_unit x.unit)];
    [("startValue", Atdml_runtime.Yojson.yojson_of_float x.start_value)];
    [("endValue", Atdml_runtime.Yojson.yojson_of_float x.end_value)];
    [("samples", (Atdml_runtime.Yojson.yojson_of_list (Atdml_runtime.Yojson.yojson_of_list Atdml_runtime.Yojson.yojson_of_int)) x.samples)];
    [("weights", (Atdml_runtime.Yojson.yojson_of_list Atdml_runtime.Yojson.yojson_of_float) x.weights)];
  ])

let sampled_profile_of_json s =
  sampled_profile_of_yojson (Yojson.Safe.from_string s)

let json_of_sampled_profile x =
  Yojson.Safe.to_string (yojson_of_sampled_profile x)

module Sampled_profile = struct
  type nonrec t = sampled_profile
  let create = create_sampled_profile
  let of_yojson = sampled_profile_of_yojson
  let to_yojson = yojson_of_sampled_profile
  let of_json = sampled_profile_of_json
  let to_json = json_of_sampled_profile
end

type frame = {
  name: string;
  file: string option;
  line: int option;
  col: int option;
}

let create_frame ~name ?file ?line ?col () : frame =
  { name; file; line; col }

let frame_of_yojson (x : Yojson.Safe.t) : frame =
  match x with
  | `Assoc fields ->
    (* Duplicate JSON keys: behavior is unspecified (RFC 8259 §4 says keys SHOULD
       be unique). Below the threshold, List.assoc_opt returns the first binding;
       above it, the hashtable returns the last. *)
    let assoc_ =
      if Atdml_runtime.list_length_gt 5 fields then
        let tbl = Hashtbl.create 16 in
        List.iter (fun (k, v) -> Hashtbl.add tbl k v) fields;
        (fun key -> Hashtbl.find_opt tbl key)
      else (fun key -> List.assoc_opt key fields)
    in
    let name =
      match assoc_ "name" with
      | Some v -> Atdml_runtime.Yojson.string_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "frame" "name"
    in
    let file =
      match assoc_ "file" with
      | None | Some `Null -> None
      | Some v -> Some (Atdml_runtime.Yojson.string_of_yojson v)
    in
    let line =
      match assoc_ "line" with
      | None | Some `Null -> None
      | Some v -> Some (Atdml_runtime.Yojson.int_of_yojson v)
    in
    let col =
      match assoc_ "col" with
      | None | Some `Null -> None
      | Some v -> Some (Atdml_runtime.Yojson.int_of_yojson v)
    in
    { name; file; line; col }
  | _ -> Atdml_runtime.Yojson.bad_type "frame" x

let yojson_of_frame (x : frame) : Yojson.Safe.t =
  `Assoc (List.concat [
    [("name", Atdml_runtime.Yojson.yojson_of_string x.name)];
    (match x.file with None -> [] | Some v -> [("file", Atdml_runtime.Yojson.yojson_of_string v)]);
    (match x.line with None -> [] | Some v -> [("line", Atdml_runtime.Yojson.yojson_of_int v)]);
    (match x.col with None -> [] | Some v -> [("col", Atdml_runtime.Yojson.yojson_of_int v)]);
  ])

let frame_of_json s =
  frame_of_yojson (Yojson.Safe.from_string s)

let json_of_frame x =
  Yojson.Safe.to_string (yojson_of_frame x)

module Frame = struct
  type nonrec t = frame
  let create = create_frame
  let of_yojson = frame_of_yojson
  let to_yojson = yojson_of_frame
  let of_json = frame_of_json
  let to_json = json_of_frame
end

(** Data shared between profiles. *)
type profile_shared = {
  frames: frame list;
}

let create_profile_shared ~frames () : profile_shared =
  { frames }

let profile_shared_of_yojson (x : Yojson.Safe.t) : profile_shared =
  match x with
  | `Assoc fields ->
    (* Duplicate JSON keys: behavior is unspecified (RFC 8259 §4 says keys SHOULD
       be unique). Below the threshold, List.assoc_opt returns the first binding;
       above it, the hashtable returns the last. *)
    let assoc_ =
      if Atdml_runtime.list_length_gt 5 fields then
        let tbl = Hashtbl.create 16 in
        List.iter (fun (k, v) -> Hashtbl.add tbl k v) fields;
        (fun key -> Hashtbl.find_opt tbl key)
      else (fun key -> List.assoc_opt key fields)
    in
    let frames =
      match assoc_ "frames" with
      | Some v -> (Atdml_runtime.Yojson.list_of_yojson frame_of_yojson) v
      | None -> Atdml_runtime.Yojson.missing_field "profile_shared" "frames"
    in
    { frames }
  | _ -> Atdml_runtime.Yojson.bad_type "profile_shared" x

let yojson_of_profile_shared (x : profile_shared) : Yojson.Safe.t =
  `Assoc (List.concat [
    [("frames", (Atdml_runtime.Yojson.yojson_of_list yojson_of_frame) x.frames)];
  ])

let profile_shared_of_json s =
  profile_shared_of_yojson (Yojson.Safe.from_string s)

let json_of_profile_shared x =
  Yojson.Safe.to_string (yojson_of_profile_shared x)

module Profile_shared = struct
  type nonrec t = profile_shared
  let create = create_profile_shared
  let of_yojson = profile_shared_of_yojson
  let to_yojson = yojson_of_profile_shared
  let of_json = profile_shared_of_json
  let to_json = json_of_profile_shared
end

type file_format = {
  schema: string;
  name: string option;  (** Profile group name. Defaults to the filename if omitted. *)
  exporter: string option;  (** Exporter name. Not consumed. Format: name\@version. *)
  active_profile_index: int option;  (** Index into profiles to display on load. Defaults to 0. *)
  profiles: sampled_profile list;  (** List of profile definitions. *)
  shared: profile_shared;  (** Data shared between profiles. *)
}

let create_file_format ~schema ?name ?exporter ?active_profile_index ~profiles ~shared () : file_format =
  { schema; name; exporter; active_profile_index; profiles; shared }

let file_format_of_yojson (x : Yojson.Safe.t) : file_format =
  match x with
  | `Assoc fields ->
    (* Duplicate JSON keys: behavior is unspecified (RFC 8259 §4 says keys SHOULD
       be unique). Below the threshold, List.assoc_opt returns the first binding;
       above it, the hashtable returns the last. *)
    let assoc_ =
      if Atdml_runtime.list_length_gt 5 fields then
        let tbl = Hashtbl.create 16 in
        List.iter (fun (k, v) -> Hashtbl.add tbl k v) fields;
        (fun key -> Hashtbl.find_opt tbl key)
      else (fun key -> List.assoc_opt key fields)
    in
    let schema =
      match assoc_ "$schema" with
      | Some v -> Atdml_runtime.Yojson.string_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "file_format" "$schema"
    in
    let name =
      match assoc_ "name" with
      | None | Some `Null -> None
      | Some v -> Some (Atdml_runtime.Yojson.string_of_yojson v)
    in
    let exporter =
      match assoc_ "exporter" with
      | None | Some `Null -> None
      | Some v -> Some (Atdml_runtime.Yojson.string_of_yojson v)
    in
    let active_profile_index =
      match assoc_ "activeProfileIndex" with
      | None | Some `Null -> None
      | Some v -> Some (Atdml_runtime.Yojson.int_of_yojson v)
    in
    let profiles =
      match assoc_ "profiles" with
      | Some v -> (Atdml_runtime.Yojson.list_of_yojson sampled_profile_of_yojson) v
      | None -> Atdml_runtime.Yojson.missing_field "file_format" "profiles"
    in
    let shared =
      match assoc_ "shared" with
      | Some v -> profile_shared_of_yojson v
      | None -> Atdml_runtime.Yojson.missing_field "file_format" "shared"
    in
    { schema; name; exporter; active_profile_index; profiles; shared }
  | _ -> Atdml_runtime.Yojson.bad_type "file_format" x

let yojson_of_file_format (x : file_format) : Yojson.Safe.t =
  `Assoc (List.concat [
    [("$schema", Atdml_runtime.Yojson.yojson_of_string x.schema)];
    (match x.name with None -> [] | Some v -> [("name", Atdml_runtime.Yojson.yojson_of_string v)]);
    (match x.exporter with None -> [] | Some v -> [("exporter", Atdml_runtime.Yojson.yojson_of_string v)]);
    (match x.active_profile_index with None -> [] | Some v -> [("activeProfileIndex", Atdml_runtime.Yojson.yojson_of_int v)]);
    [("profiles", (Atdml_runtime.Yojson.yojson_of_list yojson_of_sampled_profile) x.profiles)];
    [("shared", yojson_of_profile_shared x.shared)];
  ])

let file_format_of_json s =
  file_format_of_yojson (Yojson.Safe.from_string s)

let json_of_file_format x =
  Yojson.Safe.to_string (yojson_of_file_format x)

module File_format = struct
  type nonrec t = file_format
  let create = create_file_format
  let of_yojson = file_format_of_yojson
  let to_yojson = yojson_of_file_format
  let of_json = file_format_of_json
  let to_json = json_of_file_format
end


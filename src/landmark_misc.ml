(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

let group_proj f l =
  let tbl = Hashtbl.create (List.length l) in
  List.iter (fun x -> let key = f x in
  match Hashtbl.find tbl key with
  | exception Not_found -> Hashtbl.replace tbl key [x]
  | l -> Hashtbl.replace tbl key (x::l)) l;
  Hashtbl.fold (fun _ value acc -> value :: acc) tbl []

let flatten_map f l =
  List.flatten (List.map f l)

let base_name s =
  try
    let k = String.rindex s '.' in
    String.sub s 0 k
  with Not_found -> s

module IntSet =
  Set.Make(struct type t = int let compare = compare end)

module StringSet =
  Set.Make(struct type t = string let compare = compare end)

let group_by ?(equals = (=)) l =
  let rec aux cur stk acc = function
    | [] -> List.rev (stk :: acc)
    | hd::tl when equals cur hd ->
      aux cur (hd :: stk) acc tl
    | hd::tl ->
      aux hd [hd] ((List.rev stk) :: acc) tl
  in
  match l with
  | [] -> []
  | hd :: tl -> aux hd [hd] [] tl

let rec choose f = function
  | [] -> []
  | hd :: tl ->
    match f hd with
    | Some x -> x :: (choose f tl)
    | None -> choose f tl

let duplicated_elements proj l =
  List.sort (fun x y -> compare (proj x) (proj y)) l
  |> group_by ~equals:(fun x y -> proj x = proj y)
  |> choose (function x :: _ :: _ -> Some x | _ -> None)

let duplicated_elements ?proj l =
  match proj with
  | Some proj -> duplicated_elements proj l
  | None -> duplicated_elements (fun x -> x) l

let split c s =
  let open String in
  let res = ref [] in
  let pos = ref 0 in
  let len = length s in
  while
    match index_from s !pos c with
    | exception Not_found ->
      res := sub s !pos (len - !pos) :: !res;
      false
    | k ->
      res := sub s !pos (k - !pos) :: !res;
      pos := k + 1;
      !pos < len || (res := "" :: !res; false)
  do () done;
  List.rev !res

let starts_with ~prefix x =
  let n = String.length prefix in
  String.length x >= n && prefix = (String.sub x 0 n)


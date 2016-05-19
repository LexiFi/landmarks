val command_line_options : (string * Arg.spec * string) list
type addressing_mode =
    Ibased of string * int
  | Iindexed of int
  | Iindexed2 of int
  | Iscaled of int * int
  | Iindexed2scaled of int * int
type specific_operation =
    Ilea of addressing_mode
  | Istore_int of nativeint * addressing_mode * bool
  | Istore_symbol of string * addressing_mode * bool
  | Ioffset_loc of int * addressing_mode
  | Ifloatarithmem of float_operation * addressing_mode
  | Ibswap of int
  | Isqrtf
  | Ifloatsqrtf of addressing_mode
and float_operation = Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv
val big_endian : bool
val size_addr : int
val size_int : int
val size_float : int
val allow_unaligned_access : bool
val division_crashes_on_overflow : bool
val identity_addressing : addressing_mode
val offset_addressing : addressing_mode -> int -> addressing_mode
val num_args_addressing : addressing_mode -> int
val print_addressing :
  (Format.formatter -> 'a -> unit) ->
  addressing_mode -> Format.formatter -> 'a array -> unit
val print_specific_operation :
  (Format.formatter -> 'a -> unit) ->
  specific_operation -> Format.formatter -> 'a array -> unit

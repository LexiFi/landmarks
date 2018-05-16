include Landmark

let current_id = ref Thread.(id (self ()))

let check () =
 !current_id = Thread.(id (self ()))

let error operation =
  failwith (Printf.sprintf "%s: this operation can only be called by the thread that started the profiling" operation)

let increment ?times counter =
  if check () then
    increment ?times counter

let sample sampler value =
  if check () then
    sample sampler value

let enter lm =
  if check () then
    enter lm

let exit lm =
  if check () then
    exit lm

let wrap lm f x =
  if check () then
    wrap lm f x
  else f x

let unsafe_wrap lm f x =
  if check () then
    wrap lm f x
  else
    f x

let export ?label () =
  if check () then
    export ?label ()
  else
    error "export"

let reset () =
  if check () then
    reset ()
  else
    error "reset"

let export_and_reset ?label () =
  if check () then
    export_and_reset ?label ()
  else
    error "export_and_reset"

let start_profiling ?profiling_options () =
  current_id := Thread.(id (self ()));
  start_profiling ?profiling_options ()

let stop_profiling () =
  stop_profiling ()

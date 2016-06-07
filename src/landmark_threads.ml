let current_id = ref Thread.(id (self ()))

let check () =
 !current_id = Thread.(id (self ()))

let error operation =
  failwith (Printf.sprintf "%s: this operation can only be called by the thread that started the profiling" operation)

let increment ?times counter =
  if check () then
    Landmark.increment ?times counter

let sample sampler value =
  if check () then
    Landmark.sample sampler value

let enter lm =
  if check () then
    Landmark.enter lm

let exit lm =
  if check () then
    Landmark.exit lm

let wrap lm f x =
  if check () then
    Landmark.wrap lm f x
  else f x

let unsafe_wrap lm f x =
  if check () then
    Landmark.wrap lm f x
  else
    f x

let export () =
  if check () then
    Landmark.export ()
  else
    error "export"

let reset () =
  if check () then
    Landmark.reset ()
  else
    error "reset"

let export_and_reset () =
  if check () then
    Landmark.export_and_reset ()
  else
    error "export_and_reset"

let start_profiling ?profiling_options () =
  current_id := Thread.(id (self ()));
  Landmark.start_profiling ?profiling_options ()

let stop_profiling () =
  Landmark.stop_profiling ()

//Provides: caml_highres_clock const
//Requires: caml_int64_of_float
function caml_highres_clock() {
  // WARNING: this method's accuracy varies wildly from browser to browser due
  // to Spectre mitigations.
  // As of writing, it has:
  // - 1ms precision on Firefox [1]
  // - 5us to 100us precision on Chromium [2]
  //
  // [1]: https://developer.mozilla.org/en-US/docs/Web/API/Performance/now#reduced_time_precision
  // [2]: https://chromestatus.com/feature/6497206758539264
  //
  // We could probably get vastly better accuracy under Electron by using a Node.js runtime.
  return caml_int64_of_float(globalThis.performance.now());
}

//Provides: allocated_bytes const
//Requires: caml_int64_of_float
function allocated_bytes() {
  return caml_int64_of_float(0);
}

//Provides: allocated_bytes_major const
//Requires: caml_int64_of_float
function allocated_bytes_major() {
  return caml_int64_of_float(0);
}

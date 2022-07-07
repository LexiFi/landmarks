//Provides: caml_highres_clock const
//Requires: MlInt64

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
  var rawClockMs = globalThis.performance.now();

  // Integer representation.
  var us = Math.trunc(rawClockMs * 1000);

  // First 24 bits.
  var low = us & 0xffffff;
  // Mid 24 bits.
  var mid = Math.trunc(us / Math.pow(2, 24)) & 0xffffff;
  // Upper 16 bits.
  var hi = Math.trunc(us / Math.pow(2, 48)) & 0xffff;

  // Using MlInt64 is necessary for the return value to interact correctly with
  // OCaml code.
  return new MlInt64(low, mid, hi);
}

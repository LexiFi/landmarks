//Provides: caml_sys_const_backend_type const
function caml_sys_const_backend_type(arity, f) {
  return function () {
     return 1;
  };
}

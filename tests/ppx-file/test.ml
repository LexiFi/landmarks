let[@landmark "named"] f x = x + 1

let g y = f y

module M = struct
  let[@landmark] h z = z * 2
end

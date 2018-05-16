let () =
  if Array.length Sys.argv < 3 then begin
    let name = Filename.basename Sys.executable_name in
    Printf.eprintf "usage: %s <input files> <path to executable> [extra args ...]\n%!" name;
    exit 1
  end

let input = Sys.argv.(1)
let executable = Sys.argv.(2)
let extra_args = Array.sub Sys.argv 2 (Array.length Sys.argv - 2)

let () =
  let input_fd = Unix.openfile input [O_RDONLY] 0 in
  begin
   let pid = Unix.create_process executable extra_args input_fd Unix.stdout Unix.stderr in
   let _, _ = Unix.waitpid [] pid in
   Unix.close input_fd;
   exit 0
  end


type outfiles = {
  objfile : string;
  cmxfile : string;
  cmifile : string option
}
val delete_temps : outfiles -> unit 

val compile_module :
  filename:string ->
  Malfunction.moduleexp ->
  outfiles

val link_executable : string -> outfiles -> int

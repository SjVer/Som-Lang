open Clap

let () =
  description "Official Som compiler";

  (* named args *)
  let verbose = flag ~set_long: "verbose" ~set_short: 'v' ~description: "Produce verbose output"  false in
  let mute    = flag ~set_long: "mute"    ~set_short: 'm' ~description: "Mute all warnings"       false in
  
  (* unnamed args *)
  (* let files = list_string ~placeholder: "FILES" ~description: "Files to compile" () in *)
  let file = mandatory_string ~placeholder: "FILE" ~description: "File to compile" () in
  
  close();
  exit 0
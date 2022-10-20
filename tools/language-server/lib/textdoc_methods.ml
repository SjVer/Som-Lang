open Lsp
open Lsp.Types
open Util
open T

let hover server uri (pos: Types.Position.t) =
  let pos' = incr_pos pos in
  let path = Uri.to_path uri in
  let tast = Somc.Pipeline.TypecheckFile.call path in
  let node = Find_node.find_tast_node tast pos' in
  ()
  ;
  Error (internal_error_f "nyi")
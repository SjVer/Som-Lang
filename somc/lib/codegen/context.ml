module SMap = Map.Make(String)

type ctx =
  {
    context: Llvm.llcontext;
    llmodule: Llvm.llmodule;
    builder: Llvm.llbuilder;
    values: Llvm.llvalue SMap.t;
  }

let make name =
  let context = Llvm.create_context () in
  {
    context;
    llmodule = Llvm.create_module context name;
    builder = Llvm.builder context;
    values = SMap.empty;
  }
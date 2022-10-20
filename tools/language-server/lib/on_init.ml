open Lsp.Types

let init_info =
  let t = `Bool true in

  let capabilities =
    ServerCapabilities.create
      ~completionProvider:(
        CompletionOptions.create
          ~triggerCharacters:["."]
          ()
        )
      ~hoverProvider:t
      (* ~declarationProvider:t *)
      (* ~definitionProvider:t *)
      (* ~typeDefinitionProvider:t *)
      (* ~codeLensProvider: *)
      ()
  in
  InitializeResult.create
    ~serverInfo:{ name = "som-lsp"; version = Some Somc.Config.Cli.version }
    ~capabilities ()

let handle () = Ok init_info
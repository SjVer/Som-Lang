open Lsp.Types

let init_info =
  let t = `Bool true in

  let capabilities =
    ServerCapabilities.create
      (* ~completionProvider:(
        CompletionOptions.create
          ~triggerCharacters:["."]
          ()
        ) *)
      ~hoverProvider:t
      (* ~declarationProvider:t *)
      (* ~definitionProvider:t *)
      (* ~typeDefinitionProvider:t *)
      (* ~codeLensProvider: *)

      ~semanticTokensProvider:(`SemanticTokensOptions
        (SemanticTokensOptions.create
          ~legend:Semantics.legend
          ~range:false
          ~full:t
          ()))

      ~textDocumentSync:(`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create
            ~change:Full
            ~openClose:true
            ~willSave:false
            ~willSaveWaitUntil:false
            ()))

      ()
  in
  InitializeResult.create
    ~serverInfo:{ name = "som-lsp"; version = Some Somc.Config.Cli.version }
    ~capabilities ()

let handle () = Ok init_info
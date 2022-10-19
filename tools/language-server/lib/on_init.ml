open Lsp.Types

let init_info =
  let capabilities =
    ServerCapabilities.create

      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~willSave:false
             ~willSaveWaitUntil:false ()))

      ~workspace:
        (ServerCapabilities.create_workspace
          ~workspaceFolders:
            (WorkspaceFoldersServerCapabilities.create ~supported:true
                ~changeNotifications:(`Bool true) ())
          ())

      (* ~hoverProvider:(`Bool true) ~definitionProvider:(`Bool true) ~referencesProvider:(`Bool true) *)
      (* ~declarationProvider:(`Bool true) ~documentHighlightProvider:(`Bool true)
      ~codeActionProvider:(`Bool true)
      ~renameProvider:(`RenameOptions (RenameOptions.create ~prepareProvider:true ())) *)
      (* ~workspaceSymbolProvider:(`Bool true) *)
      (* ~executeCommandProvider:(ExecuteCommandOptions.create ~commands:[ ] ()) *)

      ()
  in
  InitializeResult.create
    ~serverInfo:{ name = "som-lsp"; version = Some Somc.Config.Cli.version }
    ~capabilities ()

let handle () = Ok init_info
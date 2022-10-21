import { execSync } from 'child_process';
import { createContext } from 'vm';
import { window, ExtensionContext, StatusBarAlignment, WorkspaceConfiguration, workspace } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

function setStatusbarItem(conf: WorkspaceConfiguration) {
	try {
		let path = conf.get<string>("languageServerPath");
		const output = execSync(path + ' --version').toString();

		let i = window.createStatusBarItem(StatusBarAlignment.Left, 0);
		i.text = output.replace(path, 'Som: ');
		i.tooltip = "Open settings";
		i.command = {
			title: i.tooltip,
			command: "workbench.action.openSettings",
			arguments: ["som.languageServerPath"],
			tooltip: i.tooltip
		}
		i.show();
	}
	catch (error) {
		window.showErrorMessage(`Failed to get Som language server version:\n\"${error}\"`);
	}
}

export function start(conf: WorkspaceConfiguration, ctx: ExtensionContext) {
	if (client) client.stop();
	else setStatusbarItem(conf);

	const exepath = conf.get<string>("languageServerPath");

	const serverOptoins: ServerOptions = {
		command: exepath,
		transport: TransportKind.stdio
	}
	
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "som" }],
		initializationOptions: conf,
		diagnosticCollectionName: "som",
		progressOnInitialization: true,
	};

	client = new LanguageClient(
		"som-lsp",
		"Som LSP client",
		serverOptoins,
		clientOptions
	);

	ctx.subscriptions.push(client.start());
}

export function stop(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
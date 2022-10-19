import { workspace, ExtensionContext } from 'vscode';
import * as lsp from './lspClient';

export function activate(ctx: ExtensionContext): any {
	let conf = workspace.getConfiguration('som');

	if(conf.get<boolean>('enableLanguageServer')) {
		console.log("starting lsp")
		lsp.start(conf, ctx)
	}
}

export function deactivate() {
	lsp.stop();
}
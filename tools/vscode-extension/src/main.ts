import { workspace, ExtensionContext, commands } from 'vscode';
import * as lsp from './lspClient';

export function activate(ctx: ExtensionContext): any {
	let conf = workspace.getConfiguration('som-lang');

	if(conf.get<boolean>('enableLanguageServer')) {
		console.log("starting lsp")
		lsp.start(conf, ctx)
	}

	ctx.subscriptions.push(commands.registerCommand(
		"som-lang.startLanguageServer", () => lsp.start(conf, ctx)
	));
	ctx.subscriptions.push(commands.registerCommand(
		"som-lang.restartLanguageServer", () => lsp.stop().then(() => lsp.start(conf, ctx))
	));
	ctx.subscriptions.push(commands.registerCommand(
		"som-lang.stopLanguageServer", lsp.stop
	));
}

export function deactivate() {
	lsp.stop();
}
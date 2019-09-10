import * as path from 'path';
import { execFile } from 'child_process';
import { workspace, ExtensionContext } from 'vscode';
import { waitForSocket } from 'socket-retry-connect';

import {
    LanguageClient,
    LanguageClientOptions,
    StreamInfo
} from 'vscode-languageclient';

let client: LanguageClient;

function startServer(serverPath: string, serverPort: number) {
    return function () {
        execFile(serverPath, [ 'start' ]);

        console.log('waiting for erlang_ls to accept connection..');

        return new Promise<StreamInfo>((resolve, reject) => {
            waitForSocket({ port: serverPort }, function(err, socket) {
                if (err) {
                    reject(err);
                }
                else {
                    console.log('socket accepted, continuing.');
                    resolve({ reader: socket, writer: socket });
                }
            });
        });
    };
};

export async function activate(context: ExtensionContext) {
    let serverPath = context.asAbsolutePath(
        path.join('_build', 'default', 'rel', 'erlang_ls', 'bin', 'erlang_ls')
    );

    let serverPort = 10000;

    let clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'erlang' }],
        synchronize: {
            fileEvents: [
                workspace.createFileSystemWatcher('**/rebar.config'),
                workspace.createFileSystemWatcher('**/rebar.lock')
            ]
        }
    };

    client = new LanguageClient(
        'erlang_ls',
        startServer(serverPath, serverPort),
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

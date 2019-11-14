# erlang_ls

![erlang_ls](images/erlang-ls-logo-small.png?raw=true "Erlang LS")

[![Build Status](https://travis-ci.org/erlang-ls/erlang_ls.svg?branch=master)](https://travis-ci.org/erlang-ls/erlang_ls)
[![Coverage Status](https://coveralls.io/repos/github/erlang-ls/erlang_ls/badge.svg?branch=master)](https://coveralls.io/github/erlang-ls/erlang_ls?branch=master)

An Erlang server implementing Microsoft's Language Server Protocol 3.0.

## Disclaimer

This project is still under heavy development and it is therefore not
still suitable as a fully-functional language server for daily usage.
Said that, contributors and early users are extremely welcome.

If you have any issues or questions about the project, feel free to
open a new issue. You can also join the #language-server channel in
the _Erlanger_ Slack if you would like to get involved or if you
prefer a more informal mean of communication.

A [wiki](https://github.com/erlang-ls/erlang_ls/wiki) is also
available and I occasionally blog about the project on
[Medium](https://medium.com/about-erlang).

## Minimum Requirements

* Erlang OTP 21+
* rebar3 3.9.1+

## Quickstart

Compile the project:

    make

## Command-line Arguments

These are the command-line arguments that can be provided to the
`erlang_ls` escript:

| Argument                | Description                                                                                     |
|-------------------------|-------------------------------------------------------------------------------------------------|
| --transport tcp / stdio | Specifies the transport the server will use for the connection with the client (default: `tcp`) |
| --port PORT             | Used when the transport is `tcp` (default: `10000`)                                             |
| --log-dir DIR           | Directory where logs will be written. When not provided no logs are generated.                  |

### Emacs Setup

The official `lsp-mode` package already includes a client for the
Erlang Language Server, so simply add the following to your `.emacs`
file (assuming the `erlang_ls` executable is available in your `PATH`:

    ;; Install the yasnippet dependency
    (package-install 'yasnippet)
    ;; Install the official lsp-mode package
    (package-install 'lsp-mode)
    ;; It is usually a good idea to install lsp-ui as well
    (package-install 'lsp-ui)
    ;; The lsp-ui sideline can become a serious distraction, so you
    ;; may want to disable it
    (setq lsp-ui-sideline-enable nil)
    ;; Ensure docs are visible
    (setq lsp-ui-doc-enable t)

    ;; Enable LSP automatically for Erlang files
    (add-hook 'erlang-mode-hook #'lsp)

    ;; Override the key bindings from erlang-mode to use LSP for completion
    (eval-after-load 'erlang
      '(define-key erlang-mode-map (kbd "C-M-i") #'company-lsp))

Ensure you have Erlang (i.e. `erl`, `escript` and friends) as part
of your Emacs path. If you don't, you can try the following:

    ;; Ensure your Emacs environment looks like your user's shell one
    (package-require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)

To enable logging on the client-side, just:

    (setq lsp-log-io t)

### IntelliJ Setup

> **WARNING**: The current version of the IntelliJ LSP plugin (1.5.4)
> is quite limited, so not all of the Erlang Language Server
> capabilities are available in IntelliJ.

First of all, ensure you have the [LSP
Support](https://github.com/gtache/intellij-lsp) plugin installed. If
you don't, you can simply navigate to:

    Preferences > Plugins > Browse Repositories

Search for "LSP Support" and install the respective plugin.

Restart IntelliJ, then navigate to:

    Preferences > Languages and Frameworks > Language Server Protocol > Server Definitions

There you can instruct IntelliJ on how to start the server. Select
`Raw Command`, set `erl;hrl` as the extension, then add as the
command:

    /ABSOLUTE/PATH/TO/erlang_ls/_build/debug/bin/erlang_ls --transport stdio

Ensure you use an absolute path. The plugin does not seem to
understand the `~` symbol. For the above command to work, IntelliJ
requires the `PATH` variable to be correctly configured to include
Erlang 20+. To circumvent this issues on Mac OS, the best way is to
start IntelliJ from the terminal (i.e. via the `idea` command) and not
via Spotlight.

To visualize documentation and type specs while hovering a function,
ensure the _Show quick documentation on mouse move_ option is enabled
in your IntelliJ preferences:

    Preferences > Editor > General

There, you can also set a delay in milliseconds.

For more information about how to configure the IntelliJ LSP Client,
please refer to the project [GitHub
page](https://github.com/gtache/intellij-lsp).

#### Troubleshooting the IntelliJ Setup

In some cases, the IntelliJ LSP client may not be able to connect to
the server. In such cases, the first step is to enable logging:

    Preferences > Languages and Frameworks > Language Server Protocol

Check the _Log servers communications_ check-box there.

After restarting IntelliJ, you will notice an extra `lsp` directory
created inside your Erlang project. This directory contains the
`error` and `output` logs, which should give you a hint about what is
going on.

An alternative source of information is represented by the IntelliJ
logs:

    Help > Show Logs

## Features

This section summarizes the functionalities currently implemented by
the `erlang_ls` server.

### Code Navigation

Code navigation is currently available for the following elements:

* Function applications (both local and remote)
* BIFs
* Behaviour attributes
* Included files
* Type definitions
* Record definitions
* Macro definitions
* Export list entries
* Import list entries

### Code Completion

Completion requests are sent from the client to the server to compute
completion items at a given cursor position. Completion items are
presented to the user for easy selection.

Code completion are currently triggered by the following characters:
`:`, `?`.

Code completion is available for the following elements:

* Exported functions

## Customization

It is possible to customize the behaviour of the `erlang_ls` server by
adding a `erlang_ls.config` file to the root of your projects.

A sample `erlang_ls.config` file would look like the following:

    otp_path: "/path/to/otp/lib/erlang"
    deps_dirs:
      - "lib/*"

Currently, the following customizations are possible:

| Parameter     | Description                                                                          |
|---------------|--------------------------------------------------------------------------------------|
| otp\_path     | Path to the OTP installation                                                         |
| deps\_dirs    | List of directories containing dependencies. It supports wildcards.                  |
| include\_dirs | List of directories provided to the compiler as include dirs. It supports wildcards. |

## Troubleshooting

It is possible to compile and start the language server in _debug mode_:

    rebar3 as debug escriptize

Ensure you update your `PATH` to include the new executable which now
resides in:

    /path/to/erlang_ls/_build/debug/bin/erlang_ls

Once an instance of the server is running, find the name of the node in
the [logs](#logs) or by running `epmd -names`. It will look something like:

    $ epmd -names
    epmd: up and running on port 4369 with data:
    name erlang_ls_62880311918 at port 50819

And you can connect to it via:

    erl -sname debug -remsh erlang_ls_62880311918@`HOSTNAME`

The [redbug](https://github.com/massemanet/redbug) application is
included in _debug mode_, so feel free to use it.

### Logging

When the escript is built using the `debug` profile as above, logging
will be enabled and the logs will be written to `/usr/local/var/log/erlang_ls`.
It's possible to change this directory either by modifying the default
value in the `erlang_ls.app.src` file or by having the LSP client
being used to provide a `--log-dir` option.

When the `escript` is built in the `default` mode (i.e. `rebar3 escript`),
no log files are generated, unless the `--log-dir` option is provided.

## References

https://microsoft.github.io/language-server-protocol/

## License

The `erlang_ls` project is licensed under the Apache License 2.0. Please refer to the `LICENSE` file for details.

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

    rebar3 escriptize

### Emacs Setup

The official `lsp-mode` package already includea a client for the
Erlang Language Server, so simply add the following to your `.emacs`
file (assuming the `erlang_ls` executable is available in your `PATH`:

    ;; Install the yasnippet dependency
    (package-install 'yasnippet)
    ;; Install the official lsp-mode package
    (package-install 'lsp-mode)
    ;; Enable LSP automatically for Erlang files
    (add-hook 'erlang-mode-hook #'lsp)

Ensure you have Erlang (i.e. `erl`, `escript` and friends) as part
of your Emacs path. If you don't, you can try the following:

    ;; Ensure your Emacs environment looks like your user's shell one
    (package-require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)

To enable logging on the client-side, just:

    (setq lsp-log-io t)

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

### Code Completion (incomplete)

Completion requests are sent from the client to the server to compute
completion items at a given cursor position. Completion items are
presented to the user for easy selection.

Code completion are typically triggered by one of the following
characters: `: # . ?`.

Code completion is available for the following elements:

* Erlang keywords (e.g. `case`, `receive`)
* Variables in the current scope
* Local function names
* Built-in functions (e.g. `now()`)
* Module names
* Macros
* Record names
* Record fields
* Atom names
* Module attributes

## Troubleshooting

It is possible to compile and start the language server in _debug mode_:

    rebar3 as debug escriptize

Ensure you update your `PATH` to include the new executable which now
resides in:

    /path/to/erlang_ls/_build/debug/bin/erlang_ls

Once an instance of the server is running you can connect to it via:

    erl -sname debug -remsh erlang_ls@`HOSTNAME`

The [redbug](https://github.com/massemanet/redbug) application is
included in _debug mode_, so feel free to use it.

## References

https://microsoft.github.io/language-server-protocol/

# erlang_ls

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
file:

    ```elisp
    ;; Require the official lsp-mode package
    (require 'lsp-mode)
    ;; Configure the location of the Erlang language server
    (setq lsp-erlang-server-install-dir "/path/to/erlang_ls")
    ;; Enable LSP automatically for Erlang files
    (add-hook 'erlang-mode-hook #'lsp)
    ```

    Ensure you have Erlang (i.e. `erl`, `escript` and friends) as part
    of your Emacs path. If you don't, you can try the following:

    ```elisp
    ;; Ensure your Emacs environment looks like your user's shell one
    (package-require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    ```

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

## References

https://microsoft.github.io/language-server-protocol/

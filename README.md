# erlang_ls

[![Build Status](https://travis-ci.org/erlang-ls/erlang_ls.svg?branch=master)](https://travis-ci.org/erlang-ls/erlang_ls)
[![Coverage Status](https://coveralls.io/repos/github/erlang-ls/erlang_ls/badge.svg?branch=master)](https://coveralls.io/github/erlang-ls/erlang_ls?branch=master)

An Erlang server implementing Microsoft's Language Server Protocol 3.0.

## Features

This section summarizes the functionalities implemented and available
via the LSP protocol.

### Code Completion

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

### Code Snippets



## Get in Touch

This project is still in a very early stage. To get
in touch, feel free to join the #language-server channel in the
Erlanger Slack.

## Dev Quickstart

    $ rebar3 shell
    > lager:set_loglevel(lager_console_backend, debug).

## Running the Tests

    $ rebar3 as eqc ct

## Coverage Reports

    $  rebar3 as eqc cover --verbose

## Emacs Setup

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

    Ensure you have Erlang (i.e. `erl`, `escript` and friends) as part of your Emacs path.
    If you don't, you can try the following:

    ```elisp
    ;; Ensure your Emacs environment looks like your user's shell one
    (package-require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    ```

## References

https://microsoft.github.io/language-server-protocol/

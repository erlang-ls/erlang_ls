erlang_ls
=====

[![Build Status](https://travis-ci.org/erlang-ls/erlang_ls.svg?branch=master)](https://travis-ci.org/erlang-ls/erlang_ls)
[![Build Status](http://quickcheck-ci.com/p/erlang-ls/erlang_ls.svg)](http://quickcheck-ci.com/p/erlang-ls/erlang_ls)

An Erlang server using Microsoft's Language Server Protocol 3.0.

Get in Touch
----

This project is still in a very early stage. To get
in touch, feel free to join the #language-server channel in the
Erlanger Slack.

Dev Quickstart
-----

    $ rebar3 shell
    > lager:set_loglevel(lager_console_backend, debug).

Running the Tests
-----

    $ rebar3 as eqc ct

Coverage Reports
-----

    $  rebar3 as eqc cover --verbose

Emacs Setup
-----

    ;; Language Server Protocol Tests
    (require 'lsp-mode)

    ;; Enable code completion
    (require 'company-lsp)

    ;; Connect to an already started language server
    (lsp-define-tcp-client
     lsp-erlang-mode
     "erlang"
     (lambda () default-directory)
     '("/usr/local/opt/coreutils/libexec/gnubin/false")
     "localhost"
     9000)

    (add-hook 'erlang-mode-hook 'company-mode)
    (add-hook 'erlang-mode-hook (lambda ()
                                  (push 'company-lsp company-backends)
                                  )
              )
    (add-hook 'erlang-mode-hook 'lsp-erlang-mode-enable)

Manual enable the server for a buffer:

M-x company-mode
M-x company-lsp
M-x lsp-erlang-mode-enable

References
-----

https://microsoft.github.io/language-server-protocol/

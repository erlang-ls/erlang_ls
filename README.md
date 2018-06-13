erlang_ls
=====

An Erlang server using Microsoft's Language Server Protocol 3.0.

Get in Touch
----

This project is still in a very early stage. To get
in touch, feel free to join the #language-server channel in the
Erlanger Slack.

Dev Quickstart
-----

    $ rebar3 shell
    application:ensure_all_started(erlang_ls).
    Opts = [{msgs, 1000}, {time, 99999}, {print_file, "/tmp/redbug"}],
    redbug:start("erlang_ls_protocol->return", Opts).

Emacs Setup
-----

    ;; Language Server Protocol Tests
    (require 'lsp-mode)

    ;; Enable code completion
    (require 'company-lsp)
    (push 'company-lsp company-backends)

    ;; Connect to an already started language server
    (lsp-define-tcp-client
     lsp-erlang-mode
     "erlang"
     (lambda () default-directory)
     '("/usr/local/opt/coreutils/libexec/gnubin/false")
     "localhost"
     9000)

    (add-hook 'erlang-mode #'lsp-erlang-mode-enable)

Manual enable the server for a buffer:

M-x company-mode
M-x company-lsp
M-x lsp-erlang-mode-enable

References
-----

https://microsoft.github.io/language-server-protocol/

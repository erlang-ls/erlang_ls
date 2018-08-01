# erlang_ls

[![Build Status](https://travis-ci.org/erlang-ls/erlang_ls.svg?branch=master)](https://travis-ci.org/erlang-ls/erlang_ls)
[![Build Status](http://quickcheck-ci.com/p/erlang-ls/erlang_ls.svg)](http://quickcheck-ci.com/p/erlang-ls/erlang_ls)

An Erlang server using Microsoft's Language Server Protocol 3.0.

## Rationale

## Principles

## History of the project

I am an Erlang developer and an affectionate Emacs user. For most of
my professional career I have been using Emacs's
[erlang-mode](http://erlang.org/doc/apps/tools/erlang_mode_chapter.html)
and, once available, the [EDTS](https://github.com/tjarvstrand/edts)
suite, whose author is a former colleague. These two tools, combined
with a few extra customizations and additions, provided me with a
decent development environment for Erlang. I felt quick and productive
when writing and navigating Erlang code and I never really missed a
more powerful IDE.

I occasionally experienced a few bugs with these tools and identified
a few potential improvements, but nothing major that would trigger the
quest for a better alternative. At some point I started consulting for
a company which used _comma-first_ indentation and Emacs' automatic
indentation started to break here and there. I started switching
heavily between projects using different versions of the Erlang
Runtime system and EDTS, even if in theory supported multiple OTP
runtimes, occasionally started to crash and spam my _EDTS_ buffer with
error messages related to
[xref](http://erlang.org/doc/apps/tools/xref_chapter.html), the Erlang
cross-reference tool. Reverting Emacs buffers, killing and restarting
the EDTS process, became normal part of my developer workflow. After a
recent upgrade, EDTS started to perform poorly. It would get
occasionally stuck and I had to press `C-g` a few times to recover
control of my Emacs. When working on huge projects (in terms of lines
of code), EDTS was freezing my entire Emacs every time I was
navigating into a newly-opened module.

It was 2018 when I contacted EDTS' author to ask if I was the only one
experiencing these issues. Some of these problems were apparently
known, but not easily reproducible. Also,
[Thomas](http://github.com/tjarvstrand) (EDTS's author) was not using
Erlang anymore, so he had very little incentive in improving or even
maintaining EDTS. I shivered for a second, since I was relying so much
on EDTS. It was at that point that Thomas asked me if I was interested
in contributing to EDTS or even becoming a maintener for it.

I started diving into the EDTS code-base and learnt about its
architecture. It was surprisingly interesting. At start-up, EDTS would
spin up a distributed Erlang node (named _edts_) and then a separate
Erlang node for each _project_. The two Erlang nodes would communicate
between each other using the standard Erlang Distribution
Protocol. The _edts_ node exposed an HTTP API that the Emacs client
interacted with. This setup allowed (with some limitations) different
versions of the Erlang runtime for different projects. The approach
had a lot of potential. One could implement a lot of interesting
features in an easy way by using the standard Erlang libraries. In
principle, if one could make the HTTP API generic enough and not
Emacs-specific, other editors could have leveraged these libraries. This
would have been particularly beneficial in a small community like the
Erlang one. Just think about it: a bug fix to the way return values
for a `spec` are indented would automatically be available to users of all
major text editors and IDEs. Unfortunately, the client part in EDTS
was still quite heavy and a lot of functionalities
(e.g. auto-completion) were too coupled with Emacs and implemented
directly in Emacs Lisp. My interest, though, was growing.

A couple of weeks later I attended the [Code
Beam](https://codesync.global/conferences/code-beam-sto-2018/)
conference in Stockholm. I was sharing my ideas with a few attendees
when [Csaba](https://github.com/hcs42) (co-author of
[vim-erlang](https://github.com/orgs/vim-erlang)) told me that
[Vlad](https://github.com/vladdu) (co-author of the
[erlide](https://github.com/erlide) Eclipse plugin) was working on a
similar project. It was time for me to attend [Vlad's
talk](https://www.youtube.com/watch?v=2IXgecLsA9g).

At Vlad's talk I learnt that the idea of having a well defined API
between text editors (or IDEs) on one side and _language-specific_
servers on the other one [was not so new](https://langserver.org/). It
was not surprising to see that the driver for such an approach was
_Microsoft_, which obviously faced the problem of supporting multiple
programming languages (e.g.. _C_, _C++_, _C#_, _F#_, _TypeScript_) in
a common IDE (i.e. _Visual Studio_). What was surprising (at least for
me) was that Microsoft actually defined and open-sourced a
fully-bledged [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/specification)
(aka _LSP_) to solve the problem, and that all major text editors and
IDEs (including Emacs \o/) [already implemented a
client](https://langserver.org/#implementations-client) for it.

I started looking for implementations of Language Servers for Erlang
and found a few ones (including one for Elixir), all at different
development stages and using different approaches (see the _other LSP
implementations_ section for details).

It was time to roll my own.

## Other LSP Implementations

### sourcer

### vscode_erlang

### EDTS

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

## References

https://microsoft.github.io/language-server-protocol/

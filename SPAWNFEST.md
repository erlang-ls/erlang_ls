# Spawnfest 2020

## Erlang LS Meets the DAP Protocol

Debug your Erlang code directly in your favourite text-editor.

![erlang-ls-meets-dap](images/erlang-ls-meets-dap.png?raw=true "Erlang LS Meets DAP")

## Authors

Roberto Aloi (Klarna -Stockholm, Sweden)
Juan Facorro (Klarna - Stockholm, Sweden)
Alan Zimmermann (WhatsApp - London, UK)

## Terminology

[Erlang LS](http://erlang-ls.github.io/) is an editor-agnostic [language server](erlang-ls.github.io) which provides language features for the Erlang programming language using the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/), or _LSP_ in short.

The [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/), or _DAP_ in short, is a similar protocol for communication between a client (a text editor or an IDE) and a _Debug Server_. The protocol is designed to allow the creation of step-by-step debuggers directly in the editor.

## Rationale

One of the strengths of the Erlang programming language is the ability to seamlessly _debug_ and _trace_ Erlang code. Many tools and libraries exist, but they are sometimes under-utilized by the Community, either because their API is not intuitive (think to the [dbg](https://erlang.org/doc/man/dbg.html) Erlang module), or because they offer a limited, obsolete, UI (think the [debugger](http://erlang.org/doc/apps/debugger/debugger_chapter.html) application).

We want to solve this problem by leveraging some of the existing debugging and tracing facilities provided by Erlang/OTP and bringing the debugging experience directly in the text-editor, next to the code, improving the user experience when using such
tools.

[This video](https://www.youtube.com/watch?v=ydcrdwQKqI8&t=3s) shows what debugging Erlang code is like via the _debugger_ application. [This other video](https://www.youtube.com/watch?v=ydcrdwQKqI8) shows what the same experience looks like from Emacs.

Due to the editor-agnostic nature of the _DAP_ protocol, a very similar experience is delivered to users of a different development tool, be it _Vim_, _VS Code_ or _Sublime Text 3_.

## Project Goal

We would like to use the opportunity provided by the _Spawnfest 2020_ to implement a _Proof of Concept_. In the _POC_ we demonstrate that it is possible, with a relatively small effort, to raise the usability standards for an Erlang user with regards to debugging and tracing.

Specifically, we aim at creating a step-by-step debugger based on the [Erlang Interpreter](http://erlang.org/doc/man/int.html), a not very well known module in Erlang/OTP, which is used as a low-level API to build tools such as [debugger](http://erlang.org/doc/man/debugger.html) and the [cedb](https://github.com/hachreak/cedb) debugger.

We will inspire our work to the implementation from the [Elixir LS](https://github.com/elixir-lsp/elixir-ls) language server, which already uses this approach.

## Personal Goals

During the weekend, we will try to:

* Get some familiarity with the _DAP_ protocol
* Get some familiarity with the _Erlang Interpreter_ (aka the _int_ module)
* Figure out a strategy to add support for the _DAP_ protocol into the Erlang LS project
* Pair most of the times and learn from each other
* Learn some Elixir along the way

## What We Achieved

* Basic understanding of the _DAP_ protocol
* Basic understanding of the _Erlang Interpreter_  (aka the _int_ module)
* Installed and configured the _dap-mode_ for Emacs to work with a new, Erlang, Debug Adapter
* Plug support for the _DAP_ protocol in Erlang LS
* Handle the following _DAP_ requests (some requests have been simplified for timing reasons)
  * initialize
  * launch
  * configurationDone
  * setBreakpoints
  * setExceptionBreakpoints
  * threads
  * stackTrace
  * scopes
  * next
  * continue
  * stepIn
  * evaluate
* Creation of a [toy project](https://github.com/erlang-ls/daptoy) to showcase the new Debug Adapter
* Creation of the two videos above to showcase the past and future of Erlang debugging

## Follow Ups

* Add unit, integration and property-based tests for the new code
* Properly decouple the "language server" and the "debug adapter" code in two separate applications
* Support the full DAP protocol
* Support the "Attach" and "Run in Terminal" configurations in addition to the "Launch" one
* Robustify the code by making it resilient to errors (eg regarding Erlang distribution)

## Future Third-Party Contributions

While working on the project, we noticed a few issues with third-party projects that we plan to address:

* The `dap-mode` Emacs can be improved:
  * Logs should be generated in separate buffers
  * The client ID should not be hard-coded in the `initialize` message
  * The `configurationDone` request should respect server capabilities
* The `int` Erlang module should be improved:
  * Part of the API (eg the `next` function) is internal or undocumented
  * The API to retrieve bindings could be simplified
  * The callback function could be extended to accept a `fun` instead of a `MFA`
  * The _attached process_ concept could be extended into a proper Erlang behaviour

## Spawnfest Instructions for Judges (Emacs)

### Requirements

* Erlang LS: https://erlang-ls.github.io/editors/emacs/
* DAP mode for Emacs: https://emacs-lsp.github.io/dap-mode/

### Build

* Checkout the `dap` branch from the https://github.com/spawnfest/frj repository
* Run `rebar3 as dap escriptize`
* Ensure the `_build/dap/bin/els_dap` escript is in your `PATH`

### Customize your Emacs

```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang DAP                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dap-mode)

;; Log io to *Messages*. Seems to be the only option at the moment. Optional
(setq dap-inhibit-io nil)
(setq dap-print-io t)

(defun dap-erlang--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path '("els_dap"))
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
      (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))
      ))

;; Add a Run Configuration for running 'rebar3 shell'
(dap-register-debug-provider "Erlang" 'dap-erlang--populate-start-file-args)
(dap-register-debug-template "Erlang rebar3 shell"
                             (list :type "Erlang"
                                   :program "rebar3"
                                   :args "shell"
                                   :name "Erlang::Run"))

;; Add a Run Configuration for executing a given MFA
(dap-register-debug-template "Erlang MFA"
                             (list :type "Erlang"
                                   :module "daptoy_fact"
                                   :function "fact"
                                   ;; :function "dummy"
                                   :args "[3]"
                                   :name "Erlang::Run MFA"))

;; Add a Run Configuration for running 'rebar3 shell'
;; in an integrated terminal in the client
;; NOTE: set the projectnode hostname in two places
(dap-register-debug-template "Erlang Terminal"
                             (list :type "Erlang"
                                   :runinterminal '("rebar3" "shell" "--name" "dapnode@alanzimm-mbp")
                                   :projectnode "dapnode@alanzimm-mbp"
                                   :name "Erlang::Terminal"))

;; Add a Run Configuration for running 'rebar3 shell'
;; in an integrated terminal in the client, running a given MFA
;; NOTE: set the projectnode hostname in two places
(dap-register-debug-template "Erlang Terminal MFA"
                             (list :type "Erlang"
                                   :runinterminal '("rebar3" "shell" "--name" "dapnode@alanzimm-mbp")
                                   :projectnode "dapnode@alanzimm-mbp"
                                   :module "daptoy_fact"
                                   :function "fact"
                                   ;; :function "dummy"
                                   :args "[4]"
                                   :name "Erlang::Terminal MFA"))





(require 'dap-mode)

;; Show debug logs
(setq dap-inhibit-io nil)
(setq dap-print-io t)

(defun dap-erlang--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path '("els_dap"))
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))))

;; Add a Run Configuration for running 'rebar3 shell'
(dap-register-debug-provider "Erlang" 'dap-erlang--populate-start-file-args)
(dap-register-debug-template "Erlang rebar3 shell"
                             (list :type "Erlang"
                                   :program "rebar3"
                                   :args "shell"
                                   :name "Erlang::Run"))

;; Add a Run Configuration for executing a given MFA
(dap-register-debug-template "Erlang MFA"
                             (list :type "Erlang"
                                   :module "daptoy_fact"
                                   :function "fact"
                                   :args "[5]"
                                   :name "Erlang::Run"))
```

### Clone the DAP Toy Project

* `git clone https://github.com/erlang-ls/daptoy`
* `cd daptoy`

### Run the Debugger

* Open the `src/daptoy_fact.erl` file in Emacs
* Add a breakpoint at a given line using `dap-breakpoints-add` on that line
* Run `dap-debug`
* Select the `Erlang MFA` Run Configuration (that will run the `daptoy_fact:fact(5)` by default)
* Step through code via `dap-next`

You can also select the `Erlang rebar3 shell` configuration as an alternative.
You can then attach to the spawned node and run custom MFAs to see the debugger in action.
See `epmd -names` for details about the node name.

Since the setup for the project can be time-consuming, you can also
enjoy the video to get the _Quickstart_ experience:

https://www.youtube.com/watch?v=ydcrdwQKqI8

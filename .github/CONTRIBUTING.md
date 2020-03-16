# Contributing to `erlang_ls`

:+1::tada: First off, thanks for taking the time to contribute! :tada::+1:

The following is a set of guidelines for contributing to `erlang_ls`,
which is hosted in the [erlang-ls
Organization](https://github.com/erlang-ls) on GitHub. These are
mostly guidelines, not rules. Use your best judgment, and feel free to
propose changes to this document in a pull request.

All contributions are highly welcome in the form of code,
documentation, bug reports and opened issues.

Whenever reviewing or commenting other people contributions, please
keep a positive attitude. Treat other contributors respectfully and be
constructive, always appreciating the effort and the time other
contributors are putting into the project.

## What should I know before I get started?

`erlang_ls` is an implementation of Microsoft's Language Server
Protocol, so you should be familiar with it. You can read about the
protocol [here][lsp]. You can find the detailed specification
[here][lsp-specs].

If you are curious about the `erlang_ls` project itself, you can also
have a look to the [wiki][wiki].

If you have a question, please do not open an issue. Instead, start a
discussion using the channel named `#language-server` in the Erlanger
Slack. Feel free to open an issue if you:

* have a specific feature request,
* would like to report a bug.

## Contribution Principles

Prefer small, reviewable changes over big changes.

Always discuss major contributions or refactoring with the authors,
before starting. This will avoid duplicated efforts and ensure
everyone is aligned and agrees with the planned changes.

Before starting to work on a big task, assign the task to yourself. If
a task is assigned to someone else and you would like to help out,
please contact the assigned person first.

Before starting a major task, always describe the problem you are
trying to solve in an issue. Explain the way you plan to address a
problem. This may save you a lot of time during the review phase.

## Styling Conventions

We try to follow the following conventions in the Erlang code:

### 80 char limit for lines

Even if you are not using punch cards for writing code and our screens
can usually display more than those characters, we still find that
this rule encourages writing of simple, straightforward code.

### Comma-first for lists and maps

We prefer this:

    #{ one = One
     , two = Two
     }

Over:

    #{ one = One,
       two = Two
     }

### Alignment for match operators and commas

We prefer this:

    First      = {first      , 1},
    Second     = {second     , 2},
    TwentyFour = {twentyfour , 24}

Over:

    First = {first, 1},
    Second = {second, 2},
    TwentyFour = {twentyfour, 24}

Whenever possible, we will try to enforce these conventions via linters.

### Commit Messages

* Use descriptive commit messages.
* Prefix every commit message with the issue number in the form `[#NNN]`.
* Use imperative form (prefer `Move function...` over `Moves function`).
* Avoid past tense.
* Use a final dot.

A good commit may look like this:

    [#42] Discover the meaning of life.

### Final Note

These CONTRIBUTING notes are partially inspired by the awesome ones of
the [Atom][atom-contributing] and [Rails][rails-contributing]
projects.

[lsp]:https://microsoft.github.io/language-server-protocol/
[lsp-specs]:https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/
[wiki]:https://github.com/erlang-ls/erlang_ls/wiki
[atom-contributing]:https://github.com/atom/atom/blob/master/CONTRIBUTING.md
[rails-contributing]:https://github.com/rails/rails/blob/master/CONTRIBUTING.md

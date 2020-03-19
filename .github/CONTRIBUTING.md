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

### Code Reviews

* Always be constructive when providing feedback (e.g. be explicit on
  what could be improved and how)
* Use [GitHub suggestion blocks][github-suggest] for fixing typos and
  small mistakes
* Positive feedback is as important as negative feedback
* Only the Pull Request author(s) should push changes to the branch
  under review
* Reviewers should be as responsive as possible in providing their
  reviews.
* Every review should be completed via either an _Approve_ or a
  _Request Changes_ action. Not selecting either option should be
  considered as a _review in progress_.
* Code reviews should be completed within a reasonable time
  window. Leaving comments in time-separated _bulks_ is discouraged.
* It is not acceptable to mark a Pull Request via a _needs work_
  without further explanation.
* PR Conversations can be marked as _resolved_ by the PR author, but
  only if at least one of the following conditions holds:
    * The issue is addressed via a code change;
    * the issue will be tackled in a follow up Pull Request (a
      follow-up ticket should be created and linked in this case);
    * The issue is marked as a _won't fix_ and an explanation is
      provided.
* Once a PR conversion is marked as _resolved_, it is the
  responsibility of both the Pull Request author and the reviewer to
  verify that the original issue has been properly addressed (either
  via a code change or a clear explanation or follow-up ticket).
* When a trivial task (e.g. fixing a typo) is marked as _done_ via a
  code change, no further explanation is required. A _Fixed_ comment
  can be added by the author of the Pull Request, but it is not
  mandatory.
* Nit-picking comments must be prefixed by the `Nit:` prefix. If
  possible, they should be addressed via a _code suggestion
  block_. The Pull Request author is encouraged to fix nit-picking
  comments, but they should not be considered as blockers.
* It is not acceptable to mark a Pull Request as a _Request Changes_
  in case only nit-picking comments are present.
* In case of post-merge comments, the original author of the Pull
  Request must be pinged directly via a `@` mention.
* It is acceptable to open a Pull Request to get some early feedback
  from other team members, even if the changes included in the Pull
  Request are not in a _reviewable_ state, yet. In this case, though,
  the Pull Request title _must_ be prefixed with `[WIP]`.

### Final Note

These CONTRIBUTING notes are partially inspired by the awesome ones of
the [Atom][atom-contributing] and [Rails][rails-contributing]
projects.

[lsp]:https://microsoft.github.io/language-server-protocol/
[lsp-specs]:https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/
[wiki]:https://github.com/erlang-ls/erlang_ls/wiki
[atom-contributing]:https://github.com/atom/atom/blob/master/CONTRIBUTING.md
[rails-contributing]:https://github.com/rails/rails/blob/master/CONTRIBUTING.md
[github-suggest]:https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/commenting-on-a-pull-request

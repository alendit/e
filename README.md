# e

`e` is a minimal Emacs Lisp package scaffold for an Emacs-hosted agent runtime.
It is intended to work as a normal Emacs package while also being convenient to
develop from Doom Emacs through Doom's local package recipe support.

## Development

Run tests with Eldev:

```sh
eldev test
```

Useful local checks:

```sh
eldev lint
eldev compile
eldev package
emacs -Q --batch -L . -L lisp -l e.el --eval "(require 'e)"
```

Inside an Emacs session, reload the package during live development with:

```elisp
(require 'e)
(require 'e-dev)
(e-dev-reload)
```

## Doom Emacs Local Install

Add this to `$DOOMDIR/packages.el`:

```elisp
(package! e
  :recipe (:local-repo "/Users/dimitrivorona/projects/elisp/e"
           :files ("e.el" "lisp")
           :build (:not compile)))
```

Then run:

```sh
~/.emacs.d/bin/doom sync
```

Restart Emacs after `doom sync`, or reload Doom's generated package autoloads
for the current session. Until the package/autoloads are loaded, commands such
as `M-x e-dev-reload` and `M-x e-chat-new` will not exist in `M-x`.

For an already-running Emacs session, the one-time manual bootstrap is:

```elisp
(load-file "/Users/dimitrivorona/projects/elisp/e/e.el")
```

After that, `M-x e-dev-reload` reloads the package modules from the checkout in
dependency order, including layer-local capability modules and presentation
shells such as `e-layers`, `e-emacs-base`, and `e-chat`.

The package itself does not depend on Doom-specific APIs.

## Chat Sessions

`M-x e-chat-new` creates a fresh persisted chat session.
Sessions are stored under `(locate-user-emacs-file "e/sessions/")` as
append-only JSONL files with a recent-session `index.json`.

Use `M-x e-chat-resume` to pick a recent session, `M-x e-chat-rename` to set
an explicit display name for the current session, and `M-x e-chat-set-model` or
`M-x e-chat-set-effort` to override the model settings for that session. If a
session has no explicit name, chat buffers use the first 25 characters of the
first user message as a fallback title, adding `...` when the prompt is longer.

## OpenAI-Like Providers

`e` configures OpenAI-like backends with Emacs Lisp provider profiles.  This is
profile data, not a TOML parser.

```elisp
(setq e-openai-model-providers
      '((codex
         :name "ChatGPT Codex"
         :base-url "https://chatgpt.com/backend-api/codex"
         :wire-api responses
         :requires-openai-auth t)
        (openai-compatible-gateway
         :name "OpenAI-Compatible Gateway"
         :base-url "https://gateway.example.test"
         :env-key "OPENAI_GATEWAY_API_KEY"
         :wire-api responses
         :requires-openai-auth nil)))

(setq e-openai-default-provider 'openai-compatible-gateway)
```

Profiles with `:requires-openai-auth t` use Codex-managed ChatGPT auth from
`CODEX_HOME/auth.json` or `~/.codex/auth.json`.  Profiles with
`:requires-openai-auth nil` read a bearer token from `:env-key` and send only
standard Responses headers.

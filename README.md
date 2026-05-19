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
           :files ("e.el" "lisp/*.el")
           :build (:not compile)))
```

Then run:

```sh
~/.emacs.d/bin/doom sync
```

The package itself does not depend on Doom-specific APIs.

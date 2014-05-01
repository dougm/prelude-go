## Synopsis

**Prelude Go** is a module for Emacs
[Prelude](https://github.com/bbatsov/prelude) that provides default
settings and packages for the Go programming language.

## Configuration

Add this repository to your `prelude/vendor` directory and add the
following to your `prelude-modules.el`:

```el
(require 'prelude-go)
```

Add the `gotest.el` repo to your `prelude/vendor`:
```bash
$ git clone https://github.com/dougm/gotest.el
```

TODO: gotest.el to MELPA, in some form.

## Go tools

Most of the Emacs Go packages have hard or soft dependencies on
programs installed via `go get`, such as `gocode`, `golint`, etc.
You can install these tools using: <kbd>M-x prelude-go-install-tools</kbd>
And update the tool using: <kbd>M-x prelude-go-update-tools</kbd>

# Emacs client wrapper

This directory contains a standalone shell wrapper for `emacsclient`.  It is
forked from the [Oh My Zsh Emacs plugin](https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/emacs)

The wrapper uses an Emacs daemon so opening files does not require starting a
new Emacs process.  It selects a suitable existing frame when possible and
creates one when necessary.

**Note:** Requires Emacs 24 or newer.

## Oh My Zsh

If you use [Oh My Zsh](https://ohmyz.sh/), use its upstream Emacs plugin
instead.  Add `emacs` to the `plugins` array in `.zshrc`:

```zsh
plugins=(... emacs)
```

## Bash or Other Shells

For Bash, such as on Linux, or another shell without Oh My Zsh, configure this
repository's wrapper in your shell configuration:

```bash
alias emacs='~/.emacs.d/util/emacsclient.sh --no-wait'
alias e=emacs
```

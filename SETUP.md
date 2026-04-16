# Setup Checklist

This is a machine setup checklist for the current Emacs configuration.

## Required For Daily Use

- [ ] `git`
- [ ] `git-lfs`
- [ ] `ripgrep` (`rg`)
- [ ] `gpg`
- [ ] one spell checker compatible with `flyspell`
- [ ] `multimarkdown`
- [ ] `podman`
- [ ] `podman-compose`
- [ ] `ty`
- [ ] `terraform-ls`
- [ ] `cmake-language-server`

## Required By Current Emacs Config

- [ ] `aider`
- [ ] `opencode`
- [ ] Ollama reachable at `OLLAMA_API_BASE` or `http://localhost:11434`
- [ ] OpenRouter API key available through `auth-source`
- [ ] forecast API key stored in `auth-source` for `api.darksky.net`
- [ ] Wolfram Alpha API key stored in `auth-source` for `api.wolframalpha.com`

## Required For Specific Workflows

### Python

- [ ] `python`
- [ ] `debugpy`

### Clojure

- [ ] `lein`

### macOS Integration

- [ ] `open`

## Optional Tools

- [ ] `pass` for `auth-source-pass`
- [ ] local geolocation checkout at `~/.emacs.d/dev/geolocation`
- [ ] `virtualenvwrapper` workflow with virtualenvs under `~/python-venv/`
- [ ] Slack credentials
- [ ] Mastodon account access
- [ ] Matrix access for `ement`

## Notes

- `flyspell` needs an external spell checker such as `aspell` or `hunspell`.
- Terraform completion, hover, and diagnostics come from `terraform-ls` through `eglot`.
- Python completion, hover, and diagnostics come from `ty` through `eglot`.
- CMake support uses `eglot` with `cmake-language-server` on `PATH`.
- `dap-mode` is still present, so Python debugging currently depends on `debugpy`.

## Current Install Status On This Mac

### Homebrew

- installed via Homebrew and found on `PATH`
- `git` at `/opt/homebrew/bin/git`
- `git-lfs` at `/opt/homebrew/bin/git-lfs`
- `ripgrep` (`rg`) at `/opt/homebrew/bin/rg`
- `gpg` from `gnupg` at `/opt/homebrew/bin/gpg`
- `aspell` at `/opt/homebrew/bin/aspell`
- `hunspell` at `/opt/homebrew/bin/hunspell`
- `podman` at `/opt/homebrew/bin/podman`
- `podman-compose` at `/opt/homebrew/bin/podman-compose`
- `ty` at `/opt/homebrew/bin/ty`
- `terraform-ls` at `/opt/homebrew/bin/terraform-ls`
- `opencode` at `/opt/homebrew/bin/opencode`
- `pass` is installed via Homebrew
- Python is available as `python3` at `/opt/homebrew/bin/python3`
- additional versioned Python executables found: `python3.11`, `python3.12`, `python3.14`

### uv Tool

- `uv` is installed at `/Users/neilo/.local/bin/uv`
- `uv tool` executable directory is `/Users/neilo/.local/bin`
- `uv tool` environments live under `/Users/neilo/.local/share/uv/tools`
- `cmake-language-server` is installed via `uv tool` at `/Users/neilo/.local/bin/cmake-language-server`
- `aider` is installed via `uv tool` at `/Users/neilo/.local/bin/aider`
- `aider-skills` is also installed via `uv tool`

### macOS Built-in

- `open` is expected to come from macOS

### Not Currently Found

- `multimarkdown`
- `python` (plain `python` is not on `PATH`; use `python3`)
- `debugpy`
- `lein`
- `pipx`

### Source Still To Confirm

- Ollama was not checked here; config expects it to be reachable at `OLLAMA_API_BASE` or `http://localhost:11434`

[![CI build badge][ci-badge]][ci]

***Flexible, simple tools for minibuffer completion in Emacs***

This repository contains:

- [**Ivy**](#ivy), a generic completion mechanism for Emacs.

- [**Counsel**](#counsel), a collection of Ivy-enhanced versions of
  common Emacs commands.

- [**Swiper**](#swiper), an Ivy-enhanced alternative to Isearch.

# Ivy

[![Ivy badge from GNU-devel ELPA][ivy-dev-badge]][ivy-dev]
[![Ivy badge from GNU ELPA][ivy-badge]][ivy]
[![Ivy badge from MELPA][ivy-melpa-badge]][ivy-melpa]
[![Ivy badge from MELPA Stable][ivy-stable-badge]][ivy-stable]

Ivy is a generic completion mechanism for Emacs.  While it operates
similarly to other completion schemes such as `icomplete-mode`, Ivy
aims to be more efficient, smaller, simpler, and smoother to use yet
highly customizable.

To try Ivy, just call <kbd>M-x</kbd> `ivy-mode`.  This will enable
generic Ivy completion, including specific completion for file and
buffer names.

## Installation

Install the `ivy` package from GNU ELPA or MELPA.

Users of Debian ≥10 (and derivatives such as Ubuntu ≥18.04) can
install Ivy, Counsel, and Swiper with `sudo apt install elpa-counsel`.
To add Hydra support: `sudo apt install elpa-ivy-hydra`.

## Documentation

### Manual

Installing `ivy` from GNU ELPA or MELPA also installs the manual under
the `(ivy)` Info node.

The source file for the Info manual is [`ivy.org`](doc/ivy.org).

The manual is also available as [HTML][ivy-manual].

### Wiki

There is an Ivy and Swiper [wiki on GitHub][ivy-wiki].

### Small config example

```elisp
(ivy-mode)
(setopt ivy-use-virtual-buffers t)
(setopt enable-recursive-minibuffers t)
;; Enable this if you want `swiper' to use it:
;; (setopt search-default-mode #'char-fold-to-regexp)
(keymap-global-set "C-s" #'swiper-isearch)
(keymap-global-set "C-c C-r" #'ivy-resume)
(keymap-global-set "<f6>" #'ivy-resume)
(keymap-global-set "M-x" #'counsel-M-x)
(keymap-global-set "C-x C-f" #'counsel-find-file)
(keymap-global-set "<f1> f" #'counsel-describe-function)
(keymap-global-set "<f1> v" #'counsel-describe-variable)
(keymap-global-set "<f1> o" #'counsel-describe-symbol)
(keymap-global-set "<f1> l" #'counsel-find-library)
(keymap-global-set "<f2> i" #'counsel-info-lookup-symbol)
(keymap-global-set "<f2> u" #'counsel-unicode-char)
(keymap-global-set "C-c g" #'counsel-git)
(keymap-global-set "C-c j" #'counsel-git-grep)
(keymap-global-set "C-c k" #'counsel-ag)
(keymap-global-set "C-x l" #'counsel-locate)
(keymap-global-set "C-S-o" #'counsel-rhythmbox)
(keymap-set minibuffer-local-map "C-r" #'counsel-minibuffer-history)
```

Note: parts of this config can be replaced by using `counsel-mode`.

# Counsel

[![Counsel badge from GNU-devel ELPA][counsel-dev-badge]][counsel-dev]
[![Counsel badge from GNU ELPA][counsel-badge]][counsel]
[![Counsel badge from MELPA][counsel-melpa-badge]][counsel-melpa]
[![Counsel badge from MELPA Stable][counsel-stable-badge]][counsel-stable]

`ivy-mode` ensures that any Emacs command invoking `completing-read`
or `completion-in-region` will use Ivy for completion.

Counsel takes this further, providing versions of common Emacs
commands that are customized to make the best use of Ivy.  For
example, `counsel-find-file` has some additional keybindings.
Pressing <kbd>DEL</kbd> will move you to the parent directory.

Enabling `counsel-mode` remaps built-in Emacs functions that have
Counsel replacements:

| Emacs command              | Counsel counterpart          |
| -------------------------- | ---------------------------- |
| `execute-extended-command` | `counsel-M-x`                |
| `describe-bindings`        | `counsel-descbinds`          |
| `describe-function`        | `counsel-describe-function`  |
| `describe-variable`        | `counsel-describe-variable`  |
| `describe-symbol`          | `counsel-describe-symbol`    |
| `apropos-command`          | `counsel-apropos`            |
| `describe-face`            | `counsel-describe-face`      |
| `list-faces-display`       | `counsel-faces`              |
| `find-file`                | `counsel-find-file`          |
| `find-library`             | `counsel-find-library`       |
| `imenu`                    | `counsel-imenu`              |
| `load-library`             | `counsel-load-library`       |
| `load-theme`               | `counsel-load-theme`         |
| `yank-pop`                 | `counsel-yank-pop`           |
| `info-lookup-symbol`       | `counsel-info-lookup-symbol` |
| `pop-to-mark-command`      | `counsel-mark-ring`          |
| `bookmark-jump`            | `counsel-bookmark`           |

## Installation

Install the `counsel` package from GNU ELPA or MELPA.

# Swiper

[![Swiper badge from GNU-devel ELPA][swiper-dev-badge]][swiper-dev]
[![Swiper badge from GNU ELPA][swiper-badge]][swiper]
[![Swiper badge from MELPA][swiper-melpa-badge]][swiper-melpa]
[![Swiper badge from MELPA Stable][swiper-stable-badge]][swiper-stable]

Swiper is an alternative to Isearch that uses Ivy to show an overview
of all matches.

A Helm version of Swiper is also available:
[`swiper-helm`][swiper-helm].

## Installation

Install the `swiper` package from GNU ELPA or MELPA.

## Screenshots

![Screenshot of Swiper in Emacs][swiper-shot]

There's also a ~ten minute [video demo][swiper-demo].

# Frequently asked questions

- Q: How do I enter an input that matches one of the candidates
  instead of this candidate?  Example: create a file `bar` when a file
  `barricade` exists in the current directory.

- A: Press <kbd>C-M-j</kbd>.  Alternatively, you can make the prompt
  line selectable with:
  ```elisp
  (setopt ivy-use-selectable-prompt t)
  ```

# Contributing

Please see the [guidelines](CONTRIBUTING.org) for reporting issues and
opening pull requests.

[ci]:
  https://github.com/abo-abo/swiper/actions/workflows/test.yml
  'CI build'
[ci-badge]:
  https://github.com/abo-abo/swiper/actions/workflows/test.yml/badge.svg
  'CI build status'

[ivy-dev]:
  https://elpa.gnu.org/devel/ivy.html
  'Ivy on GNU-devel ELPA'
[ivy-dev-badge]:
  https://elpa.gnu.org/devel/ivy.svg
  'Ivy version on GNU-devel ELPA'
[ivy]:
  https://elpa.gnu.org/packages/ivy.html
  'Ivy on GNU ELPA'
[ivy-badge]:
  https://elpa.gnu.org/packages/ivy.svg
  'Ivy version on GNU ELPA'
[ivy-melpa]:
  https://melpa.org/#/ivy
  'Ivy on MELPA'
[ivy-melpa-badge]:
  https://melpa.org/packages/ivy-badge.svg
  'Ivy version on MELPA'
[ivy-stable]:
  https://stable.melpa.org/#/ivy
  'Ivy on MELPA Stable'
[ivy-stable-badge]:
  https://stable.melpa.org/packages/ivy-badge.svg
  'Ivy version on MELPA Stable'
[ivy-manual]:
  https://elpa.gnu.org/packages/doc/ivy.html
  'Ivy manual on GNU ELPA'
[ivy-wiki]:
  https://github.com/abo-abo/swiper/wiki
  'Ivy wiki on GitHub'

[counsel-dev]:
  https://elpa.gnu.org/devel/counsel.html
  'Counsel on GNU-devel ELPA'
[counsel-dev-badge]:
  https://elpa.gnu.org/devel/counsel.svg
  'Counsel version on GNU-devel ELPA'
[counsel]:
  https://elpa.gnu.org/packages/counsel.html
  'Counsel on GNU ELPA'
[counsel-badge]:
  https://elpa.gnu.org/packages/counsel.svg
  'Counsel version on GNU ELPA'
[counsel-melpa]:
  https://melpa.org/#/counsel
  'Counsel on MELPA'
[counsel-melpa-badge]:
  https://melpa.org/packages/counsel-badge.svg
  'Counsel version on MELPA'
[counsel-stable]:
  https://stable.melpa.org/#/counsel
  'Counsel on MELPA Stable'
[counsel-stable-badge]:
  https://stable.melpa.org/packages/counsel-badge.svg
  'Counsel version on MELPA Stable'

[swiper-dev]:
  https://elpa.gnu.org/devel/swiper.html
  'Swiper on GNU-devel ELPA'
[swiper-dev-badge]:
  https://elpa.gnu.org/devel/swiper.svg
  'Swiper version on GNU-devel ELPA'
[swiper]:
  https://elpa.gnu.org/packages/swiper.html
  'Swiper on GNU ELPA'
[swiper-badge]:
  https://elpa.gnu.org/packages/swiper.svg
  'Swiper version on GNU ELPA'
[swiper-melpa]:
  https://melpa.org/#/swiper
  'Swiper on MELPA'
[swiper-melpa-badge]:
  https://melpa.org/packages/swiper-badge.svg
  'Swiper version on MELPA'
[swiper-stable]:
  https://stable.melpa.org/#/swiper
  'Swiper on MELPA Stable'
[swiper-stable-badge]:
  https://stable.melpa.org/packages/swiper-badge.svg
  'Swiper version on MELPA Stable'
[swiper-helm]:
  https://github.com/abo-abo/swiper-helm
  'Swiper-Helm on GitHub'
[swiper-shot]:
  swiper.svg
  'Swiper search for def and wip but not face'
[swiper-demo]:
  https://youtu.be/VvnJQpTFVDc
  'Swiper demo on YouTube'

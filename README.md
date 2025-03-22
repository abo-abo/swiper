[![Build Status](https://github.com/abo-abo/swiper/actions/workflows/test.yml/badge.svg)](https://github.com/abo-abo/swiper/actions/workflows/test.yml)

***flexible, simple tools for minibuffer completion in Emacs***

This repository contains:

**Ivy**, a generic completion mechanism for Emacs.

**Counsel**, a collection of Ivy-enhanced versions of common Emacs
commands.

**Swiper**, an Ivy-enhanced alternative to Isearch.

# Ivy

[![GNU-devel ELPA](https://elpa.gnu.org/devel/ivy.svg)](https://elpa.gnu.org/devel/ivy.html)
[![GNU ELPA](https://elpa.gnu.org/packages/ivy.svg)](https://elpa.gnu.org/packages/ivy.html)
[![MELPA](https://melpa.org/packages/ivy-badge.svg)](https://melpa.org/#/ivy)
[![MELPA Stable](https://stable.melpa.org/packages/ivy-badge.svg)](https://stable.melpa.org/#/ivy)

Ivy is a generic completion mechanism for Emacs.  While it operates
similarly to other completion schemes such as `icomplete-mode`, Ivy
aims to be more efficient, smaller, simpler, and smoother to use yet
highly customizable.

To try Ivy, just call <kbd>M-x</kbd> `ivy-mode`.  This will enable
generic Ivy completion, including specific completion for file and
buffer names.

### Installation

Install the `ivy` package from GNU ELPA or MELPA.

Users of Debian ≥10 (and derivatives such as Ubuntu ≥18.04) can
install Ivy, Counsel, and Swiper with `sudo apt install elpa-counsel`.
To add Hydra support: `sudo apt install elpa-ivy-hydra`.

## Documentation

### Manual

Installing `ivy` from GNU ELPA or MELPA also installs the manual under
the `(ivy)` Info node.

The source file for the Info page is [here](doc/ivy.org).

The manual is also available as [HTML](https://elpa.gnu.org/packages/doc/ivy.html).

### Wiki

Ivy and Swiper wiki [is here](https://github.com/abo-abo/swiper/wiki).

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

[![GNU-devel ELPA](https://elpa.gnu.org/devel/counsel.svg)](https://elpa.gnu.org/devel/counsel.html)
[![GNU ELPA](https://elpa.gnu.org/packages/counsel.svg)](https://elpa.gnu.org/packages/counsel.html)
[![MELPA](https://melpa.org/packages/counsel-badge.svg)](https://melpa.org/#/counsel)
[![MELPA Stable](https://stable.melpa.org/packages/counsel-badge.svg)](https://stable.melpa.org/#/counsel)

`ivy-mode` ensures that any Emacs command invoking `completing-read`
or `completion-in-region` will use Ivy for completion.

Counsel takes this further, providing versions of common Emacs
commands that are customized to make the best use of Ivy.  For
example, `counsel-find-file` has some additional keybindings.
Pressing <kbd>DEL</kbd> will move you to the parent directory.

Enabling `counsel-mode` remaps built-in Emacs functions that have
Counsel replacements:

| Emacs command              | Counsel counterpart          |
|----------------------------|------------------------------|
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

# Swiper

[![GNU-devel ELPA](https://elpa.gnu.org/devel/swiper.svg)](https://elpa.gnu.org/devel/swiper.html)
[![GNU ELPA](https://elpa.gnu.org/packages/swiper.svg)](https://elpa.gnu.org/packages/swiper.html)
[![MELPA](https://melpa.org/packages/swiper-badge.svg)](https://melpa.org/#/swiper)
[![MELPA Stable](https://stable.melpa.org/packages/swiper-badge.svg)](https://stable.melpa.org/#/swiper)

Swiper is an alternative to Isearch that uses Ivy to show an overview
of all matches.

![swiper.png](https://oremacs.com/download/swiper.png)

A Helm version of Swiper is also available:
[`swiper-helm`](https://github.com/abo-abo/swiper-helm).

## Screenshots

![ivy-swiper-1.png](https://oremacs.com/download/ivy-swiper-1.png)

There's also a ten minute [video demo](https://youtu.be/VvnJQpTFVDc).

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

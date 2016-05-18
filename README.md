[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/tonini/overseer.el.svg?branch=master)](https://travis-ci.org/tonini/overseer.el)

# overseer.el

> Ert-runner Integration Into Emacs

The [ert-runner](https://github.com/rejeep/ert-runner.el) is a great way to run your ert tests. Overseer integrates this tool into emacs and let's you using it inside your lovely editor.

***

- [Installation](#installation)
  - [ELPA](#installation-via-packageel)
  - [Via el-get](#via-el-get)
  - [Manual](#manual)
- [Usage](#usage)
  - [Interactive commands](#interactive-commands)
- [Contributing](#contributing)
- [License](#license)

## Installation

### Installation via package.el

`package.el` is the built-in package manager in Emacs.

Overseer is available on the three major community maintained repositories -
[MELPA STABLE](melpa-stable.milkbox.net), [MELPA](http://melpa.milkbox.net) and [Marmalade](https://marmalade-repo.org/).

You can install `Overseer` with the following commnad:

<kbd>M-x package-install [RET] overseer [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'overseer)
  (package-install 'overseer))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining Overseer, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

With the most recent builds of Emacs, you can pin Overseer to always
use MELPA Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(overseer . "melpa-stable") t)
```

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs. If you're an el-get
user just do <kbd>M-x el-get-install [RET] overseer [RET]</kbd>.

### Manual

You can install Overseer manually by placing it on your `load-path` and
`require` ing it. Many people favour the folder `~/.emacs.d/vendor`.

```el
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'overseer)
```

## Usage

The `ert-runner` will always run in the context of the current `emacs lisp project`.

The `overseer-mode` is enabled via `emacs-lisp-mode-hook` by default for emacs lisp test files. (`*-test.el`)

### Interactive Commands

The `overseer-mode` minor mode will be automaticly enable the following keybindings:

Keybinding           | Description
---------------------|---------------
<kbd>C-c , a</kbd>   | Runs `cask exec ert-runner`. `overseer-test`
<kbd>C-c , t</kbd>   | Runs `cask exec ert-runner -p <test-at-point>`. `overseer-test-run-test`
<kbd>C-c , b</kbd>   | Runs `cask exec ert-runner` with the current buffer file as argument. `overseer-test-this-buffer`
<kbd>C-c , f</kbd>   | Open a prompt to run `cask exec ert-runner` with a custom file as arguments. `overseer-test-file`
<kbd>C-c , g</kbd>   | Runs `cask exec ert-runner -t` with given tags (example: `indentation,syntax`). `overseer-test-this-buffer`
<kbd>C-c , p</kbd>   | Open a prompt to run `cask exec ert-runner` with custom arguments. `overseer-test-prompt`
<kbd>C-c , h</kbd>   | Runs `cask exec ert-runner --help`. `overseer-help`
<kbd>C-c , d</kbd>   | Runs `cask exec ert-runner --debug`. `overseer-help`
<kbd>C-c , v</kbd>   | Runs `cask exec ert-runner --verbose`. `overseer-help`
<kbd>C-c , q</kbd>   | Runs `cask exec ert-runner --quiet`. `overseer-help`

## Contributing

Contributions are very welcome!

1. Fork overseer.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

## License

Copyright © 2014-2015 Samuel Tonini and
[contributors](https://github.com/tonini/overseer.el/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?style=flat

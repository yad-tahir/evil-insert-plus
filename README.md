# 🖋️ evil-insert-plus

[![Evil](https://img.shields.io/badge/editor-emacs-7F5AB6.svg?logo=gnu-emacs&logoColor=white)](https://www.gnu.org/software/emacs/)

**evil-insert-plus** transforms the standard `evil-mode` insertion commands into **operators**.

In standard Vim/Evil, `i` and `a` are simple commands that act on the current cursor position. This package redefines them as evil operators, allowing you to combine them with the full power of Evil **motions** and **text objects**.

## Features

* **Grammatical Insertion**: Use patterns like `[prefix] i [motion]` (e.g., "Insert at start of paragraph").
* **Visual Block Support**: Intelligent line-counting ensures multi-line insertions (via Visual Block) work seamlessly.
* **Motion Awareness**: Automatically switches to line-insertion or character-insertion based on the motion type.
* **Repeatability**: Like delete and update operations. Repeat executing your insert and append commands.
* **Zero Dependencies**: Requires only `evil`.

## Installation

If you have cloned the repo locally to `~/.emacs.d/lisp/evil-insert-plus`:

### Using [use-package](https://github.com/jwiegley/use-package)

```elisp
(use-package evil-insert-plus
  :load-path "~/.emacs.d/lisp/evil-insert-plus"
  :bind (:map evil-normal-state-map
		 ("I" . evil-insert-plus)
		 ("A" . evil-append-plus)
		 :map evil-visual-state-map
		 ("I" . evil-insert-plus)
		 ("A" . evil-append-plus)))
```

### Manual Installation

```elisp
(add-to-list 'load-path "/path/to/evil-insert-plus")
(require 'evil-insert-plus)

;; Replace default I/A with operator versions in Normal and Visual states
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "I") 'evil-insert-plus)
  (define-key evil-normal-state-map (kbd "A") 'evil-append-plus)
  (define-key evil-visual-state-map (kbd "I") 'evil-insert-plus)
  (define-key evil-visual-state-map (kbd "A") 'evil-append-plus))
```

## Usage

By binding these operators to `I` and `A`, you retain standard Vim behavior through "double-tapping" while gaining the ability to target specific motions and text objects.

### The "Double-Tap" Logic

Because `I` and `A` are now operators, applying them to the current line (the default behavior of the original keys) is as simple as hitting the key twice or using line motions.

* **Standard Logic**: `I I` performs the traditional "Insert at start of line."
* **Operator Logic**: `I w` performs "Insert at start of current word."

### Example Commands

| Key Sequence | Description |
| :--- | :--- |
| `I b` | Jump to the **beginning** of the current word and insert. |
| `A e` | Jump to the **end** of the current word and insert. |
| `A i W` | Jump to the **end** of the current WORD and insert. |
| `I i (` | Jump to the inner start of the **parentheses** and insert. |
| `I i p` | Jump to the **start of the paragraph** and insert. |
| `A G` | Jump to the **end of of the buffer** and insert. |
| `A g n` | Insert to the **end of the next evil-ex match**. |
| `I g p` | Insert to the **start of the previous evil-ex match**. |

### Visual Block & Line Modes

This package is particularly powerful in **Visual Block mode** (`Ctrl-v`).

When you have a block selected:
1. Press `I` or `A`.
2. Type your text.

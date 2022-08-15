;;; tree-sitter-context.el --- Display context using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jake Ng

;; Author: Jake Ng
;; Keywords: tools languages
;; Version: 0
;; URL: https://github.com/jakejx/tree-sitter-context
;; Package-Requires: ((emacs "28.1") (tsc "0.18.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode (attempts) to replicate the functionality of Vim plugin
;; https://github.com/wellle/context.vim in Emacs by leveraging tree-sitter.

;;; TODO: support sibling nodes - for e.g. case statements in the same switch
;;; TODO: optimise the traversal of the tree - can we just reuse a cached value

;;; Code:

(require 'tree-sitter)
(require 'dash)

(defvar tree-sitter-context-significant-nodes-alist
  '((rust . (impl_item function_item if_expression for_expression))
    (go . (function_declaration method_declaration type_declaration if_statement for_statement expression_switch_statement)))
  "Mapping of language grammars to nodes that are considered significant.")

(defvar-local tree-sitter-context-significant--nodes
  nil)

(defvar-local tree-sitter-context--ov
  nil
  "Overlay for the current buffer.")

(defun tree-sitter-context--current-node ()
  "Get current tree sitter node of point."
  (when tree-sitter-tree
    (tree-sitter-node-at-pos)))

(defun tree-sitter-context--parents (node)
  "Get all the significant parents of the NODE."
  (->> (--unfold
        (when it
          (cons it (tsc-get-parent it))) node)
       (--filter
        (and (memq (tsc-node-type it) tree-sitter-context-significant--nodes) ;; significant node
             (< (tsc-node-start-position it) (window-start)))))) ;; node not visible in window

(defun tree-sitter-context--get-line (node)
  "Line of text to display for NODE."
  (let ((start (tsc-node-start-position node))
        (end (tsc-node-end-position node)))
    (save-excursion
      (goto-char start)
      (setq start (min start (point-at-bol)))
      (setq end (min end (point-at-eol))))
    (propertize (buffer-substring start end) 'face 'hl-line)))

(defun tree-sitter-context--create-ov ()
  "Create the overlay at `window-start'."
  (let ((ov (make-overlay (window-start) (window-start) nil t nil)))
    (overlay-put ov 'intangible t)
    ov))

(defun tree-sitter-context--get-ov ()
  "Get the buffer overlay or initialise it if nil."
  (when (not tree-sitter-context--ov)
    (setq tree-sitter-context--ov
          (tree-sitter-context--create-ov)))
  tree-sitter-context--ov)

(defun tree-sitter-context--update-ov-text (lines)
  "Update the text of the overlay with LINES."
  (let* ((combined (concat (string-join lines "\n") "\n")))
    (overlay-put (tree-sitter-context--get-ov) 'after-string combined)))

(defun tree-sitter-context--update-ov-pos ()
  "Reposition overlay at `window-start'."
  (move-overlay (tree-sitter-context--get-ov) (window-start) (window-start)))

(defun tree-sitter-context--update-ov ()
  "Main call that update the overlay based on the latest buffer state."
  (->> (tree-sitter-context--current-node)
       (tree-sitter-context--parents)
       (-map #'tree-sitter-context--get-line)
       (reverse)
       (tree-sitter-context--update-ov-text))
  (tree-sitter-context--update-ov-pos))

(defun tree-sitter-context--post-command (&optional command)
  "Trigger update after every COMMAND."
  (tree-sitter-context--update-ov))

(defun tree-sitter-context--pre-command ()
  "Remove the text in the overlay to prevent scrolling issues."
  (overlay-put tree-sitter-context--ov 'after-string ""))

(defun tree-sitter-context--setup ()
  "Setup tree-sitter-context."
  (let* ((lang (alist-get major-mode tree-sitter-major-mode-language-alist))
         (significant (alist-get lang tree-sitter-context-significant-nodes-alist)))
    (if significant
        (setq tree-sitter-context-significant--nodes significant)
      (error "%s not supported by tree-sitter-context" major-mode)))
  (progn
    (add-hook 'post-command-hook #'tree-sitter-context--post-command nil :local)
    (add-hook 'pre-command-hook #'tree-sitter-context--pre-command nil :local)))

(defun tree-sitter-context--teardown ()
  "Teardown tree-sitter-context."
  (progn
    (remove-hook 'post-command-hook #'tree-sitter-context--post-command :local)
    (remove-hook 'pre-command-hook #'tree-sitter-context--pre-command :local)
    (setq tree-sitter-context--ov nil)))

(define-minor-mode tree-sitter-context-mode
  "Show context of point using tree sitter."
  :init-value nil
  :group 'tree-sitter-context
  (tree-sitter--handle-dependent tree-sitter-context-mode
    #'tree-sitter-context--setup
    #'tree-sitter-context--teardown))

(provide 'tree-sitter-context)

;;; tree-sitter-context.el ends here

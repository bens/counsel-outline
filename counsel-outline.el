;;; counsel-outline -- Counsel interface for outlines -*- lexical-binding: t; -*-
;;
;; Author: Ben Sinclair <ben@typius.com>
;;      Eric Kaschalk <ekaschalk@gmail.com>
;;      John Ankarström <john.ankarstrom@gmail.com>
;; Maintainer: Ben Sinclair <ben@typius.com>
;; Version: 0.1
;; Package-Requires: (cl-lib dash ivy outshine)
;; Keywords: convenience, outlines
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Add a command to list outline headings in the current file to jump to them
;; easily, and optionally narrow the buffer to just the subtree of the selected
;; heading.
;;
;; This package is derived from John Ankarström's package outline-ivy-mirror[1],
;; which is a fork of Eric Kaschalk's outline-ivy[2] package.
;;
;; [1]: https://github.com/jocap/outline-ivy-mirror
;; [2]: https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/personal/local/outline-ivy/outline-ivy.el.

(require 'cl-lib)
(require 'dash)
(require 'ivy)
(require 'outshine)

;;; Code:
;;;; Config

(defvar counsel-outline-height 20
  "Number of outlines to display, overrides ivy-height.")

(defface counsel-outline-match-face
  '((t :height 1.10 :foreground "light gray" :background "black"))
  "Match face for counsel-outline prompt.")

(defface counsel-outline-face-1
  '((t :foreground "#C3A29E" :height 1.25 :underline t :weight ultra-bold))
  "Counsel-outline face for level 1.")

(defface counsel-outline-face-2
  '((t :foreground "#8D6B94" :height 1.15 :weight semi-bold))
  "Counsel-outline face for level 2.")

(defface counsel-outline-face-3
  '((t :foreground "#8C5f66"))
  "Counsel-outline face for level 3.")

;;;; Utils

(defun counsel-outline-rgx ()
  "Regex to match outlines with first group as its text."
  (cl-cadar outshine-imenu-preliminary-generic-expression))

(defun counsel-outline-format-name (STR LEVEL)
  "Format STR at LEVEL for ivy."
  (pcase LEVEL
    (2 (format " %s" STR))
    (3 (format "  %s" STR))
    (_ STR)))

(defun counsel-outline-format-name-pretty (STR PARENTS LEVEL)
  "Prepend invisible PARENTS to propertized STR at LEVEL."
  (concat (propertize
           (concat (when LEVEL (number-to-string LEVEL))
                   (apply 'concat PARENTS))
           'invisible t)
          (propertize (counsel-outline-format-name STR LEVEL)
                      'face (pcase LEVEL
                              (1 'counsel-outline-face-1)
                              (2 'counsel-outline-face-2)
                              (3 'counsel-outline-face-3)))))

(defun counsel-outline--collect-outline ()
  "Collect fontified outline string and marker for line at point."
  (save-excursion
    (beginning-of-line)
    (-let* ((level (outshine-calc-outline-level))
            (parents (when level
                       (--map (plist-get counsel-outline--parents-plist it)
                             (number-sequence 1 (- level 1)))))
            (str (match-string-no-properties 1))
            (name (counsel-outline-format-name-pretty str parents level)))
      (when level
        (setq counsel-outline--parents-plist (plist-put counsel-outline--parents-plist level str)))
      (->> (point-marker)
         (cons name)
         (when level)))))

(defun counsel-outline-collect-outlines ()
  "Collect fontified outline strings and their markers for ivy-read."
  (setq counsel-outline--parents-plist nil)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (cl-loop while (search-forward-regexp (counsel-outline-rgx) nil t)
             collect (counsel-outline--collect-outline))))

;;;; Outline Jump

(cl-defun counsel-outline--jump-to-marker ((_ . marker))
  "Move the cursor to the outline heading at MARKER."
  (with-ivy-window
    (outline-show-all)
    (-> marker marker-position goto-char)
    (recenter 2)))

(cl-defun counsel-outline--narrow-to-marker ((_ . marker))
  "Narrow the buffer to the subtree of the outline heading at MARKER."
  (with-ivy-window
    (outline-show-all)
    (-> marker marker-position goto-char)
    (outshine-narrow-to-subtree)))

(defun counsel-outline--preselect ()
  "Get parent outline at point for ivy :preselect."
  (save-excursion
    (unless (outline-on-heading-p t)
      (outline-previous-heading))
    (search-forward-regexp (counsel-outline-rgx) nil t)
    (beginning-of-line)
    (-> (match-string-no-properties 1)
       (counsel-outline-format-name (outshine-calc-outline-level)))))

(defun counsel-outline--remap-ivy-match-face ()
  "Overwrite ivy-current-match face in counsel-outline prompt."
  (set (make-local-variable 'face-remapping-alist)
       '((ivy-current-match counsel-outline-match-face))))

;;;###autoload
(defun counsel-outline-jump ()
  "Prompt fontified, hierarchal outlines for jump."
  (interactive)
  (set-mark (point))
  (deactivate-mark)
  (let ((ivy-height counsel-outline-height))
    (ivy-read "Outline " (counsel-outline-collect-outlines)
              :preselect (counsel-outline--preselect)
              :update-fn 'counsel-outline--remap-ivy-match-face
              :action '(1
                        ("o" counsel-outline--jump-to-marker "jump")
                        ("n" counsel-outline--narrow-to-marker "narrow")))))

(provide 'counsel-outline)
;;; counsel-outline.el ends here

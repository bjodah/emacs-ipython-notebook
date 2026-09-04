;;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; ein-completer.el --- Completion module

;; Copyright (C) 2018- Takafumi Arakaki / John Miller

;; Author: Takafumi Arakaki <aka.tkf at gmail.com> / John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-completer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-completer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-completer.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides completion-at-point function (CAPF) for Jupyter notebook code cells.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ein-core)
(require 'ein-log)
(require 'ein-kernel)
(require 'ein-cell)
(require 'ein-worksheet)

(make-obsolete-variable 'ein:complete-on-dot nil "0.15.0")
(make-obsolete-variable 'ein:completion-backend nil "0.17.0")

(defcustom ein:completion-timeout 1.0
  "Timeout in seconds to wait for completion replies from the kernel."
  :type 'number
  :group 'ein)

(defun ein:kernel-complete-sync (kernel code cursor-pos &optional timeout)
  "Synchronously request completions from KERNEL for CODE at CURSOR-POS.
Wait up to TIMEOUT seconds for a reply."
  (when (ein:kernel-live-p kernel)
    (let* ((timeout (or timeout ein:completion-timeout))
           (reply nil)
           (msg-id (ein:kernel-complete
                    kernel code cursor-pos
                    (list :complete_reply
                          (cons (lambda (_arg c _meta)
                                  (setq reply c))
                                nil)))))
      (when msg-id
        (with-timeout (timeout nil)
          (while (null reply)
            (accept-process-output nil 0.01)))
        reply))))

(defun ein:completion-at-point ()
  "Completion-at-point function for EIN notebook buffers."
  (when-let* ((kernel (ein:get-kernel))
              ((ein:kernel-live-p kernel)))
    (let (pos-min pos-max code cursor-pos base-pos)
      (if-let ((cell (ein:worksheet-at-codecell-p)))
          (progn
            (setq pos-min (ein:cell-input-pos-min cell)
                  pos-max (ein:cell-input-pos-max cell)
                  base-pos pos-min)
            (when (and pos-min pos-max (<= pos-min (point) pos-max))
              (setq code (buffer-substring-no-properties pos-min pos-max)
                    cursor-pos (- (point) pos-min))))
        (when (derived-mode-p 'prog-mode)
          (setq base-pos (point-min)
                code (buffer-substring-no-properties (point-min) (point-max))
                cursor-pos (- (point) (point-min)))))
      (when (and code cursor-pos)
        (when-let* ((res (ein:kernel-complete-sync kernel code cursor-pos))
                    ((equal (plist-get res :status) "ok"))
                    (matches (append (plist-get res :matches) nil)))
          (let* ((cstart (plist-get res :cursor_start))
                 (cend (plist-get res :cursor_end))
                 (beg (+ base-pos (or cstart cursor-pos)))
                 (end (+ base-pos (or cend cursor-pos)))
                 (metadata (plist-get res :metadata))
                 (experimental (plist-get metadata :_jupyter_types_experimental)))
            (list beg end matches
                  :annotation-function
                  (lambda (cand)
                    (when-let* ((match-info
                                 (seq-find (lambda (x)
                                             (string= (plist-get x :text) cand))
                                           experimental))
                                (type (plist-get match-info :type)))
                      (format " <%s>" type)))
                  :exclusive 'no)))))))

;;;###autoload
(defun ein:completer-complete ()
  "Trigger completion at point in EIN."
  (interactive)
  (completion-at-point))

(provide 'ein-completer)

;;; ein-completer.el ends here

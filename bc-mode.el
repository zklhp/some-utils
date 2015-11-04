;;; bc-mode.el --- Inferior mode for GNU bc

;; Copyright (C) 2015 Chris Zheng.

;; Author: Chris Zheng <chriszheng99@gmail.com>
;; Created: 2015-11-04
;; Version: 20151104
;; X-Original-Version: 0.1
;; Keywords: GNU bc, inferior mode, convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Using GNU bc in Emacs.

;;; Installation:

;; Make sure to place `bc-mode.el' somewhere in the load-path and
;; add `(require 'bc-mode)' to init file.

;;; Code:

(require 'comint)

(defgroup inferior-bc nil
  "Run bc in a buffer."
  :group 'inferior-bc)

(defcustom inferior-bc-program "bc"
  "Program invoked by `inferior-octave'."
  :type 'string)

(defcustom inferior-bc-buffer "*Inferior BC*"
  "Name of buffer for running an inferior bc process."
  :type 'string)

(defun remove--truncate-slash (string)
  (with-temp-buffer
    (insert string)
    (while (search-backward "\\
" nil t)
      (replace-match ""))
    (buffer-string)))

(defun run-bc (expr &optional replace)
  "Calculate the `expr' using GNU bc.
If called interactively, the user is asked for input.  If called on
region the selected expression is used as input.  By default display
output in temp buffer `*BC Output*'.  With prefix, insert the output."
  (interactive
   (list (if (use-region-p)
	     (buffer-substring (region-beginning) (region-end))
	   (read-string "Expression: "))
	 current-prefix-arg))
  (let ((buffer (get-buffer-create inferior-bc-buffer))
	(output)
	(try 30))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
	(make-comint (substring inferior-bc-buffer 1 -1)
		     inferior-bc-program)
	(set-process-filter (get-buffer-process buffer)
			    'comint-output-filter)
	(add-hook 'comint-preoutput-filter-functions
		  'remove--truncate-slash nil t)))
    (comint-send-string buffer expr)
    (comint-send-string buffer "\n")
    (while (or comint-last-output-start
	       (>= try 0))
      (sit-for 0.1)
      (setq try (1- try)))
    (when comint-last-output-start
      (setq output (with-current-buffer buffer
		     (goto-char comint-last-output-start)
		     (buffer-substring (point) (line-end-position))))
      (if (and replace output)
	  (when (use-region-p)
	    (delete-region (region-beginning) (region-end))
	    (insert output))
	(message "Result: %s" output)))))

(provide 'bc-mode)

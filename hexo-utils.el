;;; hexo-utils.el --- Functions for using Hexo under Emacs.

;; Copyright (C) 2015 Chris Zheng.

;; Author: Chris Zheng <chriszheng99@gmail.com>
;; Created: 2015-01-26
;; Version: 20150126
;; X-Original-Version: 0.1
;; Keywords: Hexo, utils

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

;; Two functions for Functions for using Hexo efficiently in Emacs.

;;; Installation:

;; Make sure to place `hexo-utils.el' somewhere in the load-path and
;; add `(require 'hexo-utils)' to init file.

;;; Code:

;;;###autoload (autoload 'hexo-deploy "hexo-utils")
(defun hexo-deploy (dir)
  "Deploy hexo site.
If called with a prefix argument, the user is asked for directory of
the site. Otherwise, the directory is guessed from
`default-directory'."
  (interactive (list (if current-prefix-arg
		   (file-name-as-directory
		    (read-directory-name "Hexo site directory: "
					 default-directory))
		 default-directory)))
  (let ((old-dir default-directory))
    (if (file-exists-p (concat dir "db.json"))
	(progn
	  (setq default-directory dir)
	  (start-process "hexo-deploy" "*Hexo Deploy Output*" "hexo" "d" "-g")
	  (setq default-directory old-dir))
      (progn
	(if (file-exists-p (expand-file-name
			    (concat old-dir "../../db.json")))
	    (progn
	      (setq default-directory (expand-file-name
			    (concat old-dir "../../")))
	      (start-process "hexo-deploy"
			     "*Hexo Deploy Output*" "hexo" "d" "-g")
	      (setq default-directory old-dir))
	  (error "Hexo deploy failed. Wrong directory?"))))))

(defun hexo-wait-and-visit (proc max)
  "Use timer to wait hexo process finish then visit the created file."
  (if (eq (process-status proc) 'exit)
      ;; Exit normally.
      (let ((hexo-buffer (process-buffer proc)))
	(set-buffer hexo-buffer)
	(goto-char (point-min))
	;; Compatible with Hexo v3.
	(if (search-forward "Created: " nil t 1)
	    (progn
	      (find-file (buffer-substring-no-properties (point) (line-end-position)))
	      (kill-buffer hexo-buffer))))
    (when (< max 5)
	(run-with-timer 6 nil `(lambda () (funcall 'hexo-wait-and-visit ,proc ,(1+ max)))))))

;;;###autoload (autoload 'hexo-new "hexo-utils")
(defun hexo-new (dir type title)
  "Create an hexo draft/page/photo/post/.
If called with a prefix argument, the user is asked for type of the
created item. By default, `tpye' is post."
  (interactive
   (list
    (cond
     ((file-exists-p (concat default-directory "db.json")) default-directory)
     ((file-exists-p (concat default-directory "../../db.json"))
      (expand-file-name (concat default-directory "../../")))
     (t (file-name-as-directory
	 (read-directory-name "Hexo site directory: "))))
    (if current-prefix-arg
	(read-string "Type to create: " "post" nil "post")
      "post")
    (read-string "Title: ")))
  (let ((old-dir default-directory))
    (if (string= title "")
	(error "Empty title.")
      (progn
	(setq default-directory dir)
	(hexo-wait-and-visit (start-process "hexo-new" "*Hexo New Output*" "hexo" "n" type title) 0)
	(setq default-directory old-dir)))))

(provide 'hexo-utils)

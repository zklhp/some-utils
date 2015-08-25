;;; astyle-utils.el --- Functions for using astyle in Emacs.

;; Copyright (C) 2015 Chris Zheng.

;; Author: Chris Zheng <chriszheng99@gmail.com>
;; Created: 2015-04-01
;; Version: 20150401
;; X-Original-Version: 0.1
;; Keywords: Astyle, utils

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

;; Functions for using astyle in Emacs.

;;; Installation:

;; Make sure to place `astyle-utils.el' somewhere in the load-path and
;; add `(require 'astyle-utils)' to init file.

;;; Code:

(defvar astyle-command "astyle --style=bsd -T8 -f -p -H -U -e")

;; The meaning of parameters are:

;; --indent=force-tab=#  OR  -T#
;; Indent using tab characters, assuming that each
;; indent is # spaces long. Force tabs to be used in areas
;; AStyle would prefer to use spaces.

;; --break-blocks  OR  -f
;; Insert empty lines around unrelated blocks, labels, classes, ...

;; --pad-oper  OR  -p
;; Insert space padding around operators.

;; --pad-header  OR  -H
;; Insert space padding after paren headers (e.g. 'if', 'for'...).

;; --unpad-paren  OR  -U
;; Remove unnecessary space padding around parenthesis. This
;; can be used in combination with the 'pad' options above.

;; --break-elseifs  OR  -e
;; Break 'else if()' statements into two different lines.

;;;###autoload
(defun astyle (start end)
  "Run astyle on region or buffer"
  (interactive (if mark-active
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))
		 ))
  (save-restriction
    (shell-command-on-region start end
			     astyle-command
			     (current-buffer) t
			     (get-buffer-create "*Astyle Errors*") t)))

(provide 'astyle-utils)

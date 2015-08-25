;;; SHELX-mode.el --- major mode to edit SHELX .ins and .res files

;; Copyright (C) 2014 Chris Zheng.

;; Author: Chris Zheng <chriszheng99@gmail.com>
;; Created: 2013-12-15
;; Version: 20131215
;; X-Original-Version: 0.1
;; Keywords: SHELX, faces, editing

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

;; Basic syntax highlighting for editing SHELX ins and res files.

;;; Installation:

;; Make sure to place `SHELX-mode.el' somewhere in the load-path and
;; add `(require 'SHELX-mode)' to init file.

;;; Code:

;; define several class of keywords
(defvar SHELX-general-keywords
  (mapcar 'symbol-name
          '(
	    ;; Crystal data and general instructions
	    TITL CELL ZERR LATT SYMM SFAC DISP UNIT LAUE
		 ;; REM
		 MORE TIME END
		 ;; Reflection data input
		 HKLF OMIT SHEL BASF TWIN EXTI SWAT HOPE MERG
		 ;; Least-squares organization
		 L.S.
		 CGLS BLOC DAMP STIR WGHT FVAR
		 ;; Lists and tables
		 BOND CONF MPLA RTAB HTAB LIST ACTA SIZE TEMP
		 WPDB
		 ;; Fouriers, peak search and lineprinter plots
		 FMAP GRID PLAN MOLE
		 )
	  )
  "SHELX general keywords.")

(defvar SHELX-refine-keywords
  (mapcar 'symbol-name
          '(
	    ;; Atom lists and least-squares constraints
	    SPEC RESI MOVE ANIS AFIX HFIX FRAG FEND EXYZ
		 EADP EQIV
		 ;; OMIT
		 ;; The connectivity list
		 CONN PART BIND FREE DFIX DANG BUMP SAME SADI
		 CHIV FLAT DELU SIMU DEFS ISOR NCSY SUMP
		 )
	  )
  "SHELX refine keywords.")

;; (regexp-opt (mapcar 'symbol-name
;; 		     '(H He
;; 			 Li Be B C N O F Ne
;; 			 Na Mg Al Si P S Cl Ar
;; 			 K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
;; 			 Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Tc I Xe
;; 			 Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Pu At Rn
;; 			 Fr Ra Ac Th Pa U Np Am Cm Bk Cf Es Fm Md No Lr Rf Db Bh Hs Mt Ds Rg Cn)
;; 		     ) 'words)

(defconst SHELX-elements-regexp "\\<\\(A[cglmr-u]\\|B[aehikr]\\|C[adefl-orsu]\\|D[bsy]\\|E[rsu]\\|F[emr]\\|G[ade]\\|H[efgos]\\|I[nr]\\|Kr\\|L[airu]\\|M[dgnot]\\|N[abdeiop]\\|Os\\|P[abdmrtu]\\|R[abe-hnu]\\|S[bceimnr]\\|T[abchilm]\\|Xe\\|Yb\\|Z[nr]\\|[BCFHIKNOPSUVWY][_\$0-9A-Z]*\\)\\>")

(defvar SHELX-general-keywords-regexp (regexp-opt SHELX-general-keywords 'words))
(defvar SHELX-refine-keywords-regexp (regexp-opt SHELX-refine-keywords 'words))

(setq SHELX-font-lock-keywords
      `(
	(,SHELX-general-keywords-regexp . font-lock-type-face)
	;; ("\\<\\(L\.S\.\\)\\>" . font-lock-type-face)
	(,SHELX-refine-keywords-regexp . font-lock-keyword-face)
	("\\<\\(REM\\).*" . font-lock-comment-face)
	(,SHELX-elements-regexp . font-lock-variable-name-face)
	))

;; (defvar SHELX-syntax-table nil "Syntax table for `SHELX-mode'.")
;; (setq SHELX-syntax-table
;;       (let ((synTable (make-syntax-table)))

;;         ;; bash style comment: “# …” 
;;         (modify-syntax-entry ?R "< b" synTable)
;;         (modify-syntax-entry ?\n "> b" synTable)

;;         synTable))

;; Open .ins and .res files with SHELX-mode

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ins\\'" . SHELX-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.res\\'" . SHELX-mode))

;;;###autoload
(define-derived-mode SHELX-mode prog-mode
  "SHELX mode"
  "Major mode for editing SHELX .ins and .res files"
  ;; :syntax-table SHELX-syntax-table
  
  (add-hook 'SHELX-mode-hook
	    '(lambda ()
	       (setq font-lock-keywords-case-fold-search t)
	       )
	    )
  (set (make-local-variable 'comment-start) "REM")
  
  ;; code for syntax highlighting
  (setq font-lock-defaults '((SHELX-font-lock-keywords)))
  )

;;;###autoload
(defun SH-adjust-AFIX ()
  "Add Missing AFIX 0."
  (interactive)
  (if (region-active-p)
      (save-restriction	; there is a text selection
	(narrow-to-region (region-beginning) (region-end))
	(goto-char (point-min))
	(while (re-search-forward "^\\(AFIX[ ]+[0-9]+\n\\)\\(?:^H.*\n\\)+\\(^PART[ ]+[0-9]+\n\\)" nil t)
	  (replace-match "\\&\\1"))))
  (progn
    (goto-char (point-min))
    (while (re-search-forward "^\\(AFIX[ ]+[0-9]+\n\\)\\(?:^H.*\n\\)+\\(^PART[ ]+[0-9]+\n\\)" nil t)
      (replace-match "\\&\\1"))))

(provide 'SHELX-mode)

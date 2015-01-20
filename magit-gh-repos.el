;;; magit-gh-repos.el --- Github repo management commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Michael Pontus

;; Author: Michael Pontus;;; magit-gh-repos.el --- Manage github repositories from magit  -*- lexical-binding: t; -*- <m.pontus@gmail.com>
;; Keywords: vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ewoc)
(require 'magit)
(require 'gh-repos)

(eval-when-compile
  (require 'names))

(define-derived-mode magit-gh-repos-mode magit-mode "Github Repos"
  "Major mode for displaying repository listing.")

(defun magit-gh-repos (&optional username switch-function)
  (interactive)
  (magit-mode-setup
   "*magit-gh-repos*" 
   (or switch-function magit-gh-repos-switch-function)
   'magit-gh-repos-mode
   'magit-gh-repos-load-next-page
   username))

(define-namespace magit-gh-repos- 
:dont-assume-function-quote
:package magit-gh-repos
:group magit-gh-repos

(defcustom list-format '((format "% -10s %s" language full-name) description) "")

(defcustom switch-function 'pop-to-buffer "")

(defun display-repo (repo)
  (magit-with-section (section section nil)
    (let ((bindings (mapcar (lambda (s) (cons s (slot-value repo s)))
                            (object-slots repo))))
      (dolist (line list-format)
        (insert (or (eval line bindings) "") ?\n)))))

(defun display-list (repos)
  (magit-with-section (section section nil)
    (let ((ewoc (ewoc-create #'magit-gh-repos-display-repo)))
      (dolist (repo repos) (ewoc-enter-last ewoc repo)))))

(defun load-next-page (username) 
  (let ((response (gh-repos-user-list 
                   (gh-repos-api "api" :sync nil)
                   username)))
    (display-list (oref response :data)))))

(provide 'magit-gh-repos)
;; Local Variables:
;; lisp-indent-function: lisp-indent-function
;; End:
;;; magit-gh-repos.el ends here

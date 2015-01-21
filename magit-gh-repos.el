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

(define-derived-mode magit-gh-repos-mode magit-mode "Github Repos")

(defun magit-gh-repos (&optional username switch-function)
  "Display listing of USERNAME's or your own repos."
  (interactive "MUsername: ")
  (magit-mode-setup "*magit-gh-repos*"
                    (or switch-function magit-gh-repos-switch-function)
                    'magit-gh-repos-mode 'magit-gh-repos-load-next-page 
                    (if (string= "" username) nil username)))

(define-namespace magit-gh-repos-
:dont-assume-function-quote
:package magit-gh-repos
:group magit-gh-repos

(defcustom list-format
  '((format "% -10s %s" language full-name) description)
  "Each form produces repo output line by being evaluated in it's context. ")

(defcustom switch-function 'pop-to-buffer
  "Function used to display buffer with repo listing.")

(defun load-next-page (username)
  (let ((response (gh-repos-user-list
                   (gh-repos-api "api") username)))
    (magit-with-section (section section nil)
      (let ((ewoc (ewoc-create
                   #'magit-gh-repos-pretty-printer nil nil 'nosep)))
        (dolist (repo (oref response :data))
          (ewoc-enter-last ewoc repo))))))

(defun pretty-printer (repo)
  (magit-with-section (section section nil)
    (let ((bindings (mapcar (lambda (s) (cons s (slot-value repo s)))
                            (object-slots repo))))
      (dolist (line list-format)
        (insert (or (eval line bindings) "") ?\n)))))

(defun create-repo (name)
  (interactive "MCreate new repo: ")
  (let* ((repo (gh-repos-repo "repo" :name name))
         (response (gh-repos-repo-new
                    (gh-repos-api "api") repo)))
    (oref response :data)))

(defun delete-repo (name)
  (interactive "MDelete repo: ")
  (let* ((response (gh-repos-repo-get (gh-repos-api "api") name))
         (repo-url  (oref (oref response :data) :clone-url)))
    (let ((response (gh-repos-repo-delete (gh-repos-api "api") name)))
      (when (= 204 (oref response :http-status))
        (let ((remote (get-remotes repo-url)))
          (and remote (magit-remove-remote (car remote))))))))

(defun get-remotes (&optional url)
  "When URL is specified returns matching alist entry.
Otherwise returns alist (REMOTE . URL) of all remotes in current repo."
  (let* ((lines (magit-git-lines "remote" "-v"))
         (chunks (mapcar 'split-string lines))
         (alist (mapcar 'butlast chunks)))
    (mapc (lambda (c) (setcdr c (cadr c)))
          (delete-duplicates alist))
    (if url (rassoc url alist) alist)))

(defun remote-add (name)
  (interactive "MAdd remote to repo: ")
  (let* ((response (gh-repos-repo-get 
                    (gh-repos-api "api") name))
         (repo (oref response :data))
         (url (oref repo :clone-url)))
    (magit-add-remote "origin" url))))

(provide 'magit-gh-repos)
;;; magit-gh-repos.el ends here

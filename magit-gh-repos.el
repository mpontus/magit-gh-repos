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

(defconst api (gh-repos-api "*api*"))

(defcustom delete-remotes nil
  "Should we delete remotes left after deleting repo.")

(defcustom switch-function 'pop-to-buffer
  "Function used to display buffer with repo listing.")

(defcustom formatters '(full-name desription last-updated)
  "Each form produces descendant section in repo output. ")

(defun load-next-page (username)
  (let ((response (gh-repos-user-list api username)))
    (draw-page (oref response :data))))

(defun draw-page (entries)
  (let* ((printer #'magit-gh-repos-draw-entry)
         (ewoc (ewoc-create printer nil nil 'nosep)))
    (mapc (apply-partially 'ewoc-enter-last ewoc) entries)))

(defun draw-entry (entry)
  (let ((context (mapcar (lambda (s) (cons s (slot-value entry s)))
                         (object-slots entry)))) 
    (dolist (form formatters)
      (magit-with-section (section section)
        (insert (eval form context) ?\n)))))

(defun create-repo (name)
  (interactive "MCreate new repo: ")
  (let* ((repo (gh-repos-repo "repo" :name name))
         (response (gh-repos-repo-new
                    (gh-repos-api "api") repo)))
    (oref response :data)))

(defun delete-repo (name)
  (interactive "MDelete repo: ")
  (let* ((response (gh-repos-repo-get (gh-repos-api "api") name))
         (repo-url  (oref (oref response :data) :clone-url))
         (response (gh-repos-repo-delete (gh-repos-api "api") name)))
    (unless (= 204 (oref response :http-status))
      (user-error "There was a problem deleting the repo"))
    (when delete-remotes 
      (let ((remote (get-remotes repo-url)))
        (when remote 
          (magit-remove-remote (car remote)))))))

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

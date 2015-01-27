;;; magit-gh-repos.el --- Github repo management commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Michael Pontus

;; Author: Michael Pontus <m.pontus@gmail.com>
;; Keywords:
;; Version: 0.1
;; Package-Requires: ((magit "1.2.2") (gh "0.8.2") (names 0.5"))

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

;; 

;;; Code:

(require 'ewoc)
(require 'magit)
(require 'gh-repos)
(eval-when-compile
  (require 'names))

(define-namespace magit-gh-repos-
:package magit-gh-repos
:group magit-gh-repos

(defconst api (gh-repos-api "*api*"))

(defcustom formatters '(full-name desription last-updated)
  "Each form produces descendant section in repo output. ")

(defun display-list (items &optional title)
  (let ((ewoc (ewoc-create #'display-item nil nil 'nosep)))
    (magit-with-section (section section nil title)
      (mapc (apply-partially 'ewoc-enter-last ewoc) items))))

(defun display-item (entry)
  (let ((context (mapcar (lambda (s) (cons s (slot-value entry s)))
                         (object-slots entry)))) 
    (dolist (form formatters)
      (magit-with-section (section section)
        (insert (eval form context) ?\n)))))

(defcustom user-repos-buffer-name "%s's repos"
  "Format for `magit-gh-repos-user-repos' buffer name.")

(defcustom user-repos-switch-function 'pop-to-buffer
  "Function for `magit-gh-repos-user-repos' to use for switching buffers.")

(defun user-repos (username &optional switch-function)
  (magit-mode-setup 
   (format magit-gh-repos-user-repos-buffer-name username)
   (or switch-function magit-gh-repos-user-repos-switch-function)
   'magit-mode (lambda (username) 
                 (magit-gh-repos-display-list
                  (oref (gh-repos-user-list magit-gh-repos-api username) :data)
                  (format "%s's repos" username))) username))

(defcustom forks-list-buffer-name "%s's repos"
  "Format for `magit-gh-repos-forks-list' buffer name.")

(defcustom forks-list-switch-function 'pop-to-buffer
  "Function for `magit-gh-repos-forks-list' to use for switching buffers.")

(defun forks-list (repo-name &optional recursive switch-function)
  (let ((repo (get-repo repo-name))) 
    (magit-mode-setup 
     (format magit-gh-repos-forks-list-buffer-name (oref repo full-name))
     (or switch-function magit-gh-repos-forks-list-switch-function)
     'magit-mode (lambda (repo) 
                   (magit-gh-repos-display-list
                    (oref (gh-repos-forks-list magit-gh-repos-api repo) :data)
                    (format "%s's forks" (oref repo full-name)))) repo)))

;; Basic Commands

(defun get-remotes (&optional url)
  "When URL is specified returns matching alist entry.
Otherwise returns alist (REMOTE . URL) of all remotes in current repo."
  (noflet ((magit-gh-repos-draw-entry (entry)))
    (let* ((lines (magit-git-lines "remote" "-v"))
           (chunks (mapcar 'split-string lines))
           (alist (mapcar 'butlast chunks)))
      (mapc (lambda (c) (setcdr c (cadr c)))
            (delete-duplicates alist))
      (if url (rassoc url alist) alist))))

(defun add-remote (name)
  (interactive "MAdd remote to repo: ") ;
  (let* ((repo (get-repo name))
         (url (oref repo :clone-url))
         (remotes (get-remotes))
         (remote "origin"))
    (unless (rassoc url remotes)
      (when (assoc remote remotes)
        (setq remote (magit-completing-read
                      "Use remote" remotes)))
      (magit-add-remote remote url))))

(defun get-repo (name)
  (if (stringp name)
      (let ((resp (gh-repos-repo-get api name)))
        (oref resp :data)) 
      name))

(defun create-repo (name)
  (interactive "MCreate new repo: ")
  (let* ((repo (gh-repos-repo-stub "repo" :name name))
         (resp (gh-repos-repo-new api repo)))
    (unless (= 201 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (add-remote (oref resp :data))))

(defun delete-repo (name)
  (interactive "MDelete repo: ")
  (let* ((repo (get-repo name))
         (resp (gh-repos-repo-delete api (oref repo :name))))
    (unless (= 204 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (let ((remote (get-remotes (oref repo :clone-url))))
      (when remote (magit-remove-remote (car remote))))))

(defun fork-repo (name &optional org)
  (let* ((repo (get-repo name))
         (resp (gh-repos-fork api repo)))
    (unless (= 202 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (add-remote (oref resp :data))))



)

(provide 'magit-gh-repos)


;;; magit-gh-repos.el ends here

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

(defcustom formatters 
  '((format "name: %s" (or name "(empty)"))
    (format "description: %s" (or description "(empty)"))
    (format "homepage: %s" (or homepage "(empty)"))
    (format "private: %s" (or private "(empty)"))
    (format "url: %s" (or url "(empty)"))
    (format "html-url: %s" (or html-url "(empty)"))
    (format "clone-url: %s" (or clone-url "(empty)"))
    (format "git-url: %s" (or git-url "(empty)"))
    (format "ssh-url: %s" (or ssh-url "(empty)"))
    (format "svn-url: %s" (or svn-url "(empty)"))
    (format "mirror-url: %s" (or mirror-url "(empty)"))
    (format "owner: %s" (or owner "(empty)"))
    (format "id: %s" (or id "(empty)"))
    (format "full-name: %s" (or full-name "(empty)"))
    (format "language: %s" (or language "(empty)"))
    (format "fork: %s" (or fork "(empty)"))
    (format "forks: %s" (or forks "(empty)"))
    (format "forks-count: %s" (or forks-count "(empty)"))
    (format "watchers: %s" (or watchers "(empty)"))
    (format "watchers-count: %s" (or watchers-count "(empty)"))
    (format "size: %s" (or size "(empty)"))
    (format "master-branch: %s" (or master-branch "(empty)"))
    (format "open-issues: %s" (or open-issues "(empty)"))
    (format "pushed-at: %s" (or pushed-at "(empty)"))
    (format "created-at: %s" (or created-at "(empty)"))
    (format "updated-at: %s" (or updated-at "(empty)"))
    (format "organisation: %s" (or organisation "(empty)"))
    (format "parent: %s" (or parent "(empty)"))
    (format "source: %s" (or source "(empty)"))
    (format "has-issues: %s" (or has-issues "(empty)"))
    (format "has-wiki: %s" (or has-wiki "(empty)"))
    (format "has-downloads: %s" (or has-downloads "(empty)"))) 
  "Each form produces descendant section in repo output. ")

(defun display-list (items &optional title)
  (let ((ewoc (ewoc-create #'display-item nil nil 'nosep)))
    (magit-with-section (section section nil title)
      (mapc (apply-partially 'ewoc-enter-last ewoc) items))))

(defun display-item (entry)
  (let ((context (mapcar (lambda (s) (cons s (slot-value entry s)))
                         (object-slots entry)))) 
    (magit-with-section (section section)
      (dolist (form formatters)
        (magit-with-section (section section)
          (insert (eval form context) ?\n))))))

(defcustom user-repos-buffer-name "%s's repos"
  "Format for `magit-gh-repos-user-repos' buffer name.")

(defcustom user-repos-switch-function 'pop-to-buffer
  "Function for `magit-gh-repos-user-repos' to use for switching buffers.")

(defun user-repos (username &optional switch-function)
  (interactive "MForks for user: ")
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
  (interactive "MForks for repo: ")
  (let ((repo (get-repo-by-name repo-name))) 
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
  "Add a repo referenced by NAME as remote."
  (interactive "MAdd remote to repo: ") ;
  (add-remote-to-repo (get-repo-by-name name)))

(defun add-remote-to-repo (repo)
  ;; Extracted so the commands can bypass name resolution
  (let* ((url (oref repo :clone-url))
         (remotes (get-remotes))
         (remote "origin"))
    (unless (rassoc url remotes)
      (when (assoc remote remotes)
        (setq remote (magit-completing-read
                      "Use remote" remotes)))
      (magit-add-remote remote url))))

(defun get-repo-by-name (name)
  (let ((resp (gh-repos-repo-get api name)))
    (unless (= 200 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (oref resp :data)))

(defun create-repo (name)
  (interactive "MCreate new repo: ")
  (let* ((repo (gh-repos-repo-stub "repo" :name name))
         (resp (gh-repos-repo-new api repo)))
    (unless (= 201 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (add-remote-to-repo (oref resp :data))))

(defun delete-repo (name)
  (interactive "MDelete repo: ")
  (let* ((repo (get-repo-by-name name))
         (resp (gh-repos-repo-delete api (oref repo :name))))
    (unless (= 204 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (let ((remote (get-remotes (oref repo :clone-url))))
      (when remote (magit-remove-remote (car remote))))))

(defun fork-repo (name &optional org)
  (interactive "MFork repo: ")
  (let* ((repo (get-repo-by-name name))
         (resp (gh-repos-fork api repo)))
    (unless (= 202 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (add-remote-to-repo (oref resp :data))))

(defun rename-repo (name new-name)
  (interactive "MRename repo: ")
  (let* ((repo (get-repo-by-name name))
         (resp (gh-repos-repo-rename api repo new-name)))
    (unless (= 200 (oref resp :http-status))
      (error (cdr (assq 'status-string (oref resp :headers)))))
    (let ((remote (get-remotes (oref repo :clone-url))))
      (when remote (magit-git-success
                    (list "remote" "set-url" (car remote) 
                          (oref (oref resp :data) :clone-url))))))))

(provide 'magit-gh-repos)


;;; magit-gh-repos.el ends here

;;; magit-gh-repos.el --- Github repo management commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Michael Pontus

;; Author: Michael Pontus <m.pontus@gmail.com>
;; Keywords:
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (magit "1.2.2") (gh "0.8.2") (names "0.5"))

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

(require 'magit)
(require 'gh-repos)

(eval-when-compile
  (require 'names))

(define-namespace magit-gh-repos-
:assume-var-quote
:package magit-gh-repos
:group magit-gh-repos

(defconst api (gh-repos-api "*api*"))

(defcustom url-slot :ssh-url
  "Which URL slot to use for adding remotes."
  :type '(radio (const :tag "HTTPS" :clone-url)
          (const :tag "GIT" :git-url)
          (const :tag "SSH" :ssh-url)))

(defvar read-repo-name-history nil)

(defun get-all-remotes ()
  "Return alist of all remotes in current repo in form (REMOTE . URL)."
  (let* ((lines (magit-git-lines "remote" "-v"))
         (chunks (mapcar 'split-string lines))
         (alist (mapcar 'butlast chunks)))
    (mapcar (lambda (c) (cons (car c) (cadr c))) alist)))

(defun add-remote-to-repo (repo-or-name &optional top-dir)
  (interactive (list (read-repo-name "Add remote to repo") (read-top-dir))) ;
  (let* ((repo (if (gh-repos-repo-p repo-or-name) repo-or-name
                 (get-repo-by-name repo-or-name)))
         (url (slot-value repo url-slot))
         (default-directory (or (when top-dir
                                  (magit-get-top-dir top-dir))
                                default-directory))
         (remotes (get-all-remotes))
         (remote "origin"))
    (unless (rassoc url remotes)
      (when (assoc remote remotes)
        (setq remote (magit-completing-read
                      "Use remote" remotes)))
      (magit-add-remote remote url))))

(defun get-repo-by-name (name)
  (let ((resp (gh-repos-repo-get api name)))
    (unless (= 200 (oref resp :http-status))
      (user-error (cdr (assq 'status-string (oref resp :headers)))))
    (oref resp :data)))

(defun read-repo-name (prompt)
  (magit-completing-read prompt nil nil nil nil 'read-repo-name-history))

(defun read-top-dir ()
  (if current-prefix-arg
      (let ((dir (magit-read-top-dir
                  (> (prefix-numeric-value current-prefix-arg)
                     4))))
        (when (and (not (magit-get-top-dir dir))
                   (yes-or-no-p
                    (format "There is no Git repository in %s.  Create one? "
                            dir))) (magit-init dir)) dir)
      (magit-get-top-dir)))

(defun create-repo (name &optional top-dir)
  (interactive (list (read-repo-name "Create new repo") (read-top-dir)))
  (let* ((repo (gh-repos-repo-stub "repo" :name name))
         (resp (gh-repos-repo-new api repo)))
    (unless (= 201 (oref resp :http-status))
      (user-error (cdr (assq 'status-string (oref resp :headers)))))
    (when (and top-dir (magit-get-top-dir top-dir))
      (add-remote-to-repo (oref resp :data) top-dir))))

(defun delete-repo (name &optional top-dir)
  (interactive (list (read-repo-name "Delete repo") (read-top-dir)))
  (let* ((repo (get-repo-by-name name))
         (resp (gh-repos-repo-delete api (oref repo :name))))
    (unless (= 204 (oref resp :http-status))
      (user-error (cdr (assq 'status-string (oref resp :headers)))))
    (when (and top-dir (magit-get-top-dir top-dir))
      (let* ((default-directory (magit-get-top-dir top-dir))
             (remote (rassoc (slot-value repo url-slot) (get-all-remotes))))
        (when remote (magit-remove-remote (car remote)))))))

(defun fork-repo (name &optional top-dir)
  (interactive (list (read-repo-name "Fork repo") (read-top-dir)))
  (let* ((repo (get-repo-by-name name))
         (resp (gh-repos-fork api repo)))
    (unless (= 202 (oref resp :http-status))
      (user-error (cdr (assq 'status-string (oref resp :headers)))))
    (when (and top-dir (magit-get-top-dir top-dir))
      (let* ((default-directory (magit-get-top-dir top-dir)))
        (add-remote-to-repo (oref resp :data) top-dir)))))

(defun rename-repo (name new-name &optional top-dir)
  (interactive
   (let* ((name (read-repo-name "Rename repo"))
          (new-name
           (read-string (format "New name (default %s): " name) 
                        name 'read-repo-name-history name)))
     (list name new-name (read-top-dir))))
  (unless (string= name new-name)
    (let* ((repo (get-repo-by-name name))
           (resp (gh-repos-repo-rename api repo new-name)))
      (unless (= 200 (oref resp :http-status))
        (user-error (cdr (assq 'status-string (oref resp :headers)))))
      (when (and top-dir (magit-get-top-dir top-dir))
        (let* ((default-directory (magit-get-top-dir top-dir))
               (remote (rassoc (slot-value repo url-slot) (get-all-remotes)))
               (url (slot-value (oref resp :data) url-slot)))
          (when remote (magit-git-success
                        (list "remote" "set-url" (car remote) url)))))))))

(provide 'magit-gh-repos)


;;; magit-gh-repos.el ends here

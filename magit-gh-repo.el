;;; magit-gh-repo.el --- Manage github repositories from magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2014

;; Author:  <michael@localhost>
;; Keywords: vc, extensions, convenience

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

;; Publish your repositories on github using magit interface.

;;; Code:


(require 'dash)
(require 'gh-repos)
(eval-when-compile
  (require 'names))

(define-namespace magit-gh-repo-

(defun api ()
  (gh-repos-api "api"))

(defun remotes (topdir)
  (let ((default-directory topdir))
    (magit-git-lines "remote")))

(defun remote-url (remote)
  (magit-get "remote" remote "url"))

(defun repos ()
  (oref (gh-repos-user-list (api)) :data))

(defun repo-url (repo)
  (oref repo :ssh-url))

(defun repo-name (repo)
  (oref repo :name))

(defun remote-repos (topdir)
  "Returns the remotes which correspond to one of user's repos."
  (let ((-compare-fn (lambda (repo remote)
                       (equal (remote-url remote) (repo-url repo)))))
    (-intersection (repos) (remotes topdir))))

(defun guess-remote (topdir repo-name)
  "Returns the remote name correspoding to repo-name."
  (let ((-compare-fn (lambda (remote repo)
                       (and (equal (oref repo :name) repo-name)
                            (equal (remote-url remote) (repo-url repo))))))
    (car (-intersection (remotes topdir) (repos)))))

(defun ask-topdir ()
  "Returns the top git directory and creates it if neccessary."
  (or (and (not current-prefix-arg) (magit-get-top-dir))
      (let* ((dir (magit-read-top-dir (> (prefix-numeric-value
                                          current-prefix-arg) 4)))
             (topdir (magit-get-top-dir dir)))
        (or topdir
            (when (yes-or-no-p
                   (format "There is no Git repository in %s. Create one? "
                           dir))
              (magit-init dir)
              (magit-get-top-dir dir))))))

(defun ask-new-remote (topdir)
  "Ask user for new remote name."
  (let ((remote "origin"))
    (while (member remote (remotes topdir))
      (setq remote
            (read-string
             (format "Remote %S already exists. Use remote: "
                     remote))))
    remote))

(defun guess-repo-name (topdir)
  (file-name-nondirectory (directory-file-name topdir)))

(defun ask-new-repo-name (topdir)
  (let ((repo-name (guess-repo-name topdir)))
    (while (member repo-name (mapcar 'magit-gh-repo-repo-name (repos)))
      (setq repo-name
            (read-string
             (format "Repo %S already exists. Use repo name: " repo-name))))
    repo-name))

(defun ask-existing-repo-name (prompt &optional topdir)
  (let ((names (mapcar 'magit-gh-repo-repo-name (repos)))
        (remote-repo-names (mapcar 'magit-gh-repo-repo-name
                              (and topdir (remote-repos topdir))))
        (repo-name (and topdir (guess-repo-name topdir))))
    (completing-read prompt names nil 'require-match
                     (cond ((and repo-name
                                 (member repo-name remote-repo-names))
                            repo-name)
                           (remote-repo-names (car remote-repo-names))))))

(defun create (topdir &optional remote repo-name)
  (interactive (let* ((topdir (ask-topdir))
                      (remote (ask-new-remote topdir))
                      (repo-name (ask-new-repo-name topdir)))
                 (list topdir remote repo-name)))
  (let ((default-directory topdir)
        (remote (or remote "origin"))
        (repo-name (or repo-name (guess-repo-name topdir))))
    (when (member remote (remotes topdir))
      (error "Remote %S already exists." remote))
    (let* ((repo-stub (gh-repos-repo-stub "repo" :name repo-name))
           (response (gh-repos-repo-new (api) repo-stub))
           (url (oref (oref response :data) :ssh-url)))
      (unless url (error "Repo with id %S already exists." repo-name))
      (magit-run-git "remote" "add" "-f" remote url)
      (magit-push))))

(defun delete (repo-name &optional topdir remote)
  (interactive (let* ((topdir (if (not current-prefix-arg) (magit-get-top-dir)
                                (magit-read-top-dir (> (prefix-numeric-value
                                                        current-prefix-arg) 4))))
                      (repo-name (ask-existing-repo-name "Delete repo:" topdir))
                      (remote (and topdir (guess-remote topdir repo-name))))
                 (list repo-name topdir remote)))
  (ignore-errors
    (gh-repos-repo-delete (api) repo-name))
  (when (and topdir remote)
    (let ((default-directory topdir))
      (magit-remove-remote remote)))))

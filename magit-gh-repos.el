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

;; 

;;; Code:

(require 'magit)
(require 'gh-repos)
(eval-when-compile
  (require 'names))

(define-namespace magit-gh-repos- :group magit-gh-repos

(defconst api (gh-repos-api "API" :cache t))

(defun get-repos ()
	(oref (gh-repos-user-list api) :data))

(defun read-repo (prompt &optional default)
	(let* ((prompt (if (not default) (concat prompt ": ")
										 (format "%s (%s): " prompt default)))
				 (alist (mapcar (lambda (repo) (cons (oref repo :name) repo))
									(oref (gh-repos-user-list api) :data)))
				 (answer (completing-read prompt alist nil nil nil nil default)))
		(or (cdr (assoc answer alist)) answer)))

:autoload
(defun create (repo-name)
	"Create new github repo named REPO-NAME."
  (interactive (list (read-string "Create new repo: "
																	(file-name-nondirectory
																	 (directory-file-name
																		(or (magit-get-top-dir)
																				default-directory))))))
  (let* ((repo-stub (gh-repos-repo-stub "repo" :name repo-name))
				 (response (gh-repos-repo-new api repo-stub))
				 (repo (oref response :data)))
		(if (<= 300 (oref response :http-status))
				(error (cdr (assq 'status-string
													(oref response :headers))))
				(remote-add (oref response :data)))))

:autoload
(defun rename (repo new-name)
	"Rename REPO to NEW-NAME."
  (interactive
	 (list (let* ((repo (read-repo "Rename repo"))
								(new-name (read-string "New name: " (oref repo :name))))
					 (list repo new-name))))
	(let ((response (gh-repos-repo-rename api repo new-name)))
		(if (<= 300 (oref response :http-status))
				(error (cdr (assq 'status-string (oref response :headers))))
				(remote-switch repo (oref response :data)))))

:autoload
(defun delete (repo)
	"Rename REPO to NEW-NAME."
	(interactive (list (read-repo "Delete repo")))
	(let ((response (gh-repos-repo-delete api (oref repo :name))))
		(if (<= 300 (oref response :http-status))
				(error (cdr (assq 'status-string
													(oref response :headers))))
				(remote-remove repo))))

(defun get-remotes ()
	(mapcar (lambda (line) (cons (car line) (cadr line)))
		(mapcar 'split-string (magit-git-lines "remote" "-v"))))

:autoload
(defun remote-add (repo)
	"Add REPO as a remote to current project."
	(interactive (list (read-repo "Add remote for repo")))
	(let ((remote-name (if (or current-prefix-arg
														 (assoc "origin" (get-remotes)))
												 (read-string "Remote name:")
											 "origin")))
		(magit-add-remote remote-name (oref repo :clone-url))))

:autoload
(defun remote-switch (repo new-repo)
	"Substitute remote url for REPO with NEW-REPO."
	(interactive (list (read-repo "Old repo") (read-repo "New repo")))
	(let ((remote-name "origin"))
		(let ((remote (rassoc (oref repo :clone-url) (get-remotes))))
			(when remote (magit-run-git "remote" "set-url" (car remote) 
																	(oref new-repo :clone-uirl))))))

:autoload
(defun remote-remove (repo)
	"Delete remote tracking REPO."
	(interactive (list (read-repo "Delete remote for repo")))
	(let ((remote (rassoc (oref repo :clone-url) (get-remotes))))
		(when remote (magit-remove-remote (car remote))))))


(provide 'magit-gh-repos)


;; Local Variables:
;; lisp-indent-function: lisp-indent-function
;; End:
;;; magit-gh-repos.el ends here

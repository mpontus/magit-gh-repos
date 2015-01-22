(require 'ert)
(require 'noflet)
(require 'magit-gh-repos)


(ert-deftest tests-magit-gh-repos/ewoc ()
  "Should create new EWOC."
  (noflet ((ewoc-create (&rest args) (throw 'ok nil))
           (gh-repos-user-list (&rest args) (test-response))) 
    (catch 'ok (magit-gh-repos-load-next-page "foo")
           (throw 'fail "Did not create new EWOC.")))
  (noflet ((ewoc-enter-last (&rest args) (throw 'ok nil))
           (gh-repos-user-list (&rest args) (test-response))) 
    (catch 'ok (magit-gh-repos-draw-page '("bar"))
           ;; (magit-gh-repos-load-next-page "bar")
           (throw 'fail "EWOC haven't received any entries!")))) ;

(ert-deftest tests-magit-gh-repos/configurable ()
  "Should insert result of evaluation as section's content."
  (let ((magit-gh-repos-formatters 
         '((format "%-4s%s" language name)))) 
    (magit-gh-repos-draw-entry
     (gh-repos-repo "repo" :name "foo" :language "bar"
                    :description "baz"))
    (let ((string (buffer-substring-no-properties
                   (point-min) (point-max))))
      (should (equal "bar foo\n" string)))))  

(ert-deftest tests-magit-gh-repos/magit-setup ()
  "Should let magit to set up the buffer."
  (noflet ((magit-mode-init (&rest args)
             (throw 'ok nil)))
    (catch 'ok (magit-gh-repos)
           (throw 'fail "Did not call `magit-mode-init'"))))

(ert-deftest tests-magit-gh-repos/switch-function ()
  "Should use magit config and allow passing as argument."

  (noflet ((show-buffer-fn (x &rest args) (throw 'ok nil)))
    (catch 'ok (magit-gh-repos nil 'show-buffer-fn)
           (throw 'fail "did not call specified switch-function")))
  
  (noflet ((another-fn (x &rest args) (throw 'ok nil)))
    (let ((magit-gh-repos-switch-function 'another-fn))
      (catch 'ok (magit-gh-repos nil 'another-fn)
             (throw 'fail "Did not call configured switch-function")))))

(ert-deftest test-magit-gh-repos/username-query ()
  "Should pass optional parameters to `gh-repos-user-list'."
  (noflet ((gh-repos-user-list (a u &rest args)
             (cond ((equal u "foobar") (throw 'ok nil))
                   (t (throw 
                          "Requested repos for wrong username.")))))
    (catch 'ok (magit-gh-repos "foobar")
           (throw 'fail "Did not request repos for any username."))))

(ert-deftest test-magit-gh-repos/interactive ()
  "Should be able to accept username through interactive input."
  (noflet ((gh-repos-user-list (api username &rest args)
             (cond ((string= "foobar" username) (throw 'ok nil))
                   (t (throw 'fail "Input was not processed.")))))
    (catch 'ok (magit-gh-repos "foobar")
           (throw 'fail "Did not contact the API."))))

(ert-deftest test-magit-gh-repos/interactive-2 ()
  "Should not pass empty string to API as username.."
  (noflet ((gh-repos-user-list (api username &rest args)
             (cond ((not username) (throw 'ok nil))
                   (t (throw 'fail "Username was not a nil.")))))
    (catch 'ok (magit-gh-repos "")
           (throw 'fail "Did not contact the API."))))

(ert-deftest test-magit-gh-repos/create-repo () 
  (noflet ((gh-repos-repo-new (api repo)
             (cond ((string= "new" (oref repo :name)) (throw 'ok nil))
                   (t (throw 'fail "Wrong repo was created.")))))
    (catch 'ok (magit-gh-repos-create-repo "new")
           (throw 'fail "Did not create a repo.")))) ;

(ert-deftest test-magit-gh-repos/delete-repo () 
  (noflet ((gh-repos-repo-get (&rest args)
             (test-response (:name "old")))
           (gh-repos-repo-delete (api repo-id)
             (cond ((string= "old" repo-id) (throw 'ok nil))
                   (t (throw 'fail "Wrong repo was deleted.")))))
    (catch 'okq (magit-gh-repos-delete-repo "old")
           (throw 'fail "Did not delete a repo.")))) 

(ert-deftest test-magit-gh-repos/add-remote ()
  "Test that repos can be attached as remotes."
  (noflet ((magit-add-remote (remote url)
             (cond ((string= "http://github.com/" url) 
                    (throw 'ok nil))
                   (t (throw "Unexpected repo was added."))))
           (gh-repos-repo-get (api name &rest args)
             (test-response (:clone-url "http://github.com/"))))
    (catch 'ok (magit-gh-repos-add-remote "foobar")
           (throw 'fail "Remote wasn't added."))))

(ert-deftest test-magit-gh-repos/remote-remove ()
  "Test that deleting a repo will remove it as remote."
  (let ((magit-gh-repos-delete-remotes t))
    (noflet ( (gh-repos-repo-get (api name &rest args)
                (test-response (:clone-url "http://github.com/")))
             (gh-repos-repo-delete (&rest args)
               (test-response () :http-status 204))
              (magit-gh-repos-get-remotes (url &rest args)
                '("github" . "http://github.com/"))
              (magit-remove-remote (remote &rest args) 
                (cond ((string= "github" remote) (throw 'ok nil)r)
                      (t (throw "Wrong remote for removal.")))))
      (catch 'ok (magit-gh-repos-delete-repo "foo")
             (throw 'fail
               "Deleted repo was not removed from remotes.")))))


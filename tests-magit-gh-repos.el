(require 'ert)
(require 'noflet)
(require 'magit-gh-repos)

(defmacro test-response (m &rest p)
  ;; (test-response () :http-status 404)
  ;; (test-response (:name 1 :language 3) :http-status 404)
  ;; (test-response ((:name 1 :language 3) (:description 4)) :http-status 404)
  `(gh-api-paged-response 
    "resp" ,@p
    ,@(when m `(:data ,(if (consp (car m)) 
                           (cons 'list 
                                 (mapcar (lambda (m) `(gh-repos-repo "repo" ,@m)) m))
                           `(gh-repos-repo "repo" ,@m))))))

;; UI / Magit Interface

(ert-deftest tests-magit-gh-repos/configurable ()
  "Should format repo output through evaluation of configured forms."
  (let ((magit-gh-repos-list-format
         '((format "%-4s%s" name language) description))) 
    (magit-gh-repos-pretty-printer 
     (test-magit-gh-repos-mock 'gh-repos-repo 
                               :name "foo"
                               :language "bar"
                               :description "baz"))
    (should (equal "foo bar\nbaz\n" 
                   (buffer-substring-no-properties
                    (point-min) (point-max))))))

(ert-deftest tests-magit-gh-repos/list-sections ()
  "Listing should have at least one section."
  (let (magit-root-section)
    (should-not magit-root-section)
    (noflet ((gh-repos-user-list (&rest args)
               (test-magit-gh-repos-mock 'gh-api-paged-response))
             (magit-gh-repos-pretty-printer (&rest args)))
      (magit-gh-repos-load-next-page nil))
    (should (magit-section-p magit-root-section))))

(ert-deftest tests-magit-gh-repos/repo-sections ()
  "Repos should have a section of their own."
  (let ((magit-gh-repos-list-format '(name)) magit-root-section)
    (noflet ((gh-repos-user-list (&rest args)
               (test-magit-gh-repos-mock 'gh-api-paged-response)))
      (magit-gh-repos-load-next-page nil))
    (should (= 2 (length 
                  (magit-section-children magit-root-section))))))

(ert-deftest tests-magit-gh-repos/ewoc ()
  "Should record printer offsets into EWOC object."
  (let (ewoc-created node-added) 
    (catch 'ok
      (noflet ((gh-repos-user-list (&rest args)
                 (test-magit-gh-repos-mock 'gh-api-paged-response))
               (magit-gh-repos-pretty-printer (&rest args))
               (ewoc-create (pp &rest args) 
                 (cond ((not ewoc-created) (setq ewoc-created t))
                       (t (throw 'fail 
                            "`ewoc-create' called twice"))))
               (ewoc-enter-last (ew nd &rest args)
                 (cond (node-added (throw 'ok nil))
                       (ewoc-created (setq node-added 1))
                       (t (throw 'fail 
                            "`ewoc-created' was not called")))))
        (magit-gh-repos-load-next-page nil)))))

(ert-deftest tests-magit-gh-repos/magit-setup ()
  "Should let magit to set up the buffer."
  (catch 'ok
    (noflet ((magit-mode-init (&rest args)
               (throw 'ok nil)))
      (magit-gh-repos))
    (throw 'fail "Did not call `magit-mode-init'")))

(ert-deftest tests-magit-gh-repos/switch-function ()
  "Should use magit config and allow passing as argument."
  (noflet ((show-buffer-fn (x &rest args) (throw 'ok nil)))
    (catch 'ok (magit-gh-repos nil 'show-buffer-fn)
           (throw 'fail "did not call specified switch-function")))
  (noflet ((another-fn (x &rest args) (throw 'ok nil)))
    (let ((magit-gh-repos-switch-function 'another-fn))
      (catch 'ok (magit-gh-repos nil 'another-fn)
             (throw 'fail 
               "Did not call configured switch-function")))))

(ert-deftest test-magit-gh-repos/username-query ()
  "Should pass optional parameters to `gh-repos-user-list'."
  (catch 'ok
    (noflet ((gh-repos-user-list (a u &rest args)
               (cond ((equal u "foobar") (throw 'ok nil))
                     (t (throw 
                            "Requested repos for wrong username.")))))
      (magit-gh-repos "foobar")
      (throw 'fail "Did not request repos for any username."))))

(ert-deftest test-magit-gh-repos/interactive ()
  "Should be able to accept username through interactive input."
  (catch 'ok
    (noflet ((gh-repos-user-list (api username &rest args)
               (cond ((string= "foobar" username) (throw 'ok nil))
                     (t (throw 'fail "Input was not processed.")))))
      (magit-gh-repos "foobar")
      (throw 'fail "Did not contact the API."))))

(ert-deftest test-magit-gh-repos/interactive-2 ()
  "Should not pass empty string to API as username.."
  (catch 'ok
    (noflet ((gh-repos-user-list (api username &rest args)
               (cond ((not username) (throw 'ok nil))
                     (t (throw 'fail "Username was not a nil.")))))
      (magit-gh-repos "")
      (throw 'fail "Did not contact the API."))))


;; Basic functions

(ert-deftest test-magit-gh-repos/create-repo () 
  (catch 'ok
    (noflet ((gh-repos-repo-new (api repo)
               (cond ((string= "new" (oref repo :name)) (throw 'ok nil))
                     (t (throw 'fail "Wrong repo was created.")))))
      (magit-gh-repos-create-repo "new")
      (throw 'fail "Did not create a repo."))))

(ert-deftest test-magit-gh-repos/delete-repo () 
  (catch 'ok
    (noflet ((gh-api-authenticated-request (&rest args))
             (gh-repos-repo-delete-repo (api repo-id)
               (cond ((string= "old" repo-id) (throw 'ok nil))
                     (t (throw 'fail "Wrong repo was deleted.")))))
      (magit-gh-repos-delete-repo "old")
      (throw 'fail "Did not delete a repo.")))) 

(ert-deftest test-magit-gh-repos/remote-add ()
  "Test that repos can be attached as remotes."
  (catch 'ok
    (noflet ((magit-add-remote (remote url)
               (cond ((string= "http://github.com/" url) (throw 'ok nil))
                     (t (throw "Unexpected repo was added."))))
             (gh-repos-repo-get (api name &rest args)
               (test-response (:clone-url "http://github.com/"))))
      (magit-gh-repos-remote-add "foobar")
      (throw 'fail "Remote wasn't added."))))


(ert-deftest test-magit-gh-repos/remote-add-remove ()
  "Test that deleting a repo will remove it as remote."
  ;; It doesn't make sense to focus on manual invocation of this method.
  (noflet ((gh-repos-repo-get (api name &rest args)
             (test-response (:clone-url "http://github.com/")))
           (gh-repos-repo-delete-repo (&rest args)
             (test-response () :http-status 204))
           (magit-gh-repos-get-remote-adds (url &rest args)
             '("github" . "http://github.com/"))
           (magit-remove-remote-add (remote &rest args) 
             (cond ((string= "github" remote) (throw 'ok nil))
                   (t (throw "Wrong remote for removal.")))))
    (catch 'ok (magit-gh-repos-delete-repo "foo")
           (throw 'fail
             "Deleted repo was not removed from remotes."))))


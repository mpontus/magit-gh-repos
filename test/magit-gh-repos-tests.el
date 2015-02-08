(require 'ert)
(require 'noflet)

(defmacro tests-magit-gh-repos-setup (&rest body) 
  (declare (debug body))
  `(noflet ((magit-git-lines (&rest args))
            (url-retrieve (&rest args))
            (url-retrieve-synchronously (&rest args)))
     (let ((magit-gh-repos-formatters '(name))
           (magit-gh-repos-url-slot :clone-url)) 
       ,@body)))

(ert-deftest tests-magit-gh-repos/root-section ()
  "Listing should have at least one section."
  (tests-magit-gh-repos-setup
   (let (magit-root-section)
     (magit-gh-repos-display-list 
      (list (gh-repos-repo "repo" :name "foo")
            (gh-repos-repo "repo" :name "bar")))
     (should (magit-section-p magit-root-section)))))

(ert-deftest tests-magit-gh-repos/root-section-title ()
  "Should be able to provide title for root section."
  (tests-magit-gh-repos-setup
   (let (magit-root-section)
     (magit-gh-repos-display-list 
      (list (gh-repos-repo "repo" :name "foo")
            (gh-repos-repo "repo" :name "bar"))
      "Test Title:")
     (goto-char (point-min))
     (should 
      (string= "Test Title:"
               (buffer-substring (point) (point-at-eol)))))))

(ert-deftest tests-magit-gh-repos/repo-sections ()
  "Repos should have a section of their own."
  (tests-magit-gh-repos-setup
   (let (magit-root-section)
     (should-not magit-root-section)
     (magit-gh-repos-display-list 
      (list (gh-repos-repo "repo" :name "foo")
            (gh-repos-repo "repo" :name "bar")))
     (should (= 2 (length (magit-section-children magit-root-section)))))))

(ert-deftest tests-magit-gh-repos/ewoc ()
  "Should create ewoc object and assign it to buffer-local variable."
  (let ((repo (gh-repos-repo "repo" :name "foo")))
    (magit-gh-repos-display-list (list repo))
    (should (bound-and-true-p magit-gh-repos-ewoc))
    (should (local-variable-p 'magit-gh-repos-ewoc))
    (should (eq repo (ewoc-data (ewoc-nth magit-gh-repos-ewoc 0))))))

(ert-deftest tests-magit-gh-repos/configurable ()
  "Should be able to configure output format."
  (tests-magit-gh-repos-setup
   (let ((magit-gh-repos-formatters '((format "%-4s%s" language name)))) 
     (magit-gh-repos-display-item
      (gh-repos-repo "repo" :name "foo" :language "bar" :description "baz"))
     (let ((string (buffer-substring-no-properties (point-min) (point-max))))
       (should (equal "bar foo\n" string))))))

(ert-deftest tests-magit-gh-repos/user-repos ()
  "Should display list of user repos."
  (tests-magit-gh-repos-setup
   (let ((repos (list (gh-repos-repo "repo" :name "foo")
                      (gh-repos-repo "repo" :name "bar"))))
     (noflet ((gh-repos-user-list (api username &rest args)
                (if (string= username "foobar")
                    (gh-api-response "resp" :data repos
                                     :http-status 200)
                    (throw 'fail "Requested repos for wrong user name.")))
              (magit-gh-repos-display-list (items title)
                (if (eq items repos) (throw 'ok nil)
                    (throw 'fail "Displaying different repos."))))
       (catch 'ok (magit-gh-repos-user-repos "foobar")
              (throw 'fail "Did not display the list."))))))

(ert-deftest tests-magit-gh-repos/user-repos-1 ()
  "Should default to using api username."
  (tests-magit-gh-repos-setup
   (noflet ((gh-api-get-username (&rest args) "foobar")
            (magit-completing-read
                (prompt collection predicate require-match 
                        initial-input history default)
              (should (string= default "foobar")) "")
            (gh-repos-user-list (api username)
              (if (string= username "foobar") (throw 'ok nil)
                  (throw 'fail 
                    "Empty input should be replaced with api username."))))
     (catch 'ok (call-interactively 'magit-gh-repos-user-repos)
            (throw 'fail "Did not attempt to fetch the list.")))))

(ert-deftest tests-magit-gh-repos/get-repo-by-name ()
  "Test retrieving a repo object by name."
  (tests-magit-gh-repos-setup
   (let ((repo (gh-repos-repo "repo" :name "test"))) 
     (noflet ((gh-repos-repo-get (api name &rest args)
                (if (string= name "test") 
                    (gh-api-response "resp" :data repo
                                     :http-status 200)
                    (throw 'fail "Invalid name."))))
       (should (eq repo (magit-gh-repos-get-repo-by-name "test")))))))

(ert-deftest tests-magit-gh-repos/add-remote ()
  "Test adding repo as a remote."
  (tests-magit-gh-repos-setup
   (noflet ((magit-gh-repos-get-repo-by-name (name)
              (gh-repos-repo "repo ":clone-url "http://github.com/"))
            (magit-gh-repos-get-remotes ())
            (magit-add-remote (remote url)
              (cond ((string= "http://github.com/" url) (throw 'ok nil))
                    (t (throw 'fail "Unexpected repo was added.")))))
     (catch 'ok (magit-gh-repos-add-remote "foobar")
            (throw 'fail "Did not add remote!")))))

(ert-deftest tests-magit-gh-repos/add-remote-1 ()
  "Test that user will be asked if origin remote already exists."
  (tests-magit-gh-repos-setup
   (noflet ((magit-gh-repos-get-repo-by-name (name)
              (gh-repos-repo "repo" :clone-url "http://github.com/"))
            (magit-gh-repos-get-remotes (&optional url)
              '(("origin" . "http://google.com")))
            (magit-completing-read (&rest args) "alternative")
            (magit-add-remote (remote url)
              (cond ((string= "alternative" remote) (throw 'ok nil))
                    (t (throw 'fail "Unexpected remote name.")))))
     (catch 'ok (magit-gh-repos-add-remote "foobar")
            (throw 'fail "Did not add remote!")))))

(ert-deftest tests-magit-gh-repos/add-remote-2 ()
  "Test ignoring duplicates."
  (tests-magit-gh-repos-setup
   (noflet ((magit-gh-repos-get-repo-by-name (name) 
              (gh-repos-repo "repo" :clone-url "http://github.com/"))
            (magit-gh-repos-get-remotes ()
              '(("whatever" . "http://github.com/")))
            (magit-add-remote (&rest args)
              (throw 'fail "Should not add duplicates.")))
     (magit-gh-repos-add-remote "foobar"))))

(ert-deftest tests-magit-gh-repos/create-repo ()
  "Test creating a repo."
  (tests-magit-gh-repos-setup
   (noflet ((gh-repos-repo-new (api repo &rest args )
              (if (string= "foo" (oref repo :name)) (throw 'ok nil)
                  (throw 'fail "Wrong name for created repo."))))
     (catch 'ok (magit-gh-repos-create-repo "foo")
            (throw 'fail "Did not create a repo.")))))

(ert-deftest tests-magit-gh-repos/create-repo-1 ()
  "Test adding a remote for created repo."
  (tests-magit-gh-repos-setup
   (let ((repo (gh-repos-repo "repo" :clone-url "http://github.com/"))) 
     (noflet ((gh-repos-repo-new (&rest args) 
                (gh-api-response "resp" :data repo
                                 :http-status 201))
              (magit-add-remote (remote url)
                (if (string= "http://github.com/" url) (throw 'ok nil)
                    (throw 'fail "Adding remote for wrong repo."))))
       (catch 'ok (magit-gh-repos-create-repo "foo")
              (throw 'fail "Did not add remote."))))))

(ert-deftest tests-magit-gh-repos/delete-repo ()
  "Test creating a repo."
  (tests-magit-gh-repos-setup
   (noflet ((gh-repos-repo-get (api name &rest args)
              (should (string= "foo" name))
              (gh-api-response "resp" 
                :http-status 200
                :data (gh-repos-repo "repo" :name "foo")))
            (gh-repos-repo-delete (api repo-id &rest ags)
              (if (string= "foo" repo-id) (throw 'ok nil)
                  (throw 'fail "Wrong name for deleted repo."))))
     (catch 'ok (magit-gh-repos-delete-repo "foo")
            (throw 'fail "Did not delete a repo.")))))

(ert-deftest tests-magit-gh-repos/delete-repo-1 ()
  "Test that deleting a repo will remove it as remote."
  (tests-magit-gh-repos-setup
   (let ((repo (gh-repos-repo "repo" :clone-url "http://github.com/")))
     (noflet ((gh-repos-repo-get (api name &rest args)
                (gh-api-response "resp" :data repo
                                 :http-status 200))
              (gh-repos-repo-delete (&rest args)
                (gh-api-response "resp" :http-status 204))
              (magit-gh-repos-get-remotes (url &rest args)
                '("github" . "http://github.com/"))
              (magit-remove-remote (remote &rest args) 
                (cond ((string= "github" remote) (throw 'ok nil))
                      (t (throw 'fail "Removing wrong remote.")))))
       (catch 'ok (magit-gh-repos-delete-repo "foo")
              (throw 'fail "Did not remove remote."))))))

(ert-deftest tests-magit-gh-repos/fork-repo ()
  "Should be able to fork repo."
  (tests-magit-gh-repos-setup
   (let ((orig-repo (gh-repos-repo "repo" :clone-url "http://github.com/orig/"))
         (fork-repo (gh-repos-repo "repo" :clone-url "http://github.com/fork/"))) 
     (noflet ((gh-repos-repo-get (api name)
                (should (string= "foobar" name)) 
                (gh-api-response "resp" :data orig-repo
                                 :http-status 200))
              (gh-repos-fork (api repo &rest args)
                (should (eq repo orig-repo))
                (gh-api-response "resp" :data fork-repo
                                 :http-status 202))
              (magit-add-remote (remote url)
                (if (string= url "http://github.com/fork/") (throw 'ok nil)
                    (throw 'fail "Adding wrong url as remote."))))
       (catch 'ok (magit-gh-repos-fork-repo "foobar")
              (throw 'fail "Repo wasn't added as a remote."))))))

(ert-deftest tests-magit-gh-repos/forks-list ()
  "Test that list of forks will be displayed."
  (tests-magit-gh-repos-setup
   (let ((repos (list (gh-repos-repo "repo" :name "foo")
                      (gh-repos-repo "repo" :name "bar")))
         (main-repo   (gh-repos-repo "repo" :name "baz")))
     (noflet ((gh-repos-repo-get (api name &rest args)
                (gh-api-response (eq "foo" name))
                (gh-api-response "resp" :data main-repo
                                 :http-status 200))

              (gh-repos-forks-list (api repo &rest args)
                (should (eq main-repo repo))
                (gh-api-response "resp" :data repos
                                 :http-status 200))
              (magit-gh-repos-display-list (items title)
                (if (equal repos items) (throw 'ok nil)
                    (throw 'fail "Wrong items have been sent to display."))))
       (catch 'ok (magit-gh-repos-forks-list "foo")
              (throw 'fail "Nothing was displayed."))))))

(ert-deftest tests-magit-gh-repos/rename ()
  "Renaming the repo should update remote url."
  (tests-magit-gh-repos-setup
   (let ((orig-repo (gh-repos-repo "repo" :clone-url "http://google.com/"))
         (new-repo (gh-repos-repo "repo" :clone-url "http://github.com/")))
     (noflet ((gh-repos-repo-get (api name &rest args)
                (should (string= "foo" name))
                (gh-api-response "date" :data orig-repo
                                 :http-status 200 ))
              (gh-repos-repo-rename (api repo name &rest args)
                (should (eq orig-repo repo))
                (gh-api-response "date" :data new-repo
                                 :http-status 200 ))
              (magit-gh-repos-get-remotes (url)
                (should (string= "http://google.com/" url))
                (cons "test" "http://google.com/"))
              (magit-git-success (args)
                (if (equal args '("remote" "set-url" "test" "http://github.com/")) 
                    (throw 'ok nil) (throw 'fail "Wrong arguments!"))))
       (catch 'ok (magit-gh-repos-rename-repo "foo" "bar")
              (throw 'fail "Remote url hasn't changed!"))))))


(ert-deftest tests-magit-gh-repos/edit ()
  "Should use eieio customizing interface to customize object."
  (let* ((repo (gh-repos-repo "repo" :name "foo" :description "bar"
                              :private :json-false :has-issues t
                              :has-wiki t :has-downloads t)))
    (noflet ((magit-gh-repos-get-repo-by-name (name)
               (should (string= "foo" name)) repo)
             (customize-object (editable-repo)
               (if (and (eq repo (oref editable-repo :repo))
                        (string= "foo" (oref editable-repo :name))
                        (string= "bar" (oref editable-repo :description))
                        (eq nil (oref editable-repo :private))
                        (eq t (oref editable-repo :has-issues))
                        (eq t (oref editable-repo :has-wiki))
                        (eq t (oref editable-repo :has-downloads))) 
                   (throw 'ok nil)
                   (throw 'fail "Wrong original repo or parameters."))))
      (catch 'ok (magit-gh-repos-edit-repo "foo")
             (throw 'fail "Did not call `customize-object'.")))))

(ert-deftest tests-magit-gh-repos/edit-1 ()
  "Should rename the repo"
  (let* ((orig-repo (gh-repos-repo "repo" :name "foo"))
         (editable-repo (magit-gh-repos-editable-repo orig-repo)))
    (oset editable-repo :name "bar")
    (noflet ((magit-gh-repos-rename-repo (old-name new-name)
               (if (and (string= "foo" old-name)
                        (string= "bar" new-name)) (throw 'ok nil)
                        (throw 'fail "Wrong repo or name"))))
      (catch 'ok (eieio-done-customizing editable-repo)
             (throw 'fail "Repo was not renamed.")))))

(ert-deftest tests-magit-gh-repos/edit-2 ()
  "Should update the repo"
  (let* ((repo (gh-repos-repo "repo" :name "foo" :description "bar"
                              :private :json-false :has-issues t
                              :has-wiki t :has-downloads t))
         (editable-repo (magit-gh-repos-editable-repo repo)))
    (oset editable-repo :description "baz")
    (oset editable-repo :private t)
    (oset editable-repo :has-issues nil)
    (oset editable-repo :has-wiki nil)
    (oset editable-repo :has-downloads nil)
    (noflet ((gh-repos-repo-update (api repo &optional user &rest caps)
               (if (and (string= "baz" (oref repo :description))
                        (eq :json-true (oref repo :private))
                        (eq nil (plist-get caps :issues))
                        (eq nil (plist-get caps :wiki))
                        (eq nil (plist-get caps :downloads)))
                   (throw 'ok nil)
                   (throw 'fail "Wrong update parameters"))))
      (catch 'ok (eieio-done-customizing editable-repo)
             (throw 'fail "Did not update the repo.")))))

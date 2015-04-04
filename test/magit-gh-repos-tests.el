(require 'ert)
(require 'noflet)

(defmacro tests-magit-gh-repos-setup (&rest body) 
  (declare (debug body))
  `(noflet ((magit-git-lines (&rest args))
            (magit-get-top-dir (&optional dir) dir)
            (url-retrieve (&rest args))
            (url-retrieve-synchronously (&rest args)))
     (let ((magit-gh-repos-formatters '(name))
           (magit-gh-repos-url-slot :clone-url)) 
       ,@body)))

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

(ert-deftest test-magit-gh-repos/read-top-dir ()
  "Should dispatch return value of `magit-get-top-dir' without prefix arg."
  (let ((symbol (make-symbol "symbol")))
    (noflet ((magit-get-top-dir (&rest args) symbol))
      (should (eq symbol (magit-gh-repos-read-top-dir))))))

(ert-deftest test-magit-gh-repos/read-top-dir-1 ()
  "Should ask for top directory with prefix argument."
  (noflet ((magit-read-top-dir (dir)
             (if (not dir) (throw 'ok nil)
                 (throw 'fail
                   "Should have called `magit-read-top-dir' with negative argument."))))
    (catch 'ok (let ((current-prefix-arg '(4)))
                 (magit-gh-repos-read-top-dir))
           (throw 'fail "Did not ask for top dir."))))

(ert-deftest test-magit-gh-repos/read-top-dir-2 ()
  "Should call `magit-read-top-dir' with positive argument when prefix argument > 4."
  (noflet ((magit-read-top-dir (dir)
             (if dir (throw 'ok nil)
                 (throw 'fail
                   "Should have called `magit-read-top-dir' with positive argument."))))
    (catch 'ok (let ((current-prefix-arg 8))
                 (magit-gh-repos-read-top-dir))
           (throw 'fail "Did not ask for top dir."))))

(ert-deftest test-magit-gh-repos/read-top-dir-3 ()
  "Should offer to create new repository with prefix argument."
  (noflet ((magit-read-top-dir (&rest args))
           (magit-get-top-dir (&rest args))
           (yes-or-no-p (&rest args) (throw 'ok nil)))
    (catch 'ok (let ((current-prefix-arg '(4)))
                 (magit-gh-repos-read-top-dir))
           (throw 'fail "Did not offer to create new repository."))))

(ert-deftest tests-magit-gh-repos/add-remote-to-repo ()
  "Test adding repo as a remote."
  (tests-magit-gh-repos-setup
   (noflet ((magit-gh-repos-get-repo-by-name (name)
              (gh-repos-repo "repo ":clone-url "http://github.com/"))
            (magit-gh-repos-get-all-remotes ())
            (magit-add-remote (remote url)
              (cond ((string= "http://github.com/" url) (throw 'ok nil))
                    (t (throw 'fail "Unexpected repo was added.")))))
     (catch 'ok (magit-gh-repos-add-remote-to-repo "foobar")
            (throw 'fail "Did not add remote!")))))

(ert-deftest tests-magit-gh-repos/add-remote-1 ()
  "Test that user will be asked if origin remote already exists."
  (tests-magit-gh-repos-setup
   (noflet ((magit-gh-repos-get-repo-by-name (name)
              (gh-repos-repo "repo" :clone-url "http://github.com/"))
            (magit-gh-repos-get-all-remotes (&optional url)
              '(("origin" . "http://google.com")))
            (magit-completing-read (&rest args) "alternative")
            (magit-add-remote (remote url)
              (cond ((string= "alternative" remote) (throw 'ok nil))
                    (t (throw 'fail "Unexpected remote name.")))))
     (catch 'ok (magit-gh-repos-add-remote-to-repo "foobar")
            (throw 'fail "Did not add remote!")))))

(ert-deftest tests-magit-gh-repos/add-remote-2 ()
  "Test ignoring duplicates."
  (tests-magit-gh-repos-setup
   (noflet ((magit-gh-repos-get-repo-by-name (name) 
              (gh-repos-repo "repo" :clone-url "http://github.com/"))
            (magit-gh-repos-get-all-remotes ()
              '(("whatever" . "http://github.com/")))
            (magit-add-remote (&rest args)
              (throw 'fail "Should not add duplicates.")))
     (magit-gh-repos-add-remote-to-repo "foobar"))))

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
       (catch 'ok (magit-gh-repos-create-repo "foo" "/")
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
              (magit-gh-repos-get-all-remotes ()
                '(("github" . "http://github.com/")))
              (magit-remove-remote (remote &rest args) 
                (cond ((string= "github" remote) (throw 'ok nil))
                      (t (throw 'fail "Removing wrong remote.")))))
       (catch 'ok (magit-gh-repos-delete-repo "foo" "/")
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
       (catch 'ok (magit-gh-repos-fork-repo "foobar" "/")
              (throw 'fail "Repo wasn't added as a remote."))))))

(ert-deftest tests-magit-gh-repos/rename ()
  "Renaming the repo should update remote url."
  (tests-magit-gh-repos-setup
   (let ((orig-repo (gh-repos-repo "repo" :clone-url "http://google.com/"))
         (new-repo (gh-repos-repo "repo" :clone-url "http://github.com/")))
     (noflet ((gh-repos-repo-get (api name &rest args)
                (should (string= "foo" name))
                (gh-api-response "date" :data orig-repo :http-status 200 ))
               (gh-repos-repo-rename (api repo name &rest args)
                 (should (eq orig-repo repo))
                 (gh-api-response "date" :data new-repo :http-status 200 ))
               (magit-gh-repos-get-all-remotes ()
                 '(("test" . "http://google.com/")))
               (magit-git-success (args)
                 (if (equal args
                            '("remote" "set-url" "test" "http://github.com/"))
                     (throw 'ok nil) (throw 'fail "Wrong arguments!"))))
       (catch 'ok (magit-gh-repos-rename-repo "foo" "bar" "/")
              (throw 'fail "Didn't change remote url!"))))))

(ert-deftest tests-magit-gh-repos/rename-1 ()
  "Should query for a new name based on original name."
  (noflet ((magit-gh-repos-read-repo-name (prompt) "foo")
           (read-string (prompt initial &rest args)
             (if (string= "foo" initial) (throw 'ok nil)
                 (throw 'fail "Wrong initial value"))))
    (catch 'ok (call-interactively 'magit-gh-repos-rename-repo)
           (throw 'fail "Did not call `read-string'"))))

(ert-deftest tests-magit-gh-repos/rename-2 ()
  "Should do nothing when new name matches old one."
  (noflet ((magit-gh-repos-get-repo-by-name (name)
             (throw 'fail "Should not try to resolve repo by name."))
           (gh-repos-repo-rename (&rest args)
             (throw 'fail "Should not try to rename the repo.")))
    (magit-gh-repos-rename-repo "foo" "foo")))

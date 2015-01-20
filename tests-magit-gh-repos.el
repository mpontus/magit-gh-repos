(require 'ert)
(require 'noflet)
(require 'magit-gh-repos)

(defmacro tests-magit-gh-repos-setup (&rest body)
  `(noflet ((gh-repos-user-list ())
            (gh-repos-api ()))
     (let (magit-gh-repos-format)
       ,@body)))

(defun test-magit-gh-repos-mock (cls &rest args)
  (cl-case cls 
    (gh-api-paged-response
     (apply 'make-instance cls :data
            (list (test-magit-gh-repos-mock 'gh-repos-repo)
                  (test-magit-gh-repos-mock 'gh-repos-repo)) args))
    (t (apply 'make-instance cls args))))

(ert-deftest tests-magit-gh-repos/configurable ()
  "Should format repo output through evaluation of configured forms."
  (let ((magit-gh-repos-list-format
         '((format "%-4s%s" name language) description))) 
    (magit-gh-repos-display-repo 
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
    (noflet ((magit-gh-repos-display-repo (repo)))
      (magit-gh-repos-display-list 
       (list (test-magit-gh-repos-mock 'gh-repos-repo)
             (test-magit-gh-repos-mock 'gh-repos-repo))))
    (should (magit-section-p magit-root-section))))

(ert-deftest tests-magit-gh-repos/repo-sections ()
  "Repos should have a section of their own."
  (let ((magit-gh-repos-list-format '(name)) magit-root-section)
    (magit-gh-repos-display-list
     (list (test-magit-gh-repos-mock 'gh-repos-repo :name "foo")
           (test-magit-gh-repos-mock 'gh-repos-repo :name "bar")))
    (should (= 2 (length (magit-section-children magit-root-section))))))

(ert-deftest tests-magit-gh-repos/ewoc ()
  "Should record printer offsets into EWOC object."
  (let (ewoc-created node-added) 
    (catch 'ok
      (noflet ((ewoc-create (pp) 
                 (cond ((not ewoc-created) (setq ewoc-created t))
                       (t (throw 'fail "`ewoc-create' called twice"))))
               (ewoc-enter-last (ew nd)
                 (cond (node-added (throw 'ok nil))
                       (ewoc-created (setq node-added 1))
                       (t (throw 'fail "`ewoc-created' was not called")))))
        (magit-gh-repos-display-list 
         (list (test-magit-gh-repos-mock 'gh-repos-repo)
               (test-magit-gh-repos-mock 'gh-repos-repo)))))))

(ert-deftest tests-magit-gh-repos/magit-setup ()
  "Should let magit to set up the buffer."
  (catch 'ok
    (noflet ((magit-mode-init (&rest args)
               (throw 'ok nil)))
      (magit-gh-repos))
    (throw 'fail "Did not call `magit-mode-init'")))

(ert-deftest tests-magit-gh-repos/switch-function ()
  "Should use magit config and allow passing as argument."
  (noflet ((show-buffer-fn (x) (throw 'ok nil)))
    (catch 'ok (magit-gh-repos nil 'show-buffer-fn)
           (throw 'fail "did not call specified switch-function")))
  (noflet ((another-fn (x) (throw 'ok nil)))
    (let ((magit-gh-repos-switch-function 'another-fn))
      (catch 'ok (magit-gh-repos nil 'another-fn)
             (throw 'fail "Did not call configured switch-function")))))

(ert-deftest test-magit-gh-repos/username-query ()
  "Should pass optional parameters to `gh-repos-user-list'."
  (catch 'ok
    (noflet ((gh-repos-api (&rest args))
             (gh-repos-user-list (a u)
               (cond ((equal u "foobar") (throw 'ok nil))
                     p(t (throw "Requested repos for wrong username.")))))
      (magit-gh-repos "foobar")
      (throw 'fail "Did not request repos for any username."))))

(ert-deftest test-magit-gh-respos/interactive ()
  "Should be interactive and username with prefix-arg."
  (catch 'ok
    (noflet ((read-string (prompt &rest args) 
               (throw 'ok nil))
             (gh-repos-user-list nil) )
      (call-interactively 'magit-gh-repos)
      (throw 'fail "Did not call `read-string'."))))

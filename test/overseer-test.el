(ert-deftest test-root-directory/with-root-indicator ()
  (within-sandbox "lisp/path"
                  (f-touch "../../Cask")
                  (should (equal (overseer-project-root) (f-slash overseer-sandbox-path)))))

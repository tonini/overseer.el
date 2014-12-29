(ert-deftest test-root-directory/with-root-indicator ()
  (within-sandbox "lisp/path"
                  (f-touch "../../Cask")
                  (should (equal (ert-runner-mode-project-root) ert-runner-mode-sandbox-path))))

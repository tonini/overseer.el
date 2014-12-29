(require 'f)

(defvar ert-runner-mode-test/test-path
  (f-parent (f-this-file)))

(defvar ert-runner-mode-test/root-path
  (f-parent ert-runner-mode-test/test-path))

(defvar ert-runner-mode-test-path
  (f-dirname (f-this-file)))

(defvar ert-runner-mode-sandbox-path
  (f-expand "sandbox" ert-runner-mode-test-path))

(defmacro within-sandbox (&optional current &rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory
           (f-join ert-runner-mode-sandbox-path (format "%s" ,current))))
     (f-mkdir ert-runner-mode-sandbox-path)
     ,@body
     (f-delete ert-runner-mode-sandbox-path :force)))

(require 'ert-runner-mode (f-expand "ert-runner-mode" ert-runner-mode-test/root-path))
(require 'ert)

(provide 'test-helper)

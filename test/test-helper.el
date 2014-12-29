(require 'f)

(defvar overseer-test-path
  (f-parent (f-this-file)))

(defvar overseer-root-path
  (f-parent overseer-test-path))

(defvar overseer-test-path
  (f-dirname (f-this-file)))

(defvar overseer-sandbox-path
  (f-expand "sandbox" overseer-test-path))

(defmacro within-sandbox (&optional current &rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory
           (f-join overseer-sandbox-path (format "%s" ,current))))
     (f-mkdir overseer-sandbox-path)
     ,@body
     (f-delete overseer-sandbox-path :force)))

(require 'overseer (f-expand "overseer" overseer-root-path))
(require 'ert)

(provide 'test-helper)

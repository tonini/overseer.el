(require 'f)

(defvar overseer-test-path
  (f-parent (f-this-file)))

(defvar overseer-root-path
  (f-parent overseer-test-path))

(defvar overseer-test-path
  (f-dirname (f-this-file)))

(defvar overseer-sandbox-path
  (file-name-as-directory (f-expand "sandbox" overseer-test-path)))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory overseer-sandbox-path))
     (when (f-dir? overseer-sandbox-path)
       (f-delete overseer-sandbox-path :force))
     (f-mkdir overseer-sandbox-path)
     ,@body))

(require 'overseer (f-expand "overseer" overseer-root-path))
(require 'ert)

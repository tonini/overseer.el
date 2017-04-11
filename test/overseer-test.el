;;; overseer-test.el --- Test suite for Overseer functionality.

;; Copyright © 2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test suite for Overseer functionality.

;;; Code:

(ert-deftest test-root-directory/with-root-indicator ()
  (within-sandbox
   (f-touch "Cask")
   (should (equal (overseer-project-root) (f-slash overseer-sandbox-path)))))

(ert-deftest test-if-the-current-buffer-file-is-a-test-file ()
  "Should return t if visited file is a test file"
  (within-sandbox
   (f-touch "lisp-test.el")
   (find-file "lisp-test.el")
   (should (overseer--current-buffer-test-file-p))))

(provide 'overseer-test)

;;; overseer-test.el ends here

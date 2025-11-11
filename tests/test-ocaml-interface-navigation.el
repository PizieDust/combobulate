;;; test-ocaml-interface-navigation.el --- Tests for OCaml interface (.mli) navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Tim McGilchrist

;; Author: Tim McGilchrist <timmcgil@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for navigation between top-level bindings in .mli files

;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'ert)

(ert-deftest combobulate-test-ocaml-interface-sibling-navigation ()
  "Test sibling navigation between top-level items in .mli files.
Currently navigates through keyword nodes (val, type, module, etc.) rather than
structural nodes (value_specification, type_definition, etc.)."
  :tags '(ocaml navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      ;; Navigate to first val declaration - line 3 is at position 35
      (goto-char 35)

      ;; Verify starting position
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "val"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "Starting position - Expected: %s, Got: %s" expected actual)))

      ;; Navigate through all top-level items using combobulate-navigate-next
      ;; Currently goes through keywords: val -> type -> module -> class -> exception -> module
      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "val"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 1st next - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "type"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 2nd next - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 3rd next - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "class"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 4th next - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "exception"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 5th next - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 6th next - Expected: %s, Got: %s" expected actual)))

      ;; Navigate backward
      (combobulate-navigate-previous)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "exception"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 1st prev - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-previous)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "class"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 2nd prev - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-previous)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 3rd prev - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-previous)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "type"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 4th prev - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-previous)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "val"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 5th prev - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-previous)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "val"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After 6th prev - Expected: %s, Got: %s" expected actual))))))

(ert-deftest combobulate-test-ocaml-interface-tree-structure ()
  "Test that .mli files parse correctly with expected tree structure."
  :tags '(ocaml combobulate)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

    (let* ((root (combobulate-root-node))
           (children (combobulate-node-children root)))

      ;; Should have comment + 7 top-level items
      (should (= (length children) 8))

      ;; First child is a comment
      (should (equal (combobulate-node-type (nth 0 children)) "comment"))

      ;; Check each top-level item type
      (should (equal (combobulate-node-type (nth 1 children)) "value_specification"))
      (should (equal (combobulate-node-type (nth 2 children)) "value_specification"))
      (should (equal (combobulate-node-type (nth 3 children)) "type_definition"))
      (should (equal (combobulate-node-type (nth 4 children)) "module_definition"))
      (should (equal (combobulate-node-type (nth 5 children)) "class_definition"))
      (should (equal (combobulate-node-type (nth 6 children)) "exception_definition"))
      (should (equal (combobulate-node-type (nth 7 children)) "module_type_definition"))

      ;; Verify sibling relationships at tree-sitter level
      (let ((val1 (nth 1 children)))
        (should (combobulate-node-eq
                 (combobulate-node-next-sibling val1)
                 (nth 2 children))))))))

(ert-deftest combobulate-test-ocaml-interface-hierarchy-navigation ()
  "Test hierarchy navigation (C-M-d) through module signatures in .mli files.
This tests navigating down from module keyword → module_binding → signature → value_specification."
  :tags '(ocaml navigation combobulate hierarchy)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      ;; Navigate to "module MyModule" line and position at the 'module' keyword
      (goto-char (point-min))
      (re-search-forward "^module MyModule")
      (beginning-of-line)

      ;; Verify we're at the 'module' keyword node
      (let ((node (combobulate-node-at-point)))
        (should (equal "module" (combobulate-node-type node))))

      ;; First C-M-d: should move to module_name
      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module_name"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; Second C-M-d: should move to sig
      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "sig"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After second C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; Third C-M-d: should move to val
      (combobulate-navigate-down)
      (let* ((node (combobulate-node-at-point))
             (actual (combobulate-node-type node))
             (expected "val"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After third C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; TODO This navigation is un-expected, it should be symetrical and
      ;; visit sig on the way back up.
      ;; Navigate up should get us back to sig
      ;; (combobulate-navigate-up)
      ;; (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
      ;;        (expected "sig"))
      ;;   (should (equal expected actual))
      ;;   (unless (equal expected actual)
      ;;     (message "After first C-M-u - Expected: %s, Got: %s" expected actual)))

      ;; Navigate up should get us back to module_name
      (combobulate-navigate-up)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module_name"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-u - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-up)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After third C-M-u - Expected: %s, Got: %s" expected actual)))

      )))

(ert-deftest combobulate-test-ocaml-interface-hierarchy-navigation-class ()
  "Test hierarchy navigation (C-M-d) through module signatures in .mli files.
This tests navigating down from module keyword → module_binding → signature → value_specification."
  :tags '(ocaml navigation combobulate hierarchy)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      (goto-char (point-min))
      (re-search-forward "^class my_class")
      (beginning-of-line)

      (let ((node (combobulate-node-at-point)))
        (should (equal "class" (combobulate-node-type node))))

      ;; First C-M-d: should move to class_name
      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "class_name"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; Navigate next to object keyword (class_body_type)
      (combobulate-navigate-next)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "object"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After C-M-n - Expected: %s, Got: %s" expected actual)))

      ;; Navigate down to method keyword
      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "method"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After second C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; Navigate down to method_name
      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "method_name"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After third C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; Navigate back up to method
      (combobulate-navigate-up)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "method"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-u - Expected: %s, Got: %s" expected actual)))

      ;; Navigate up to object
      (combobulate-navigate-up)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "object"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After second C-M-u - Expected: %s, Got: %s" expected actual)))

      ;; Navigate up to class
      (combobulate-navigate-up)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "class"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After third C-M-u - Expected: %s, Got: %s" expected actual)))

      )))

(ert-deftest combobulate-test-ocaml-interface-hierarchy-navigation-module-type ()
  "Test hierarchy navigation (C-M-d) through module signatures in .mli files.
This tests navigating down from module keyword → module_binding → signature → value_specification."
  :tags '(ocaml navigation combobulate hierarchy)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      ;; Navigate to "module MyModule" line and position at the 'module' keyword
      (goto-char (point-min))
      (re-search-forward "^module type MyModuleType")
      (beginning-of-line)

      ;; Verify we're at the 'module' keyword node
      (let ((node (combobulate-node-at-point)))
        (should (equal "module" (combobulate-node-type node))))

      ;; First C-M-d: should move to module_name
      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module_type_name"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-d - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "sig"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-d - Expected: %s, Got: %s" expected actual)))

      (combobulate-navigate-down)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "type"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-d - Expected: %s, Got: %s" expected actual)))

      ;; TODO: Navigate up is asymmetric - it skips sig and module_type_name
      ;; and goes directly from type to module keyword
      (combobulate-navigate-up)
      (let* ((actual (combobulate-node-type (combobulate-node-at-point)))
             (expected "module"))
        (should (equal expected actual))
        (unless (equal expected actual)
          (message "After first C-M-u - Expected: %s, Got: %s" expected actual)))

      )))


(provide 'test-ocaml-interface-navigation)
;;; test-ocaml-interface-navigation.el ends here

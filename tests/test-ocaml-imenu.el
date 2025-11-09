;;; test-ocaml-imenu.el --- tests for OCaml imenu support  -*- lexical-binding: t; -*-

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

;; Tests for OCaml imenu support in Combobulate

;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'ert)
(require 'imenu)

(ert-deftest combobulate-test-ocaml-imenu-ml-file ()
  "Test that imenu is correctly populated for OCaml .ml files."
  :tags '(ocaml imenu combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (let ((fixture-file (expand-file-name "tests/fixtures/imenu/ocaml-sample.ml"
                                        default-directory)))
    (with-temp-buffer
      ;; Load the file content
      (insert-file-contents fixture-file)
      ;; Set up tuareg mode which will create the tree-sitter parser
      (tuareg-mode)
      ;; Enable combobulate
      (combobulate-mode)
      ;; Give tree-sitter a moment to parse
      (sit-for 0.1)
      ;; Get the imenu index
      (let ((index (funcall imenu-create-index-function)))
        ;; Verify we have entries
        (should index)
        ;; Check for Type entries
        (let ((types (cdr (assoc "Type" index))))
          (should types)
          (should (assoc "type my_type" types)))
        ;; Check for Module entries
        (let ((modules (cdr (assoc "Module" index))))
          (should modules)
          (should (assoc "module MyModule" modules)))
        ;; Check for Class entries
        (let ((classes (cdr (assoc "Class" index))))
          (should classes)
          (should (assoc "class my_class" classes)))
        ;; Check for Value entries (let bindings)
        (let ((values (cdr (assoc "Value" index))))
          (should values)
          (should (assoc "let my_function" values))
          (should (assoc "let another_value" values)))
        ;; Check for Exception entries
        (let ((exceptions (cdr (assoc "Exception" index))))
          (should exceptions)
          (should (assoc "exception MyException" exceptions)))
        ;; Check for External entries
        (let ((externals (cdr (assoc "External" index))))
          (should externals)
          (should (assoc "external my_external" externals)))))))

(ert-deftest combobulate-test-ocaml-imenu-mli-file ()
  "Test that imenu is correctly populated for OCaml .mli files."
  :tags '(ocaml imenu combobulate)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "tests/fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      ;; Load the file content
      (insert-file-contents fixture-file)
      ;; Set the buffer file name so tuareg-treesit creates the right parser
      (setq buffer-file-name fixture-file)
      ;; Set up tuareg mode which will create the tree-sitter parser
      (tuareg-mode)
      ;; Enable combobulate
      (combobulate-mode)
      ;; Give tree-sitter a moment to parse
      (sit-for 0.1)
      ;; Get the imenu index
      (let ((index (funcall imenu-create-index-function)))
        ;; Verify we have entries
        (should index)
        ;; Check for Type entries
        (let ((types (cdr (assoc "Type" index))))
          (should types)
          (should (assoc "type my_type" types)))
        ;; Check for Module entries
        (let ((modules (cdr (assoc "Module" index))))
          (should modules)
          (should (assoc "module MyModule" modules)))
        ;; Check for Class entries
        (let ((classes (cdr (assoc "Class" index))))
          (should classes)
          (should (assoc "class my_class" classes)))
        ;; Check for Value Spec entries (val declarations)
        (let ((value-specs (cdr (assoc "Value Spec" index))))
          (should value-specs)
          (should (assoc "val my_function" value-specs))
          (should (assoc "val another_value" value-specs)))
        ;; Check for Exception entries
        (let ((exceptions (cdr (assoc "Exception" index))))
          (should exceptions)
          (should (assoc "exception MyException" exceptions)))
        ;; Check for Module Type entries
        (let ((module-types (cdr (assoc "Module Type" index))))
          (should module-types)
          (should (assoc "module type MyModuleType" module-types)))))))

(provide 'test-ocaml-imenu)
;;; test-ocaml-imenu.el ends here

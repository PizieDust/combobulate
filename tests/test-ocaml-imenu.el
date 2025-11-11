;;; test-ocaml-imenu.el --- Tests for OCaml imenu support  -*- lexical-binding: t; -*-

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

;; Tests for imenu generation in OCaml implementation (.ml) and interface (.mli) files

;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'ert)

(ert-deftest combobulate-test-ocaml-imenu-interface ()
  "Test that imenu works correctly for OCaml interface (.mli) files."
  :tags '(ocaml imenu combobulate)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/demo.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      ;; Generate imenu index
      (let ((index (funcall imenu-create-index-function)))

        ;; Should have entries
        (should (> (length index) 0))

        ;; Check for expected top-level modules
        (let ((module-entries (alist-get "Module" index nil nil #'equal)))
          (should module-entries)

          ;; Check for specific modules
          (should (cl-some (lambda (entry)
                            (string-match-p "module Positive" (car entry)))
                          module-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module Math" (car entry)))
                          module-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module Collections" (car entry)))
                          module-entries)))

        ;; Check for module types
        (let ((module-type-entries (alist-get "Module Type" index nil nil #'equal)))
          (should module-type-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "module type ORDERED" (car entry)))
                          module-type-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module type MONAD" (car entry)))
                          module-type-entries))
          ;; Check for new module types
          (should (cl-some (lambda (entry)
                            (string-match-p "module type COMPARABLE" (car entry)))
                          module-type-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module type PRINTABLE" (car entry)))
                          module-type-entries)))

        ;; Check for class types
        (let ((class-type-entries (alist-get "Class Type" index nil nil #'equal)))
          (should class-type-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "class type point_type" (car entry)))
                          class-type-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "class type shape_type" (car entry)))
                          class-type-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "class type colored_shape_type" (car entry)))
                          class-type-entries)))

        ;; Check for include statements (include_module_type)
        (let ((include-sig-entries (alist-get "Include Sig" index nil nil #'equal)))
          (when include-sig-entries
            ;; We have include statements within module type definitions
            (should (> (length include-sig-entries) 0))))

        ;; Check for value specifications (val declarations)
        (let ((val-entries (alist-get "Value Spec" index nil nil #'equal)))
          (when val-entries
            ;; Should have val entries for top-level functions
            (should (cl-some (lambda (entry)
                              (string-match-p "val make_adder" (car entry)))
                            val-entries))
            ;; Check for polymorphic variant examples
            (should (cl-some (lambda (entry)
                              (string-match-p "val color_to_string" (car entry)))
                            val-entries))))

        ;; Check for types (including polymorphic variants)
        (let ((type-entries (alist-get "Type" index nil nil #'equal)))
          (when type-entries
            (should (cl-some (lambda (entry)
                              (string-match-p "type color" (car entry)))
                            type-entries))
            (should (cl-some (lambda (entry)
                              (string-match-p "type message" (car entry)))
                            type-entries))))))))

(ert-deftest combobulate-test-ocaml-imenu-implementation ()
  "Test that imenu works correctly for OCaml implementation (.ml) files."
  :tags '(ocaml imenu combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (let ((fixture-file (expand-file-name "fixtures/imenu/demo.ml"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      ;; Generate imenu index
      (let ((index (funcall imenu-create-index-function)))

        ;; Should have entries
        (should (> (length index) 0))

        ;; Check for expected top-level modules
        (let ((module-entries (alist-get "Module" index nil nil #'equal)))
          (should module-entries)

          ;; Check for specific modules
          (should (cl-some (lambda (entry)
                            (string-match-p "module Positive" (car entry)))
                          module-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module Math" (car entry)))
                          module-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module Collections" (car entry)))
                          module-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "module DataStructures" (car entry)))
                          module-entries)))

        ;; Check for module types
        (let ((module-type-entries (alist-get "Module Type" index nil nil #'equal)))
          (when module-type-entries
            (should (cl-some (lambda (entry)
                              (string-match-p "module type ORDERED" (car entry)))
                            module-type-entries))))

        ;; Check for value definitions (let bindings)
        (let ((value-entries (alist-get "Value" index nil nil #'equal)))
          (when value-entries
            ;; Should have let bindings for top-level functions
            (should (cl-some (lambda (entry)
                              (string-match-p "let make_adder" (car entry)))
                            value-entries))
            (should (cl-some (lambda (entry)
                              (string-match-p "let compose" (car entry)))
                            value-entries))))

        ;; Check for type definitions
        (let ((type-entries (alist-get "Type" index nil nil #'equal)))
          (when type-entries
            ;; Collections.Tree.t should appear
            (should (cl-some (lambda (entry)
                              (string-match-p "type" (car entry)))
                            type-entries))))))))

(ert-deftest combobulate-test-ocaml-imenu-sample-interface ()
  "Test that imenu works for the simple sample interface file."
  :tags '(ocaml imenu combobulate)
  (skip-unless (treesit-language-available-p 'ocaml_interface))
  (let ((fixture-file (expand-file-name "fixtures/imenu/ocaml-sample.mli"
                                        default-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-file)
      (setq buffer-file-name fixture-file)
      (tuareg-mode)
      (combobulate-mode)
      (sit-for 0.1)

      ;; Generate imenu index
      (let ((index (funcall imenu-create-index-function)))

        ;; Should have entries
        (should (> (length index) 0))

        ;; Check for value specifications
        ;; Note: imenu captures all val declarations including nested ones
        (let ((val-entries (alist-get "Value Spec" index nil nil #'equal)))
          (should val-entries)
          (should (>= (length val-entries) 2))
          ;; Check for top-level value specs
          (should (cl-some (lambda (entry)
                            (string-match-p "val my_function" (car entry)))
                          val-entries))
          (should (cl-some (lambda (entry)
                            (string-match-p "val another_value" (car entry)))
                          val-entries)))

        ;; Check for type definitions
        (let ((type-entries (alist-get "Type" index nil nil #'equal)))
          (should type-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "type my_type" (car entry)))
                          type-entries)))

        ;; Check for modules
        (let ((module-entries (alist-get "Module" index nil nil #'equal)))
          (should module-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "module MyModule" (car entry)))
                          module-entries)))

        ;; Check for classes
        (let ((class-entries (alist-get "Class" index nil nil #'equal)))
          (should class-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "class my_class" (car entry)))
                          class-entries)))

        ;; Check for exceptions
        (let ((exception-entries (alist-get "Exception" index nil nil #'equal)))
          (should exception-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "exception MyException" (car entry)))
                          exception-entries)))

        ;; Check for module types
        (let ((module-type-entries (alist-get "Module Type" index nil nil #'equal)))
          (should module-type-entries)
          (should (cl-some (lambda (entry)
                            (string-match-p "module type MyModuleType" (car entry)))
                          module-type-entries)))))))

(provide 'test-ocaml-imenu)
;;; test-ocaml-imenu.el ends here

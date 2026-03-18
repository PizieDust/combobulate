;;; combobulate-ocaml.el --- ocaml support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Tim McGilchrsit

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

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defmacro combobulate-step (name &rest body) "Wrap BODY as a test step named NAME. If failure occurs, the step name is included in the failure report." (declare (indent 1)) `(condition-case err (progn ,@body) 
  (ert-test-failed 
    (signal (car err) 
      (append (cdr err) (list :step ,name)))))) 

(defgroup combobulate-ocaml nil
  "Configuration switches for OCaml"
  :group 'combobulate
  :prefix "combobulate-ocaml-")

(defun combobulate-ocaml-pretty-print-node-name (node default-name)
  "Pretty printer for OCaml nodes"      ; TODO Fill this in
  default-name)

(defun combobulate-ocaml-imenu-node-p (node)
  "Return t if NODE is a valid imenu node for OCaml."
  (member (treesit-node-type node)
          '("type_definition"
            "exception_definition"
            "external"
            "value_definition"
            "method_definition"
            "instance_variable_definition"
            "module_definition"
            "module_type_definition"
            "class_definition"
            "class_type_definition"
            "include_module"
            "include_module_type"
            "open_module"
            "value_specification")))

(defun combobulate-ocaml-imenu-name-function (node)
  "Return the name of the imenu entry for NODE in OCaml."
  (or
   (pcase (treesit-node-type node)
     ;; For value definitions (let bindings), get the pattern name
     ("value_definition"
      (when-let* ((let-binding (treesit-search-subtree node "let_binding"))
                  (pattern (treesit-node-child-by-field-name let-binding "pattern"))
                  (name-node (or (treesit-search-subtree pattern "value_name")
                                 (treesit-search-subtree pattern "value_pattern"))))
        (concat "let " (treesit-node-text name-node t))))

     ;; For type definitions, get the type name
     ("type_definition"
      (when-let* ((type-binding (treesit-search-subtree node "type_binding"))
                  (name-node (treesit-search-subtree type-binding "type_constructor")))
        (concat "type " (treesit-node-text name-node t))))

     ;; For module definitions, get the module name
     ("module_definition"
      (when-let* ((module-binding (treesit-search-subtree node "module_binding"))
                  (name-node (treesit-search-subtree module-binding "module_name")))
        (concat "module " (treesit-node-text name-node t))))

     ;; For class definitions, get the class name
     ("class_definition"
      (when-let* ((class-binding (treesit-search-subtree node "class_binding"))
                  (name-node (treesit-search-subtree class-binding "class_name")))
        (concat "class " (treesit-node-text name-node t))))

     ;; For exception definitions, get the exception name
     ("exception_definition"
      (when-let ((name-node (treesit-search-subtree node "constructor_name")))
        (concat "exception " (treesit-node-text name-node t))))

     ;; For external definitions, get the name
     ("external"
      (when-let ((name-node (treesit-search-subtree node "value_name")))
        (concat "external " (treesit-node-text name-node t))))

     ;; For module type definitions
     ("module_type_definition"
      (when-let ((name-node (treesit-search-subtree node "module_type_name")))
        (concat "module type " (treesit-node-text name-node t))))

     ;; For value specifications (in .mli files)
     ("value_specification"
      (when-let ((name-node (treesit-search-subtree node "value_name")))
        (concat "val " (treesit-node-text name-node t))))

     ;; For method definitions
     ("method_definition"
      (when-let ((name-node (treesit-search-subtree node "method_name")))
        (concat "method " (treesit-node-text name-node t))))

     ;; For class type definitions
     ("class_type_definition"
      (when-let* ((class-type-binding (treesit-search-subtree node "class_type_binding"))
                  (name-node (treesit-search-subtree class-type-binding "class_type_name")))
        (concat "class type " (treesit-node-text name-node t))))

     ;; For include_module
     ("include_module"
      (when-let ((module-node (treesit-node-child-by-field-name node "module")))
        (concat "include " (treesit-node-text module-node t))))

     ;; For include_module_type
     ("include_module_type"
      (when-let ((sig-node (treesit-search-subtree node "signature")))
        "include <sig>")))
   ;; Fallback to just the first text content we can find
   "Anonymous"))

(eval-and-compile

  ;; Define combobulate support for *.ml files (implementations)
  (defconst combobulate-ocaml-definitions
    ;; ... DEFINITIONS ...
    ;; Context nodes is a list of node types that are contextual in your language.
    ;; e.g. constant values, identifiers and type identifiers
    '((context-nodes
       '("false" "true" "number" "class_name" "value_name" "module_name" "module_type_name" "field_name" "false" "true"))

      ;; The function to use to indent a region. Defaults to indent-region which
      ;; is fine if you're not using a whitespace-sensitive language.
      (envelope-indent-region-function #'indent-region)

      ;; You can pretty print the display of node names in many places in
      ;; Combobulate. Use your own function here to do this.
      (pretty-print-node-name-function #'combobulate-ocaml-pretty-print-node-name)

      ;; Plausible separators between items, probably comma and semi-colon?
      (plausible-separators '(";" "," "|" "struct" "sig" "end" "begin" "{" "}"))

      (display-ignored-node-types '("let" "module" "struct" "sig" "external" "val" "type" "class" "exception" "open" "include"))

      ;; This is a list of procedures that determine what a defun is.
      ;; In OCaml it is any _definition node. Select the defun using C-M-h
      (procedures-defun
       '((:activation-nodes ((:nodes (
                                      "type_definition"
                                      "exception_definition"
                                      "external"
                                      "value_definition"
                                      "method_definition"
                                      "instance_variable_definition"
                                      "module_definition"
                                      "module_type_definition"
                                      "class_definition"))))))

      ;; Logical navigation is bound to M-a and M-e. These commands move to the
      ;; next logical node after or before point. It defaults to all possible nodes
      ;; types, and this is usually the right default.
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))

      ;; Sibling navigation really means picking the right siblings as point will
      ;; often intersect many nodes, each having its own siblings. Sibling navigation
      ;; is essential to get right and it must work consistently and everywhere.
      ;; You navigate by siblings with C-M-n and C-M-p.
      (procedures-sibling
       '(
         ;; Instead of typing out all possible node types that you want to
         ;; navigate by, it's often easier to use their common parent node and
         ;; ask Combobulate to give you all the node types that can appear in it:

         (:activation-nodes
          ((:nodes (
            "variant_declaration" "record_declaration" "list_expression" "cons_expression" "field_get_expression" "function_type")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("value_definition" "application_expression" "let_expression")
            :has-parent ("let_expression")))
          :selector
          (:choose parent
          :match-children t))

          (:activation-nodes
          ((:nodes ("type_variable" "parameter" "value_path" "add_operator" "mult_operator" "pow_operator" "rel_oparator" "concat_oparator" "or_oparator" "and_operator" "assign_operator" "infix_expression" "type_constructor_path" "field_declaration" "tag_specification" "match_case" "field_expression"))
          (:nodes ((rule "signature") (rule "structure")) 
            :has-ancestor ("module_definition")))
          :selector
          (:choose node
          :match-siblings t))

          (:activation-nodes
          ((:nodes ("signature" "structure" "module_name" "module_path" "module_type_constraint") :has-ancestor ("module_definition" "module_type_definition" "package_expression"))
          (:nodes (
            "attribute" "field_declaration" "function_expression"
            (rule "function_type")
            (rule "attribute_payload")
            (rule "record_expression")
            (rule "object_expression")
            (rule "constructor_declaration")
            (rule "class_binding")
            (rule "class_application")
            (rule "type_binding")
            (rule "method_definition")
            (rule "structure")
            (rule "signature")
            (irule "signature")
            (irule "structure")
            (rule "_class_field_specification")
            (rule "_sequence_expression")
            (rule "_signature_item")
            (rule "_structure_item"))
                   ))
          :selector (:choose
                     node
                     :match-siblings t))

          (:activation-nodes
          ((:nodes (
            (rule "compilation_unit"))))
          :selector (:choose node :match-children t))
         ))

      ;; This is a list of procedures that determine the parent-child relationship
      ;; between nodes. Specifically C-M-d and C-M-u.
      (procedures-hierarchy
       '(
        ;; KNOWN LIMITATION: Class hierarchy navigation in OCaml has a fundamental issue.
        ;; The `:match-rules` and `:discard-rules` selectors do not work as expected for OCaml.
        ;; This means we cannot skip intermediate nodes like `parameter` when navigating through
        ;; class definitions. The navigation will go through ALL children in tree order:
        ;;   class → class_definition → class_binding → class_name → parameter → parameter → object_expression
        ;;
        ;; The desired navigation path would be:
        ;;   class → class_name → object → instance_variable_definition
        ;;
        ;; But due to the selector limitations, navigation visits parameter nodes before reaching object_expression.
        ;;
        ;; This appears to be either:
        ;; 1. A limitation in how combobulate processes selector rules for certain grammars
        ;; 2. An issue specific to the OCaml tree-sitter grammar structure
        ;; 3. A bug in the combobulate procedure matching logic
        ;;
        ;; Multiple attempts were made to fix this:
        ;; - Using `:match-rules` with explicit node types - ignored
        ;; - Using `:discard-rules` to skip parameters - ignored
        ;; - Using `:match-siblings` vs `:match-children` - no difference
        ;; - Using `:position at` - no effect
        ;; - Adding activation rules for specific node types (class_name) - no effect
        ;; - Removing conflicting rules from catch-all - no effect
        ;;
        ;; The production rules for `class_binding` are:
        ;;   :*unnamed*: ("abstract_type" "item_attribute" "parameter" "class_function_type" "type_variable")
        ;;   :body: ("class_function" "let_open_class_expression" "let_class_expression" "class_application")
        ;;   :name: ("class_name")
        ;;
        ;; Note that "object_expression" does not appear in these rules at all, which may be
        ;; part of the problem.

        ;; Navigate from class_definition through its children

        (:activation-nodes
         ((:nodes ("field_get_expression"))(:nodes ((rule "polymorphic_variant_type"))))
         :selector (:choose node
                            :match-children t))

        (:activation-nodes
         ((:nodes ("object_expression" (rule "class_definition") (rule "object_expression") (rule "class_binding"))))
         :selector (:choose node
                            :match-children (:discard-rules ("tag_specification"))))

        ;; Catch-all for structural nodes - match all their children
        (:activation-nodes
        ((:nodes ("parameter" "value_path")))
        :selector
        (:choose node
        :match-children t))

        (:activation-nodes
          (
            (:nodes ("signature" "structure" "module_name" "module_path") :has-ancestor ("module_definition" "module_type_definition" "package_expression"))
            (:nodes (
            (rule "module_definition")
            (rule "record_declaration")
            (rule "attribute_payload")
            (rule "function_type")
            (irule "function_type")
            (irule "set_expression")
            (irule "infix_expression")
            (rule "constructor_declaration")
            ;; Removed class_application to avoid conflict with class_binding rule above
            (rule "type_binding")
            (rule "method_definition")
            (irule "value_path")
            (irule "signature")
            (irule "structure")
            (rule "_signature_item")
            (rule "_structure_item"))
                   ))
          :selector (:choose
                     node
                     :match-children t))

         ;; This should be equivalent to listing everything in "compilation_unit"
         (:activation-nodes
          ((:nodes (rule "compilation_unit")))
          :selector (:choose node :match-children t))

         ))
    ))

  ;; Define combobulate support for *.mli files (interfaces)
  ;; Interface files have a subset of constructs compared to implementation files
  (defconst combobulate-ocaml-interface-definitions
    '((context-nodes
       '("false" "true" "number" "class_name" "value_name" "module_name" "module_type_name" "field_name"
         ;; Keywords and punctuation that should be skipped during navigation
         "module" "sig" "end" "val" "type" "class" "exception" "open" "external"
         ":" ";" "," "|" "->" "=" "(" ")" "[" "]" "{" "}"))

      (envelope-indent-region-function #'indent-region)
      (pretty-print-node-name-function #'combobulate-ocaml-pretty-print-node-name)
      (plausible-separators '(";" ",", "|"))

      ;; Interface files only have specifications, not definitions
      (procedures-defun
       '((:activation-nodes ((:nodes (
                                      "type_definition"
                                      "exception_definition"
                                      "value_specification"  ; val declarations
                                      "module_definition"
                                      "module_type_definition"
                                      "class_definition"
                                      "class_type_definition"  ; class type specs
                                      "include_module"
                                      "include_module_type"
                                      "open_module"))))))

      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))

      ;; Sibling navigation - same as implementation files
      (procedures-sibling
       '(
         (:activation-nodes
          ((:nodes (
            "variant_declaration" "record_declaration")))
          :selector (:choose node :match-children t))

          (:activation-nodes
          ((:nodes (
            ;; Top-level interface items - using rule to get all _signature_item types
            (rule "_signature_item")
            ;; Other nodes
            "attribute" "field_declaration"
            (rule "attribute_payload")
            (rule "object_expression")
            (rule "constructor_declaration")
            (rule "class_binding")
            (rule "type_binding")
            (rule "signature")
            (irule "signature")
            (rule "_class_field_specification"))))
          :selector (:choose
                     node
                     :match-siblings t))

          (:activation-nodes
          ((:nodes (
            (rule "compilation_unit")
                    )))
          :selector (:choose node :match-children t))
         ))

      ;; Hierarchy navigation - simplified for interfaces
      (procedures-hierarchy
       '(
        ;; From module_name, navigate up to parent then to signature
        (:activation-nodes
          ((:nodes ("module_name")))
          :selector (:choose parent
                             :match-children (:match-rules ("signature"))))

        ;; From sig keyword, navigate to sibling signature items (type_definition, value_specification, etc.)
        (:activation-nodes
          ((:nodes ("sig")))
          :selector (:choose node
                             :match-siblings (:match-rules ((rule "_signature_item")))))

        ;; From method keyword, navigate to parent then to method_name
        (:activation-nodes
          ((:nodes ("method")))
          :selector (:choose parent
                             :match-children (:match-rules ("method_name"))))

        ;; For signature and other structural nodes, match their children
        (:activation-nodes
          ((:nodes (
            (rule "attribute_payload")
            (rule "object_expression")
            (rule "constructor_declaration")
            (rule "class_binding")
            "class_body_type"
            "method_specification"
            (rule "type_binding")
            (rule "signature")
            (irule "signature")
            (rule "_signature_item"))
                   ))
          :selector (:choose
                     node
                     :match-children t))

         (:activation-nodes
          ((:nodes (rule "compilation_unit")))
          :selector (:choose node :match-children t))
         ))
    )))

;; Note: OCaml has two tree-sitter grammars: 'ocaml' for .ml files and
;; 'ocaml_interface' for .mli files. The tuareg-treesit-bridge automatically
;; creates the appropriate parser based on the file extension.
;;
;; We register both as separate "languages" in Combobulate terms with their own
;; rule sets. Interface files (.mli) have a more restricted set of top-level
;; constructs (specifications rather than implementations). The :language parameter
;; matches what tree-sitter uses, while the :name is used for Emacs Lisp symbol names.
(define-combobulate-language
 :name ocaml
 :language ocaml
 :major-modes (tuareg-mode)
 :custom combobulate-ocaml-definitions
 :setup-fn combobulate-ocaml-setup)

(define-combobulate-language
 :name ocaml-interface
 :language ocaml_interface
 :major-modes (tuareg-mode)
 :custom combobulate-ocaml-interface-definitions
 :setup-fn combobulate-ocaml-setup)

(defun combobulate-ocaml-setup (_)
  "Setup function for OCaml mode with Combobulate."
  ;; Configure imenu for OCaml files
  (setq-local treesit-simple-imenu-settings
              `(("Type" "type_definition" nil combobulate-ocaml-imenu-name-function)
                ("Module" "module_definition" nil combobulate-ocaml-imenu-name-function)
                ("Class" "class_definition" nil combobulate-ocaml-imenu-name-function)
                ("Class Type" "class_type_definition" nil combobulate-ocaml-imenu-name-function)
                ("Value" "value_definition" nil combobulate-ocaml-imenu-name-function)
                ("Function" "value_definition" nil combobulate-ocaml-imenu-name-function)
                ("Exception" "exception_definition" nil combobulate-ocaml-imenu-name-function)
                ("External" "external" nil combobulate-ocaml-imenu-name-function)
                ("Module Type" "module_type_definition" nil combobulate-ocaml-imenu-name-function)
                ("Include" "include_module" nil combobulate-ocaml-imenu-name-function)
                ("Include Sig" "include_module_type" nil combobulate-ocaml-imenu-name-function)
                ("Value Spec" "value_specification" nil combobulate-ocaml-imenu-name-function)))
  ;; Use tree-sitter based imenu (treesit-simple-imenu creates the index from the settings above)
  (setq-local imenu-create-index-function #'treesit-simple-imenu)
  (setq-local combobulate-navigate-down-into-lists nil))

(provide 'combobulate-ocaml)
;;; combobulate-ocaml.el ends here

;;; e-tools-test-support.el --- Work-backed tool test fixtures -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'e-tools)

(cl-defun e-tools-test-register
    (registry &key name description parameters handler start work metadata
              blocking-class invocation-only)
  "Register fixture NAME through the work-only production contract.
HANDLER and START describe test fixture behavior only; both are converted to
canonical `e-work-spec' values before reaching `e-tools-register'."
  (ignore invocation-only)
  (e-tools-register
   registry
   :name name
   :description description
   :parameters parameters
   :metadata metadata
   :blocking-class blocking-class
   :work (or work
             (and start
                  (e-tools-callback-work
                   (format "test.tool.%s.callback" name) start))
             (and handler
                  (e-tools-cheap-work
                   (format "test.tool.%s.cheap" name) handler)))))

(provide 'e-tools-test-support)

;;; e-tools-test-support.el ends here

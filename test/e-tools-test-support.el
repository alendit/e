;;; e-tools-test-support.el --- Work-backed tool test fixtures -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'e-tools)
(require 'e-work)

(cl-defun e-tools-test--callback-work
    (id start &key description (owner 'tools))
  "Return a test-local callback-backed cooperative work fixture.
START receives the ordinary callback keyword arguments used by tool tests.
This deliberately lives under `test/' because production tools use only the
canonical `e-work' registration contract."
  (e-work-spec-create
   :id id
   :description (or description (format "Run callback-backed tool %s." id))
   :execution 'cooperative
   :interactive-policy 'async
   :owner owner
   :runner
   (lambda (handle arguments _context)
     (cl-labels
         ((adopt-request
           (request)
           (when request
             (setf (e-work-handle-metadata handle)
                   (append (e-work-handle-metadata handle)
                           (list :request request)))
             (when (e-tools-request-p request)
               (setf (e-tools-request-metadata request)
                     (append (e-tools-request-metadata request)
                             (list :work-id (e-work-handle-id handle)
                                   :work-handle handle))))
             (setf (e-work-handle-cancel-function handle)
                   (lambda (_handle)
                     (e-tools-cancel-request request)
                     t)))))
       (let ((request
              (e-tools--apply-start-with-optional-event
               start
               (list :arguments arguments
                     :on-done (lambda (value) (e-work-finish handle value))
                     :on-error (lambda (err) (e-work-fail handle err))
                     :on-request-start #'adopt-request)
               (lambda (_type payload) (e-work-progress handle payload)))))
         (adopt-request request)
         (when request
           (when (e-tools-request-p request)
             (setf (e-tools-request-metadata request)
                   (append (e-tools-request-metadata request)
                           (list :work-id (e-work-handle-id handle)
                                 :work-handle handle))))
           (setf (e-work-handle-cancel-function handle)
                 (lambda (_handle)
                   (e-tools-cancel-request request)
                   t))
           (setf (e-work-handle-metadata handle)
                 (append (e-work-handle-metadata handle)
                         (list :underlying-request-metadata
                               (and (e-tools-request-p request)
                                    (e-tools-request-metadata request))))))
         :deferred)))))

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
                  (e-tools-test--callback-work
                   (format "test.tool.%s.callback" name) start))
             (and handler
                  (e-tools-cheap-work
                   (format "test.tool.%s.cheap" name) handler)))))

(provide 'e-tools-test-support)

;;; e-tools-test-support.el ends here

;;; e-chat-activity.el --- Chat activity presentation owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns progress, provider-round/tool/action projections, transient activity
;; blocks, redraw scheduling, and activity replay presentation.  Durable turn
;; semantics remain in the harness/session services.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-chat-service)
(require 'e-chat-surface)
(require 'e-chat-transcript)
(require 'e-tools)
(require 'e-ui-work)
(require 'e-work)

(defcustom e-chat-tool-activity-preview-bytes 4096
  "Maximum UTF-8 bytes of a tool result retained in chat activity UI."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-progress-interval 0.6
  "Seconds between active assistant progress indicator frames."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-activity-redraw-delay 0.05
  "Seconds to coalesce running activity redraws."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-activity-redraw-large-block-chars 8000
  "Transient block size above which activity redraws are throttled harder.
Once the visible running-status region exceeds this many characters, its
coalescing delay is multiplied by `e-chat-activity-redraw-large-block-factor'
so a big, rapidly-updating block repaints less often."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-activity-redraw-large-block-factor 4.0
  "Delay multiplier applied to activity redraws of a large transient block.
See `e-chat-activity-redraw-large-block-chars'."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-activity-reasoning-visible-line-limit 3
  "Maximum non-empty reasoning lines shown in compact activity summaries.
The default shows a bounded provider-supplied summary.  Explicit zero hides
provider-summary previews, while a positive value opts into a bounded live
preview; complete provider-summary history remains available from explicitly
expanded response details."
  :type 'natnum
  :group 'e-chat)

(defcustom e-chat-live-activity-round-limit 12
  "Maximum recent provider rounds shown in a live activity block.
Earlier rounds remain in the turn record and settled expandable details."
  :type '(integer 1)
  :group 'e-chat)

(defcustom e-chat-session-replay-activity-event-limit 64
  "Maximum recent activity events reconstructed in a chat buffer.
Replay first restricts activity to turns represented by the bounded message
tail plus any currently active turn, then retains at most this many newest
events.  Durable board history is not changed."
  :type '(integer 1)
  :group 'e-chat)

(when (equal e-chat-progress-interval 0.35)
  (setq e-chat-progress-interval 0.6))

(declare-function e-chat-transcript-presentation-turn-id "e-chat-transcript")
(declare-function e-chat-transcript-observed-turn-id "e-chat-transcript")
(declare-function e-chat-transcript-event-selected-participant-p "e-chat-transcript")
(declare-function e-chat-transcript-message-selected-participant-p "e-chat-transcript")
(declare-function e-chat-surface-refresh-ui-work-diagnostics "e-chat-surface")
(declare-function e-chat-transcript-project-activity "e-chat-transcript")
(declare-function e-chat-transcript-remove-activity "e-chat-transcript")
(declare-function e-chat-transcript-update-message-details "e-chat-transcript")
(declare-function e-chat-surface-capture-output-tail-windows "e-chat-surface")
(declare-function e-chat-surface-restore-output-tail-windows "e-chat-surface")
(declare-function e-chat-surface-redraw-visible-p "e-chat-surface")
(declare-function e-chat-surface-set-status "e-chat-surface")
(declare-function e-chat-transcript-render-activity-notice "e-chat-transcript")
(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")
(declare-function e-chat-surface-capture-running-status-display-state "e-chat-surface")
(declare-function e-chat-surface-restore-running-status-display-state "e-chat-surface")
(declare-function e-chat-surface-set-running-status-bounds "e-chat-surface")
(declare-function e-chat-surface-output-follow-position "e-chat-surface")
(declare-function e-chat-surface-set-redraw-visible "e-chat-surface")
(declare-function e-chat-surface-without-recenter "e-chat-surface")
(declare-function e-chat-surface-transcript-p "e-chat-surface")

(defvar e-chat-harness nil)
(defvar e-chat-session-id nil)
(defvar e-chat-session-replay-message-limit)
(defconst e-chat-activity--progress-glyphs
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Glyphs used for the active assistant progress indicator.")
(defun e-chat-activity--chat-buffer-p ()
  "Return non-nil when the current buffer is an attached chat surface."
  (or (e-chat-surface-transcript-p)
      (e-chat-surface-composer-p)
      (and (boundp 'e-chat-harness)
           (boundp 'e-chat-session-id)
           e-chat-harness
           e-chat-session-id)))

(defun e-chat-activity--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-chat-activity--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when profiling is enabled."
  (if (e-chat-activity--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defvar-local e-chat-activity--progress-turn-id nil
  "Turn id currently represented by the assistant progress indicator.")

(defvar-local e-chat-activity--progress-frame 0
  "Current active assistant progress indicator frame.")

(defvar-local e-chat-activity--pending-activity-redraw-turn-id nil
  "Turn id with a scheduled running activity redraw.")

(defvar-local e-chat-activity--pending-activity-redraw-handle nil
  "UI work handle scheduled to redraw running activity.")

(defvar-local e-chat-activity--pending-activity-redraw-kind nil
  "Kind of pending activity redraw, either `activity' or `progress'.")

(defvar-local e-chat-activity--pending-activity-redraw-generation nil
  "Generation token for the pending activity redraw.")

(defvar-local e-chat-activity--activity-redraw-generation 0
  "Latest activity redraw generation for stale timer detection.")

(defvar-local e-chat-activity--activity-redraw-running nil
  "Non-nil while this buffer is executing an activity redraw.")

(defvar-local e-chat-activity--deferred-activity-redraw nil
  "Latest deferred activity redraw withheld from this chat buffer.")

(defvar-local e-chat-activity--assistant-streaming-p nil
  "Non-nil after streaming activity has been displayed for a provider round.")

(defvar-local e-chat-activity--progress-interval-handle nil
  "UI work interval advancing the active assistant progress indicator.")

(defvar-local e-chat-activity--progress-next-tick-time nil
  "Expected `float-time' of the next assistant progress interval tick.")

(defvar-local e-chat-activity--running-status-rendered-hook nil
  "Abnormal hook run after each running-status redraw.")

(defvar-local e-chat-activity--rendered-turn-id nil
  "Turn id currently represented by the activity projection.

The transcript owns the projection's markers and block metadata.  This scalar
belongs to activity because it answers an activity question (which local
activity state is currently being displayed) without reading transcript
representation state back through a port.")

(defvar-local e-chat-activity--rendered-activity-size 0
  "Character count returned by the last activity projection.")

(defvar-local e-chat-activity--turn-registry nil
  "Activity-owned turn state keyed by presentation turn id.

The transcript owner keeps only durable entry and navigation projection state.
Provider rounds, transient entries, timing, failures, and redraw metadata live
here so activity changes do not mutate a transcript record.")

(defun e-chat-activity--ensure-turn-registry ()
  "Ensure the activity registry exists for the current chat buffer."
  (unless (hash-table-p e-chat-activity--turn-registry)
    (setq e-chat-activity--turn-registry (make-hash-table :test 'equal)))
  e-chat-activity--turn-registry)

(defun e-chat-activity--turn-record (turn-id)
  "Return mutable activity state for TURN-ID, creating it when needed."
  (when turn-id
    (let ((registry (e-chat-activity--ensure-turn-registry)))
      (or (gethash turn-id registry)
          (let ((record (list :id turn-id
                              :started-at nil
                              :ended-at nil
                              :has-provider-activity nil
                              :activity-records nil
                              :intermittent-entries nil
                              :failure-error nil
                              :failure-details nil
                              :failure-rendered nil
                              :pending-hook-summary nil
                              :message-details nil
                              :assistant-output-rendered nil
                              :final-rendered nil
                              :activity-rendered nil
                              :details-text nil)))
            (puthash turn-id record registry)
            record)))))

(defun e-chat-activity--existing-turn-record (turn-id)
  "Return activity state for TURN-ID, or nil when not initialized."
  (when (and turn-id (hash-table-p e-chat-activity--turn-registry))
    (gethash turn-id e-chat-activity--turn-registry)))

(defun e-chat-activity--set-turn-time (turn-id field value)
  "Set activity timing FIELD to VALUE for TURN-ID."
  (when (and turn-id value)
    (plist-put (e-chat-activity--turn-record turn-id) field value)))

(defun e-chat-activity--format-time-value (value)
  "Return VALUE as a compact display string."
  (cond
   ((numberp value)
    (format-time-string "%Y-%m-%d %H:%M:%S UTC"
                        (seconds-to-time value)
                        t))
   (value (format "%s" value))
   (t "unknown")))

(defun e-chat-activity--time-seconds (value)
  "Return VALUE as seconds when it can be parsed as a time."
  (cond
   ((numberp value) value)
   ((stringp value)
    (condition-case nil
        (float-time (date-to-time value))
      (error nil)))
   (t nil)))

(defun e-chat-activity--format-duration (started-at ended-at)
  "Return duration between STARTED-AT and ENDED-AT."
  (let ((started-seconds (e-chat-activity--time-seconds started-at))
        (ended-seconds (e-chat-activity--time-seconds ended-at)))
    (if (and started-seconds ended-seconds)
        (let* ((seconds (max 0 (truncate (- ended-seconds started-seconds))))
               (minutes (/ seconds 60))
               (remaining (% seconds 60)))
          (format "%dmin %dsec" minutes remaining))
      "unknown")))

(defun e-chat-activity--current-time-seconds ()
  "Return the activity owner's current time as float seconds."
  (float-time))

(defun e-chat-activity--indent-detail-text (text)
  "Return TEXT with each line indented for expanded turn details."
  (concat "  " (replace-regexp-in-string "\n" "\n  " (string-trim-right text))))

(defun e-chat-activity--intermittent-entry-text (entry)
  "Return display text for intermittent turn ENTRY."
  (format "%s\n%s"
          (plist-get entry :title)
          (plist-get entry :content)))

(defun e-chat-activity--activity-tool-count-text (count)
  "Return collapsed display text for COUNT tool invocations."
  (format "%d tool call%s" count (if (= count 1) "" "s")))

(defun e-chat-activity--activity-action-count-text (count)
  "Return collapsed display text for COUNT action invocations."
  (format "%d action%s" count (if (= count 1) "" "s")))

(defun e-chat-activity--context-curation-count (record)
  "Return the number of distinct context-curation entries in RECORD."
  (cl-count-if
   (lambda (entry) (eq (plist-get entry :kind) 'context-curated))
   (plist-get record :intermittent-entries)))

(defun e-chat-activity--activity-records (record)
  "Return semantic activity records for RECORD."
  (plist-get record :activity-records))

(defun e-chat-activity--append-activity-record (record activity-record)
  "Append semantic ACTIVITY-RECORD to RECORD."
  (plist-put record
             :activity-records
             (append (e-chat-activity--activity-records record)
                     (list activity-record)))
  activity-record)

(defun e-chat-activity--last-round-record (record)
  "Return RECORD's latest provider round record."
  (car (last (e-chat-activity--activity-records record))))

(defun e-chat-activity--active-round-record (record)
  "Return RECORD's active provider round record."
  (cl-find-if
   (lambda (activity-record)
     (and (eq (plist-get activity-record :kind) 'round)
          (eq (plist-get activity-record :status) 'active)))
   (reverse (e-chat-activity--activity-records record))))

(defun e-chat-activity--round-record-for-child (record)
  "Return semantic round record that should own the next child event."
  (or (e-chat-activity--active-round-record record)
      (e-chat-activity--last-round-record record)))

(defun e-chat-activity--append-round-reasoning (record content &optional append)
  "Append reasoning CONTENT to RECORD's current round.
When APPEND is non-nil, merge CONTENT into the previous reasoning child."
  (when-let ((round (and content
                         (not (string-empty-p content))
                         (e-chat-activity--round-record-for-child record))))
    (let* ((reasoning (plist-get round :reasoning))
           (last-reasoning (car (last reasoning))))
      (if (and append last-reasoning)
          (plist-put last-reasoning
                     :content
                     (concat (plist-get last-reasoning :content) content))
        (plist-put round
                   :reasoning
                   (append reasoning
                           (list (list :kind 'reasoning
                                       :round (plist-get round :round)
                                       :content content))))))))

(defun e-chat-activity--current-round-tool-batch (round)
  "Return ROUND's current tool batch, creating it when needed."
  (or (car (last (plist-get round :tool-batches)))
      (let ((batch (list :kind 'tool-batch
                         :round (plist-get round :round)
                         :items nil)))
        (plist-put round
                   :tool-batches
                   (append (plist-get round :tool-batches)
                           (list batch)))
        batch)))

(defun e-chat-activity--append-round-tool-call (record payload &optional created-at)
  "Append tool call PAYLOAD to RECORD's current round.
CREATED-AT records when the tool started so the running row can tick."
  (when-let ((round (e-chat-activity--round-record-for-child record)))
    (let* ((batch (e-chat-activity--current-round-tool-batch round))
           (items (plist-get batch :items))
           (tool-id (or (plist-get payload :id)
                        (plist-get payload :call-id))))
      (plist-put batch
                 :items
                 (append items
                         (list (list :kind 'tool
                                     :round (plist-get round :round)
                                     :id tool-id
                                     :call (e-chat-activity--format-tool-call payload)
                                     :call-payload payload
                                     :started-at (or created-at
                                                     (e-chat-activity--current-time-seconds))
                                     :output nil)))))))

(defun e-chat-activity--tool-finished-id (payload)
  "Return the tool id associated with tool-finished PAYLOAD."
  (let ((tool-call (plist-get payload :tool-call)))
    (or (plist-get payload :id)
        (plist-get payload :call-id)
        (plist-get tool-call :id)
        (plist-get tool-call :call-id))))

(defun e-chat-activity--round-tool-items (round)
  "Return all tool items recorded for ROUND."
  (apply #'append
         (mapcar (lambda (batch)
                   (plist-get batch :items))
                 (plist-get round :tool-batches))))

(defun e-chat-activity--find-round-tool-item (record tool-id)
  "Return semantic tool item matching TOOL-ID in RECORD."
  (when tool-id
    (cl-loop for round in (reverse (e-chat-activity--activity-records record))
             thereis
             (cl-find-if
              (lambda (item)
                (equal (plist-get item :id) tool-id))
              (e-chat-activity--round-tool-items round)))))

(defun e-chat-activity--latest-incomplete-tool-item (record)
  "Return RECORD's latest tool item without output."
  (cl-loop for round in (reverse (e-chat-activity--activity-records record))
           thereis
           (cl-find-if
            (lambda (item)
              (not (plist-get item :output)))
            (reverse (e-chat-activity--round-tool-items round)))))

(defun e-chat-activity--string-byte-prefix (text max-bytes)
  "Return TEXT prefix limited to MAX-BYTES UTF-8 bytes."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-chat-activity--tool-result-display-text (result)
  "Return compact chat activity display text for tool RESULT."
  (let* ((content (if (e-tools-result-p result)
                      (plist-get result :content)
                    result))
         (max-bytes (max 0 e-chat-tool-activity-preview-bytes))
         (preview-data (e-tools-result-content-preview content max-bytes))
         (preview (plist-get preview-data :text))
         (shown-bytes (plist-get preview-data :shown-bytes))
         (original-bytes (and (stringp content) (string-bytes content)))
         (truncated (or (plist-get preview-data :truncated)
                        (and original-bytes (> original-bytes max-bytes))))
         (metadata (and (e-tools-result-p result)
                        (plist-get result :metadata)))
         (uri (or (plist-get metadata :tmp-uri)
                  (plist-get metadata :full-output-path))))
    (if truncated
        (string-trim-right
         (format "%s

[Tool result preview truncated: showing first %d%s bytes%s]"
                 preview
                 shown-bytes
                 (if original-bytes (format " of %d" original-bytes) "")
                 (if uri (format ". Full output: %s" uri) "")))
      preview)))

(defun e-chat-activity--complete-round-tool-result (record payload &optional finished-at)
  "Attach tool result PAYLOAD to the matching semantic tool item in RECORD.
FINISHED-AT records when the tool completed so the settled row can show
how long it ran."
  (let* ((tool-id (e-chat-activity--tool-finished-id payload))
         (item (or (e-chat-activity--find-round-tool-item record tool-id)
                   (e-chat-activity--latest-incomplete-tool-item record))))
    (when item
      (plist-put item
                 :finished-at (or finished-at
                                  (e-chat-activity--current-time-seconds)))
      (plist-put item
                 :output
                 (e-chat-activity--tool-result-display-text
                  (plist-get payload :result))))))

(defun e-chat-activity--record-round-tool-progress (record payload)
  "Attach streaming tool progress PAYLOAD to a semantic tool item in RECORD."
  (let* ((tool-id (or (plist-get payload :tool-call-id)
                      (plist-get payload :id)
                      (plist-get payload :call-id)))
         (item (or (e-chat-activity--find-round-tool-item record tool-id)
                   (e-chat-activity--latest-incomplete-tool-item record))))
    (when item
      (plist-put item :progress payload))))

(defun e-chat-activity--round-tool-progress-text (round)
  "Return compact output progress text for ROUND, or nil."
  (when-let* ((item (cl-find-if
                     (lambda (candidate)
                       (plist-get candidate :progress))
                     (reverse (e-chat-activity--round-tool-items round))))
              (bytes (plist-get (plist-get item :progress) :bytes)))
    (format "%s bytes output" bytes)))

(defun e-chat-activity--round-tool-count (round)
  "Return number of tool calls recorded for ROUND."
  (length (e-chat-activity--round-tool-items round)))

(defun e-chat-activity--activity-record-tool-count (record)
  "Return number of tool calls recorded in semantic RECORD activity."
  (apply #'+
         (mapcar #'e-chat-activity--round-tool-count
                 (e-chat-activity--activity-records record))))

(defun e-chat-activity--normalize-round-status (status)
  "Return presentation round status for provider STATUS."
  (pcase status
    ((or 'error "error" 'attempt-failed "attempt-failed") 'attempt-failed)
    ((or 'retrying "retrying") 'retrying)
    ((or 'failed "failed") 'failed)
    ((or 'cancelled "cancelled") 'cancelled)
    ((or 'active "active" 'started "started") 'active)
    (_ 'done)))

(defun e-chat-activity--thought-content (status started-at ended-at &optional active-at)
  "Return thought line text for STATUS from STARTED-AT to ENDED-AT.
ACTIVE-AT is used for active thinking duration."
  (pcase (e-chat-activity--normalize-round-status status)
    ('active
     (format "%s Thinking for %s"
             (e-chat-activity--progress-dots)
             (e-chat-activity--format-duration
              started-at
              (or active-at (e-chat-activity--current-time-seconds)))))
    ('failed
     (format "Thought failed after %s"
             (e-chat-activity--format-duration started-at ended-at)))
    ('attempt-failed
     (format "Provider attempt failed after %s"
             (e-chat-activity--format-duration started-at ended-at)))
    ('cancelled
     (format "Thought cancelled after %s"
             (e-chat-activity--format-duration started-at ended-at)))
    (_
     (format "Thought for %s"
             (e-chat-activity--format-duration started-at ended-at)))))

(defun e-chat-activity--semantic-tool-items (items)
  "Return display tool-list items for semantic tool ITEMS."
  (mapcar
   (lambda (item)
     (list :id (plist-get item :id)
           :name (e-chat-activity--tool-item-name item)
           :call (plist-get item :call)
           :output (plist-get item :output)))
   items))

(defun e-chat-activity--round-thought-text (round)
  "Return visible thought text for semantic ROUND."
  (if (eq (e-chat-activity--normalize-round-status (plist-get round :status))
          'retrying)
      (concat
       (format "Provider attempt failed after %s; retry %s in %.0fsec"
               (e-chat-activity--format-duration
                (plist-get round :started-at)
                (plist-get round :ended-at))
               (or (plist-get round :retry-attempt) 1)
               (or (plist-get round :retry-backoff-seconds) 0))
       (when-let ((error-message (plist-get round :error)))
         (format "\nError: %s" error-message)))
    (e-chat-activity--thought-content
     (plist-get round :status)
     (plist-get round :started-at)
     (plist-get round :ended-at)
     (when (eq (e-chat-activity--normalize-round-status
                (plist-get round :status))
               'active)
       (e-chat-activity--current-time-seconds)))))

(defun e-chat-activity--activity-round-row-text (left &optional right)
  "Return activity round row with LEFT and optional right-side RIGHT text."
  (if (and right (not (string-empty-p right)))
      (concat left
              (propertize
               " "
               'display
               `(space :align-to (- right ,(string-width right))))
              right)
    left))

(defun e-chat-activity--activity-round-visible-reasoning-lines (round &optional complete)
  "Return visible reasoning lines for semantic activity ROUND.
When COMPLETE is non-nil, return the complete explicitly requested detail."
  (let ((lines nil))
    (dolist (reasoning (plist-get round :reasoning))
      (when-let ((content (plist-get reasoning :content)))
        (dolist (line (string-lines content))
          (setq line (string-trim line))
          (unless (string-empty-p line)
            (push line lines)))))
    (setq lines (nreverse lines))
    (let ((limit (and (not complete)
                      e-chat-activity-reasoning-visible-line-limit)))
      (cond
       (complete lines)
       ((and (integerp limit) (= limit 0))
        nil)
       ((and (integerp limit)
             (> limit 0)
             (> (length lines) limit))
        (last lines limit))
       (t
        lines)))))

(defun e-chat-activity--round-running-tool-items (round)
  "Return ROUND's tool items that have started but not yet produced output."
  (cl-remove-if
   (lambda (item)
     (plist-get item :output))
   (e-chat-activity--round-tool-items round)))

(defun e-chat-activity--tool-item-name (item)
  "Return a compact display name for tool ITEM.
When the tool invoked actions (e.g. run_elisp calling `e-actions-call'),
append the single action name or a count when several distinct actions ran."
  (let ((name (or (plist-get (plist-get item :call-payload) :name)
                  (car (split-string (or (plist-get item :call) "") "\n"))
                  "tool"))
        (actions (plist-get item :actions)))
    (cond
     ((null actions) name)
     ((= (length actions) 1) (format "%s (%s)" name (car actions)))
     (t (format "%s (%d actions)" name (length actions))))))

(defun e-chat-activity--round-tool-names-text (round)
  "Return a comma-joined list of the distinct tool names called in ROUND.
Returns nil when ROUND recorded no tool calls."
  (when-let ((names (delete-dups
                     (mapcar #'e-chat-activity--tool-item-name
                             (e-chat-activity--round-tool-items round)))))
    (string-join names ", ")))

(defun e-chat-activity--round-tools-duration-text (round)
  "Return elapsed run time for ROUND's finished tools, or nil.
Spans the earliest tool start to the latest tool finish so the settled
row keeps showing how long the sub-turn's tools ran after they complete.
Returns nil while any tool is still running or when timing is missing."
  (let ((items (e-chat-activity--round-tool-items round)))
    (when (and items
               (not (e-chat-activity--round-running-tool-items round)))
      (let ((started (delq nil (mapcar (lambda (item)
                                         (e-chat-activity--time-seconds
                                          (plist-get item :started-at)))
                                       items)))
            (finished (delq nil (mapcar (lambda (item)
                                          (e-chat-activity--time-seconds
                                           (plist-get item :finished-at)))
                                        items))))
        (when (and started finished)
          (e-chat-activity--format-duration (apply #'min started)
                                   (apply #'max finished)))))))

(defun e-chat-activity--round-running-tools-text (round)
  "Return live running-tool row text for ROUND, or nil when nothing runs.
Shows a spinner, the running tool name (or a count when several run at once),
and how long the oldest running tool has been active."
  (when-let ((running (e-chat-activity--round-running-tool-items round)))
    (let* ((names (delete-dups
                   (mapcar #'e-chat-activity--tool-item-name running)))
           (started (delq nil (mapcar (lambda (item)
                                        (e-chat-activity--time-seconds
                                         (plist-get item :started-at)))
                                      running)))
           (oldest (and started (apply #'min started)))
           (label (if (> (length running) 1)
                      (format "%d tools (%s)"
                              (length running)
                              (string-join names ", "))
                    (car names))))
      (format "%s Running %s%s"
              (e-chat-activity--progress-dots)
              label
              (if oldest
                  (format " for %s"
                          (e-chat-activity--format-duration
                           oldest (e-chat-activity--current-time-seconds)))
                "")))))

(defun e-chat-activity--round-between-steps-text (round)
  "Return live between-step progress text for settled ROUND.
This temporary presentation covers an active turn's gap after the provider
request settles and before a tool or the next provider request starts."
  (when (eq (e-chat-activity--normalize-round-status (plist-get round :status)) 'done)
    (format "%s Working for %s"
            (e-chat-activity--progress-dots)
            (e-chat-activity--format-duration
             (plist-get round :started-at)
             (e-chat-activity--current-time-seconds)))))

(defun e-chat-activity--activity-round-progress-row-text
    (round &optional active-tail)
  "Return the mutable progress/status row for semantic activity ROUND."
  (let* ((running-text (e-chat-activity--round-running-tools-text round))
         ;; While tools are running, replace the frozen \"Thought for ...\"
         ;; left cell with a live spinner naming the running tool and its
         ;; elapsed time.  The shared progress interval already reticks this row.
         (thought (or running-text
                      (and active-tail
                           (e-chat-activity--round-between-steps-text round))
                      (e-chat-activity--round-thought-text round)))
         (tool-count (e-chat-activity--round-tool-count round))
         (tool-text (and (> tool-count 0)
                         (let* ((count-text
                                 (e-chat-activity--activity-tool-count-text tool-count))
                                (names-text (e-chat-activity--round-tool-names-text round))
                                (progress-text
                                 (e-chat-activity--round-tool-progress-text round))
                                (duration-text
                                 (e-chat-activity--round-tools-duration-text round))
                                ;; Keep the called tool names on the row even
                                ;; after the calls finish, e.g. "1 tool call
                                ;; (bash)".
                                (count-text (if names-text
                                                (format "%s (%s)"
                                                        count-text names-text)
                                              count-text))
                                ;; Keep the run duration on the row after the
                                ;; sub-turn settles, e.g. "1 tool call (bash)
                                ;; for 0min 5sec".
                                (count-text (if duration-text
                                                (format "%s for %s"
                                                        count-text duration-text)
                                              count-text)))
                           (if progress-text
                               (format "%s, %s" count-text progress-text)
                             count-text)))))
    (and thought
         (e-chat-activity--activity-round-row-text thought tool-text))))

(defun e-chat-activity--activity-round-visible-text
    (round &optional active-tail complete)
  "Return visible text for semantic activity ROUND.
When ACTIVE-TAIL is non-nil, ROUND is the latest settled round of the
harness-confirmed active turn.  COMPLETE exposes explicitly requested
reasoning detail rather than the default compact preview."
  (let* ((progress-row
          (e-chat-activity--activity-round-progress-row-text round active-tail))
         (reasoning-lines
          (e-chat-activity--activity-round-visible-reasoning-lines
           round complete))
         (lines (and progress-row (list progress-row))))
    (when reasoning-lines
      (setq lines
            (append reasoning-lines
                    (and progress-row (list "" progress-row)))))
    (when lines
      (string-join lines "\n"))))

(defun e-chat-activity--activity-record-visible-chunks (record &optional complete)
  "Return visible activity chunks for semantic activity RECORD.
When COMPLETE is non-nil, include every durable round."
  (plist-get (e-chat-activity--activity-record-transient-data record complete) :chunks))

(defun e-chat-activity--activity-record-projected-rounds (record &optional complete)
  "Return RECORD rounds for a live or COMPLETE activity projection."
  (let ((rounds (e-chat-activity--activity-records record)))
    (if complete
        rounds
      (last rounds (min (length rounds)
                        (max 1 e-chat-live-activity-round-limit))))))

(defun e-chat-activity--curation-entries-for-round (record round)
  "Return curation entries observed after provider ROUND in RECORD."
  (let ((ordinal (plist-get round :round)))
    (cl-remove-if-not
     (lambda (entry)
       (and (eq (plist-get entry :kind) 'context-curated)
            (equal (plist-get entry :round) ordinal)))
     (plist-get record :intermittent-entries))))

(defun e-chat-activity--activity-record-transient-data (record &optional complete)
  "Return transient render data for semantic activity RECORD.
The returned plist contains visible :text, :chunks, and :rounds.  Unless
COMPLETE is non-nil, the live projection is bounded and begins with an omitted
round summary when needed.  While the latest round is the active turn's mutable
progress tail, :progress-tail-text identifies that semantic tail.  Transcript
offsets and markers are deliberately not part of this owner-to-owner value."
  (let* ((all-rounds (e-chat-activity--activity-records record))
         (rounds (e-chat-activity--activity-record-projected-rounds record complete))
         (omitted-count (- (length all-rounds) (length rounds)))
         (latest (car (last rounds)))
         (active-tail
          (and latest
               (equal (plist-get record :id) e-chat-activity--progress-turn-id)
               (e-chat-activity--service-active-turn-matches-p
                e-chat-activity--progress-turn-id)))
         (separator (concat "\n" e-chat-activity-separator "\n"))
         chunks
         progress-tail-text)
    (when (> omitted-count 0)
      (setq chunks
            (list (format "… %d earlier activity %s omitted"
                          omitted-count
                          (if (= omitted-count 1) "round" "rounds")))))
    ;; A curation is observed at a provider-round boundary.  Entries belonging
    ;; to omitted rounds remain immediately after the omission marker.
    (dolist (entry (plist-get record :intermittent-entries))
      (when (and (eq (plist-get entry :kind) 'context-curated)
                 (plist-get entry :round)
                 (not (cl-some
                       (lambda (round)
                         (equal (plist-get round :round)
                                (plist-get entry :round)))
                       rounds)))
        (setq chunks
              (append chunks
                      (list (e-chat-activity--intermittent-entry-text entry))))))
    ;; Render each visible round followed by the curation entries observed at
    ;; that boundary.  This keeps the mutable current Thinking row last.
    (dolist (round rounds)
      (let* ((latest-active-p (and active-tail (eq round latest)))
             (curations (e-chat-activity--curation-entries-for-round
                         record round))
             ;; Between provider requests the latest completed round is still
             ;; the live tail.  Its summary and boundary activity are stable;
             ;; only the synthetic Working row remains mutable.
             (between-tail-p
              (and latest-active-p
                   (eq (e-chat-activity--normalize-round-status
                        (plist-get round :status))
                       'done))))
        (if between-tail-p
            (progn
              (when-let ((reasoning-lines
                          (e-chat-activity--activity-round-visible-reasoning-lines
                           round complete)))
                (setq chunks
                      (append chunks (list (string-join reasoning-lines "\n")))))
              (dolist (entry curations)
                (setq chunks
                      (append chunks
                              (list (e-chat-activity--intermittent-entry-text
                                     entry)))))
              (when-let ((progress-row
                          (e-chat-activity--activity-round-progress-row-text
                           round t)))
                (setq progress-tail-text progress-row
                      chunks (append chunks (list progress-row)))))
          (when-let ((text (e-chat-activity--activity-round-visible-text
                            round latest-active-p complete)))
            (when latest-active-p
              (setq progress-tail-text
                    (e-chat-activity--activity-round-progress-row-text
                     round t)))
            (setq chunks (append chunks (list text))))
          (dolist (entry curations)
            (setq chunks
                  (append chunks
                          (list (e-chat-activity--intermittent-entry-text
                                 entry))))))))
    ;; Legacy/replayed entries without a round boundary retain their previous
    ;; fallback position after the ordered provider activity.
    (dolist (entry (plist-get record :intermittent-entries))
      (when (and (not (plist-get entry :round))
                 (eq (plist-get entry :kind) 'context-curated))
        (setq chunks
              (append chunks
                      (list (e-chat-activity--intermittent-entry-text entry))))))
    (let ((text (and chunks
                     (concat (string-join chunks separator) "\n\n"))))
      (list :chunks chunks
            :text text
            :rounds rounds
            :omitted-round-count omitted-count
            :progress-tail-text progress-tail-text))))

(defun e-chat-activity--activity-action-visible-chunks (record)
  "Return visible action chunks from RECORD intermittent entries."
  (e-chat-activity--activity-visible-chunks
   (cl-remove-if-not
    (lambda (entry)
      (member (plist-get entry :title) '("Action call" "Action")))
    (plist-get record :intermittent-entries))))

(defun e-chat-activity--activity-visible-chunks (entries)
  "Return visible collapsed activity chunks for intermittent ENTRIES.
Count tool invocations after the reasoning chunk they followed."
  (let ((chunks nil)
        (current nil)
        (tool-count 0))
    (cl-labels
        ((finish-current
          ()
          (when (> tool-count 0)
            (setq current
                  (append current
                          (list (e-chat-activity--activity-tool-count-text
                                 tool-count))))
            (setq tool-count 0))
          (when current
            (push (string-join current "\n") chunks)
            (setq current nil))))
      (dolist (entry entries)
        (let ((title (plist-get entry :title))
              (content (plist-get entry :content)))
          (pcase title
            ((or "Thinking" "Thought")
             (finish-current)
             (when (and content (not (string-empty-p content)))
               (push content chunks)))
            ("Tool call"
             (setq tool-count (1+ tool-count)))
            ("Tool")
            ("Action call"
             (setq current
                   (append (or current nil)
                           (list (format "Action: %s" content)))))
            ("Action")
            ("Context curated"
             (finish-current)
             (push (e-chat-activity--intermittent-entry-text entry) chunks))
            (_
             (finish-current)
             (when (and content (not (string-empty-p content)))
               (setq current (list content)))))))
      (finish-current)
      (nreverse chunks))))

(defun e-chat-activity--activity-tool-count (record)
  "Return number of tool calls recorded for RECORD."
  (or (plist-get record :summary-tool-count)
      (if (e-chat-activity--activity-records record)
          (e-chat-activity--activity-record-tool-count record)
        (cl-count-if
         (lambda (entry)
           (equal (plist-get entry :title) "Tool call"))
         (plist-get record :intermittent-entries)))))

(defun e-chat-activity--activity-action-count (record)
  "Return number of action calls recorded for RECORD."
  (or (plist-get record :summary-action-count)
      (plist-get record :action-count)
      (cl-count-if
       (lambda (entry)
         (equal (plist-get entry :title) "Action call"))
       (plist-get record :intermittent-entries))))

(defun e-chat-activity--record-message-details (turn-id message-id details)
  "Record generic DETAILS for durable MESSAGE-ID in TURN-ID."
  (when-let ((record (e-chat-activity--turn-record turn-id)))
    (let* ((key (or message-id (list 'turn-message turn-id)))
           (current (assoc-delete-all
                     key (plist-get record :message-details))))
      (plist-put record :message-details
                 (if details
                     (append current (list (cons key details)))
                   current))
      (e-chat-activity--refresh-turn-details record))))

(defun e-chat-activity--message-detail-summary-text (record)
  "Return the generic message-detail suffix for RECORD."
  (let (summaries)
    (dolist (entry (plist-get record :message-details))
      (dolist (detail (cdr entry))
        (when-let ((summary (e-message-detail-summary detail)))
          (unless (member summary summaries)
            (setq summaries (append summaries (list summary)))))))
    (if summaries
        (format " (%s)" (string-join summaries ", "))
      "")))

(defun e-chat-activity--message-details-text (details)
  "Return expandable presentation text for generic message DETAILS."
  (when details
    (concat (string-join (mapcar #'e-message-detail-body details) "\n\n")
            "\n")))

(defun e-chat-activity--activity-summary-text (record)
  "Return settled turn summary text for RECORD."
  (when (and (plist-get record :started-at)
             (plist-get record :ended-at)
             (plist-get record :has-provider-activity))
    (let* ((duration
            (if (numberp (plist-get record :summary-duration-seconds))
                (e-chat-activity--format-duration
                 0 (plist-get record :summary-duration-seconds))
              (e-chat-activity--format-duration
               (plist-get record :started-at)
               (plist-get record :ended-at))))
           (tool-count (e-chat-activity--activity-tool-count record))
           (action-count (e-chat-activity--activity-action-count record))
           (curation-count (e-chat-activity--context-curation-count record))
           (tool-text (cond
                       ((= tool-count 0) "")
                       ((= tool-count 1) ", 1 tool call")
                       (t (format ", %d tool calls" tool-count))))
           (action-text (cond
                         ((= action-count 0) "")
                         ((= action-count 1) ", 1 action")
                         (t (format ", %d actions" action-count))))
           (curation-text (cond
                           ((= curation-count 0) "")
                           ((= curation-count 1) ", 1 curation")
                           (t (format ", %d curations" curation-count))))
           (detail-text (e-chat-activity--message-detail-summary-text record)))
      (format "Turn took %s%s%s%s%s."
              duration tool-text action-text curation-text detail-text))))

(defun e-chat-activity--activity-expanded-text (record)
  "Return expanded per-line activity history for RECORD."
  (if (e-chat-activity--activity-records record)
      (when-let ((chunks (append (e-chat-activity--activity-record-visible-chunks
                                  record t)
                                 (e-chat-activity--activity-action-visible-chunks record))))
        (when chunks
          (concat (string-join
                   chunks
                   (concat "\n" e-chat-activity-separator "\n"))
                  "\n\n")))
    (when-let ((chunks (e-chat-activity--activity-visible-chunks
                        (plist-get record :intermittent-entries))))
      (when chunks
        (concat (mapconcat #'identity chunks "\n") "\n\n")))))

(defun e-chat-activity--intermittent-details-text (record)
  "Return expanded intermittent details text for RECORD."
  (when-let ((entries (plist-get record :intermittent-entries)))
    (concat
     (mapconcat
      (lambda (entry)
        (e-chat-activity--indent-detail-text (e-chat-activity--intermittent-entry-text entry)))
      entries
      "\n\n")
     "\n\n")))

(defun e-chat-activity--activity-summary-details-text (turn-id record)
  "Return inline details text for TURN-ID's settled activity summary."
  (concat
   (or (e-chat-activity--activity-expanded-text record) "")
   (format "Turn: %s\nStarted: %s\nEnded: %s\nDuration: %s\n"
           turn-id
           (e-chat-activity--format-time-value (plist-get record :started-at))
           (e-chat-activity--format-time-value (plist-get record :ended-at))
           (e-chat-activity--format-duration (plist-get record :started-at)
                                    (plist-get record :ended-at)))))

(defun e-chat-activity--turn-details-text (turn-id record)
  "Return expanded diagnostic details for TURN-ID using activity RECORD."
  (concat
   (or (e-chat-activity--intermittent-details-text record) "")
   (or (e-chat-activity--retry-details-text record) "")
   (or (e-chat-activity--failure-details-text record) "")
   (format "  Turn: %s\n  Started: %s\n  Ended: %s\n  Duration: %s\n\n"
           turn-id
           (e-chat-activity--format-time-value (plist-get record :started-at))
           (e-chat-activity--format-time-value (plist-get record :ended-at))
           (e-chat-activity--format-duration (plist-get record :started-at)
                                             (plist-get record :ended-at)))))

(defun e-chat-activity--refresh-turn-details (record)
  "Refresh the derived details text cached on activity RECORD.
The activity owner is the only component that knows how reasoning, retries,
tools, actions, failures, and message details combine into an expandable turn
diagnostic.  Keeping this derived projection current lets the transcript owner
remain independent of activity internals while still rendering details on
later navigation."
  (when record
    (plist-put record :details-text
               (e-chat-activity--turn-details-text
                (plist-get record :id) record)))
  record)

(defun e-chat-activity--prepare-message (turn-id message)
  "Record MESSAGE activity details and return its expandable display text."
  (when (eq (plist-get message :role) 'assistant)
    (let* ((hidden (e-harness-message-hidden-p message))
           (record (e-chat-activity--turn-record turn-id))
           (presentation
            (and (not hidden)
                 (e-chat-service-message-presentation
                  e-chat-harness e-chat-session-id message)))
           (details (plist-get presentation :details)))
      (e-chat-activity--record-message-details
       turn-id (plist-get message :id) details)
      ;; Generic message details are only one part of the expandable turn
      ;; projection.  Once activity has seen the message, return its complete
      ;; semantic details as well so the transcript can attach the activity
      ;; projection without reading this owner's record.
      (or (e-chat-activity--message-details-text details)
          (and (or (plist-get record :has-provider-activity)
                   (plist-get record :intermittent-entries)
                   (plist-get record :message-details)
                   ;; Replay reconstructs timing from durable message
                   ;; timestamps even when no provider activity was stored.
                   (and (plist-get record :started-at)
                        (plist-get record :ended-at)))
               (plist-get record :details-text))))))

(defun e-chat-activity--reconcile-message-display (message)
  "Refresh activity details after MESSAGE's durable display disposition changes."
  (when (eq (plist-get message :role) 'assistant)
    (let* ((message-id (plist-get message :id))
           (turn-id (plist-get message :turn-id))
           (hidden (e-harness-message-hidden-p message))
           (presentation
            (and (not hidden)
                 (e-chat-service-message-presentation
                  e-chat-harness e-chat-session-id message)))
           (details (plist-get presentation :details)))
      (e-chat-transcript-update-message-details message-id
                                                 (e-chat-activity--message-details-text
                                                  details))
      (when turn-id
        (e-chat-activity--record-message-details turn-id message-id details)
        (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
          (when (plist-get record :final-rendered)
            (e-chat-activity--render-turn-transient turn-id record)))))))

(defun e-chat-activity--activity-summary-child-records (record)
  "Return navigable child block descriptors for RECORD's activity summary."
  (let (children)
    (dolist (round (e-chat-activity--activity-records record))
      (when-let ((thought (e-chat-activity--round-thought-text round)))
        (unless (equal thought "Thinking...")
          (push (list :kind 'activity-thought
                      :text thought
                      :action-text thought)
                children)))
      (dolist (reasoning (plist-get round :reasoning))
        (when-let ((content (plist-get reasoning :content)))
          (unless (string-empty-p content)
            (push (list :kind 'activity-reasoning
                        :text content
                        :action-text content)
                  children))))
      (dolist (batch (plist-get round :tool-batches))
        (let* ((items (plist-get batch :items))
               (count (length items)))
          (when (> count 0)
            (let ((text (e-chat-activity--activity-tool-count-text count)))
              (push (list :kind 'activity-tool-batch
                          :text text
                          :action-text text
                          :tool-items (e-chat-activity--semantic-tool-items items))
                    children)))))
      (dolist (entry (e-chat-activity--curation-entries-for-round record round))
        (let ((text (format "%s: %s"
                            (plist-get entry :title)
                            (plist-get entry :content))))
          (push (list :kind 'activity-context-curation
                      :text text
                      :action-text text)
                children))))
    (dolist (entry (plist-get record :intermittent-entries))
      (cond
       ((equal (plist-get entry :title) "Action call")
        (let ((text (format "Action: %s" (plist-get entry :content))))
          (push (list :kind 'activity-action
                      :text text
                      :action-text text)
                children)))
       ((and (eq (plist-get entry :kind) 'context-curated)
             (not (plist-get entry :round)))
        (let ((text (format "%s: %s"
                            (plist-get entry :title)
                            (plist-get entry :content))))
          (push (list :kind 'activity-context-curation
                      :text text
                      :action-text text)
                children)))))
    (nreverse children)))

(defun e-chat-activity--failure-details-text (record)
  "Return expanded failure details text for RECORD."
  (when-let ((error-message (plist-get record :failure-error)))
    (concat
     (e-chat-activity--indent-detail-text
      (format "Failure\n%s" error-message))
     "\n\n"
     (when-let ((details (plist-get record :failure-details)))
       (concat
        (e-chat-activity--indent-detail-text
         (format "Provider details\n%s" (pp-to-string details)))
        "\n\n")))))

(defun e-chat-activity--retry-details-text (record)
  "Return expanded provider retry diagnostics for RECORD."
  (let (sections)
    (dolist (round (e-chat-activity--activity-records record))
      (when (or (plist-get round :error)
                (plist-member round :error-details))
        (let ((text
               (format "Provider retry %s\nRetry delay: %s seconds"
                       (or (plist-get round :retry-attempt) 1)
                       (or (plist-get round :retry-backoff-seconds) 0))))
          (when (plist-member round :retry-reset-wait)
            (setq text
                  (concat text
                          (format "\nReset wait: %s seconds"
                                  (plist-get round :retry-reset-wait)))))
          (when-let ((error-message (plist-get round :error)))
            (setq text (concat text "\nError: " error-message)))
          (when (plist-member round :error-details)
            (setq text
                  (concat text "\nProvider details\n"
                          (string-trim-right
                           (pp-to-string
                            (plist-get round :error-details))))))
          (push (e-chat-activity--indent-detail-text text) sections))))
    (when sections
      (concat (string-join (nreverse sections) "\n\n") "\n\n"))))

(defun e-chat-activity--activity-tool-items (record &optional live-projection)
  "Return tool call/output items derived from RECORD.
When LIVE-PROJECTION is non-nil, include only live-projected rounds."
  (if (e-chat-activity--activity-records record)
      (mapcar
       (lambda (item)
         (list :id (plist-get item :id)
               :name (e-chat-activity--tool-item-name item)
               :call (plist-get item :call)
               :output (plist-get item :output)
               :progress (plist-get item :progress)))
       (apply #'append
              (mapcar #'e-chat-activity--round-tool-items
                      (if live-projection
                          (e-chat-activity--activity-record-projected-rounds record)
                        (e-chat-activity--activity-records record)))))
    (let ((items nil)
          current)
      (dolist (entry (plist-get record :intermittent-entries))
        (pcase (plist-get entry :title)
          ("Tool call"
           (when current
             (push current items))
           (setq current (list :call (plist-get entry :content)
                               :output nil)))
          ("Tool"
           (if current
               (progn
                 (plist-put current :output (plist-get entry :content))
                 (push current items)
                 (setq current nil))
             (push (list :call "Tool" :output (plist-get entry :content))
                   items)))))
      (when current
        (push current items))
      (nreverse items))))

(defun e-chat-activity--settled-activity-p (record)
  "Return non-nil when RECORD has current activity to keep after final output."
  (e-chat-activity--activity-summary-text record))

(defun e-chat-activity--transient-text (record)
  "Return visible transient text for RECORD."
  (if (e-chat-activity--activity-records record)
      (plist-get (e-chat-activity--activity-record-transient-data record) :text)
    (when-let ((entries (plist-get record :intermittent-entries)))
      (let ((chunks (e-chat-activity--activity-visible-chunks entries)))
        (when chunks
          (concat (mapconcat #'identity chunks "\n\n") "\n\n"))))))

(defun e-chat-activity--append-activity-entry (record entry)
  "Append structured activity ENTRY to RECORD."
  (plist-put record
             :intermittent-entries
             (append (plist-get record :intermittent-entries)
                     (list entry))))

(defun e-chat-activity--current-activity-round (record)
  "Return RECORD's current LLM round number."
  (or (plist-get record :activity-round) 0))

(defun e-chat-activity--record-provider-started (turn-id created-at)
  "Record provider request start for TURN-ID at CREATED-AT."
  (let* ((record (e-chat-activity--turn-record turn-id))
         (round (1+ (e-chat-activity--current-activity-round record))))
    (plist-put record :has-provider-activity t)
    (plist-put record :activity-round round)
    (e-chat-activity--append-activity-record
     record
     (list :kind 'round
           :round round
           :started-at created-at
           :ended-at nil
           :status 'active
           :reasoning nil
           :tool-batches nil))
    (e-chat-activity--append-activity-entry
     record
     (list :title "Thinking"
           :kind 'thinking
           :round round
           :status 'active
           :started-at created-at
           :content "Thinking..."
           :source 'activity))
    (e-chat-activity--refresh-turn-details record)))

(defun e-chat-activity--record-provider-finished (turn-id created-at &optional status)
  "Record provider request finish for TURN-ID at CREATED-AT.
STATUS defaults to `done'."
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (let ((round (e-chat-activity--active-round-record record))
          (status (e-chat-activity--normalize-round-status (or status 'done))))
      (when round
        (plist-put round :status status)
        (plist-put round :ended-at created-at)))
    (let ((entry
           (cl-find-if
            (lambda (candidate)
              (and (eq (plist-get candidate :kind) 'thinking)
                   (eq (plist-get candidate :status) 'active)))
            (reverse (plist-get record :intermittent-entries)))))
      (when entry
        (plist-put entry :title
                   (if (eq status 'attempt-failed)
                       "Provider attempt"
                     "Thought"))
        (plist-put entry :status
                   (e-chat-activity--normalize-round-status (or status 'done)))
        (plist-put entry :ended-at created-at)
        (plist-put entry :content
                   (e-chat-activity--thought-content
                    (plist-get entry :status)
                    (plist-get entry :started-at)
                    created-at))))
    (e-chat-activity--refresh-turn-details record)))

(defun e-chat-activity--record-turn-retrying (turn-id payload)
  "Record retry decision PAYLOAD for TURN-ID's latest failed attempt."
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (let ((round
           (cl-find-if
            (lambda (candidate)
              (memq (e-chat-activity--normalize-round-status
                     (plist-get candidate :status))
                    '(attempt-failed retrying)))
            (reverse (e-chat-activity--activity-records record))))
          (entry
           (cl-find-if
            (lambda (candidate)
              (and (eq (plist-get candidate :kind) 'thinking)
                   (memq (e-chat-activity--normalize-round-status
                          (plist-get candidate :status))
                         '(attempt-failed retrying))))
            (reverse (plist-get record :intermittent-entries)))))
      (when round
        (plist-put round :status 'retrying)
        (plist-put round :retry-attempt (plist-get payload :attempt))
        (plist-put round :retry-backoff-seconds
                   (plist-get payload :backoff-seconds))
        (plist-put round :error (plist-get payload :error))
        (when (plist-member payload :reset-wait)
          (plist-put round :retry-reset-wait
                     (plist-get payload :reset-wait)))
        (when (plist-member payload :details)
          (plist-put round :error-details (plist-get payload :details))))
      (when entry
        (plist-put entry :title "Provider attempt")
        (plist-put entry :status 'retrying)
        (when round
          (plist-put entry :content (e-chat-activity--round-thought-text round))))
      (e-chat-activity--refresh-turn-details record))))

(defun e-chat-activity--latest-open-round-record (record)
  "Return RECORD's latest non-terminal provider round."
  (cl-find-if
   (lambda (round)
     (memq (e-chat-activity--normalize-round-status (plist-get round :status))
           '(active attempt-failed retrying)))
   (reverse (e-chat-activity--activity-records record))))

(defun e-chat-activity--settle-open-thinking (turn-id ended-at status)
  "Settle TURN-ID's open thinking round at ENDED-AT with STATUS."
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (let ((status (e-chat-activity--normalize-round-status status)))
      (when-let ((round (e-chat-activity--latest-open-round-record record)))
        (plist-put round :status status)
        (plist-put round :ended-at ended-at))
      (when-let ((entry
                  (cl-find-if
                   (lambda (candidate)
                     (and (eq (plist-get candidate :kind) 'thinking)
                          (memq (e-chat-activity--normalize-round-status
                                 (plist-get candidate :status))
                                '(active attempt-failed retrying))))
                   (reverse (plist-get record :intermittent-entries)))))
        (plist-put entry :title "Thought")
        (plist-put entry :status status)
        (plist-put entry :ended-at ended-at)
        (plist-put entry :content
                   (e-chat-activity--thought-content
                    status
                    (plist-get entry :started-at)
                    ended-at))))
    (e-chat-activity--refresh-turn-details record)))

(defun e-chat-activity--record-reasoning-delta (record content &optional append source)
  "Record reasoning CONTENT in RECORD and its semantic activity records."
  (e-chat-activity--append-round-reasoning record content append)
  (e-chat-activity--add-intermittent-entry record "Reasoning" content append source)
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--reasoning-append-p (payload)
  "Return non-nil when reasoning PAYLOAD is an appendable stream fragment.
Board activity uses `snapshot' content because its publisher has already
coalesced the raw provider stream."
  (not (eq (plist-get payload :content-mode) 'snapshot)))

(defun e-chat-activity--record-tool-started (record payload &optional source created-at)
  "Record tool-started PAYLOAD in RECORD.
CREATED-AT records the tool start time for the running-tool row."
  (e-chat-activity--append-round-tool-call record payload created-at)
  (e-chat-activity--add-intermittent-entry
   record
   "Tool call"
   (e-chat-activity--format-tool-call payload)
   nil
   source)
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-tool-finished (record payload &optional source finished-at)
  "Record tool-finished PAYLOAD in RECORD.
FINISHED-AT records when the tool completed so the settled row can show
its run duration."
  (e-chat-activity--complete-round-tool-result record payload finished-at)
  (e-chat-activity--add-intermittent-entry
   record
   "Tool"
   (e-chat-activity--tool-result-display-text (plist-get payload :result))
   nil
   source)
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--format-action-call (payload)
  "Return compact action call text for PAYLOAD."
  (let ((capability (plist-get payload :capability-id))
        (action (plist-get payload :action)))
    (format "%s/%s"
            (or capability "unknown")
            (cond
             ((keywordp action) (substring (symbol-name action) 1))
             ((symbolp action) (symbol-name action))
             ((stringp action) (string-remove-prefix ":" action))
             (t "unknown")))))

(defun e-chat-activity--action-preview-content (preview)
  "Return display content from action PREVIEW plist."
  (cond
   ((and (listp preview) (plist-get preview :content))
    (plist-get preview :content))
   (preview (prin1-to-string preview))
   (t "")))

(defun e-chat-activity--attach-action-to-parent-tool (record payload)
  "Record PAYLOAD's action name on its parent run_elisp tool item in RECORD.
Does nothing when the action has no parent tool call or the parent tool
item is not found."
  (when-let* ((tool-id (plist-get payload :parent-tool-call-id))
              (item (e-chat-activity--find-round-tool-item record tool-id))
              (name (e-chat-activity--format-action-call payload)))
    (unless (member name (plist-get item :actions))
      (plist-put item :actions
                 (append (plist-get item :actions) (list name))))))

(defun e-chat-activity--record-action-started (record payload &optional source)
  "Record action-started PAYLOAD in RECORD."
  (plist-put record :action-count (1+ (or (plist-get record :action-count) 0)))
  (e-chat-activity--attach-action-to-parent-tool record payload)
  (e-chat-activity--add-intermittent-entry
   record
   "Action call"
   (e-chat-activity--format-action-call payload)
   nil
   source)
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-action-finished (record payload &optional source)
  "Record terminal action PAYLOAD in RECORD."
  (let* ((status (or (plist-get payload :status) 'ok))
         (result (or (plist-get payload :result)
                     (and (plist-get payload :message)
                          (list :content (plist-get payload :message)))))
         (preview (string-trim-right (e-chat-activity--action-preview-content result))))
    (e-chat-activity--add-intermittent-entry
     record
     "Action"
     (string-trim-right
      (format "%s -> %s%s"
              (e-chat-activity--format-action-call payload)
              status
              (if (string-empty-p preview)
                  ""
                (concat "
" preview))))
     nil
   source)
  (e-chat-activity--refresh-turn-details record)))

(defun e-chat-activity--record-tool-progress (record payload)
  "Record streaming tool progress PAYLOAD in RECORD."
  (e-chat-activity--record-round-tool-progress record payload)
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-steering-input (record preview)
  "Record accepted steering PREVIEW in RECORD's visible activity."
  (when (and record preview (not (string-empty-p preview)))
    (e-chat-activity--add-intermittent-entry
     record
     "Steering"
     (format "Steered: %s" preview)
     nil
     'steering))
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--intermittent-entry-exists-p (record title content &optional source)
  "Return non-nil when RECORD already has TITLE and CONTENT.
When SOURCE is non-nil, only match entries from that source."
  (cl-some
   (lambda (entry)
     (and (equal (plist-get entry :title) title)
          (equal (plist-get entry :content) content)
          (or (not source)
              (eq (plist-get entry :source) source))))
   (plist-get record :intermittent-entries)))

(defun e-chat-activity--remove-intermittent-entry (record title content source)
  "Remove intermittent RECORD entries matching TITLE, CONTENT, and SOURCE."
  (plist-put
   record
   :intermittent-entries
   (cl-remove-if
    (lambda (entry)
      (and (equal (plist-get entry :title) title)
           (equal (plist-get entry :content) content)
           (eq (plist-get entry :source) source)))
    (plist-get record :intermittent-entries))))

(defun e-chat-activity--add-intermittent-entry (record title content &optional append source)
  "Add intermittent TITLE and CONTENT to RECORD.
When APPEND is non-nil, merge CONTENT into the previous entry with TITLE.
SOURCE identifies where the entry came from for duplicate suppression."
  (when (and record content (not (string-empty-p content)))
    (when (eq source 'activity)
      (e-chat-activity--remove-intermittent-entry record title content 'transcript))
    (let* ((entries (plist-get record :intermittent-entries))
           (last-entry (car (last entries))))
      (if (and append
               last-entry
               (equal (plist-get last-entry :title) title))
          (plist-put last-entry
                     :content
                     (concat (plist-get last-entry :content) content))
        (plist-put record
                   :intermittent-entries
                   (append entries
                           (list (list :title title
                                       :content content
                                       :source source))))))))

(defun e-chat-activity--delete-turn-transient (&optional _turn-id)
  "Delete the current transient activity projection through transcript API."
  (e-chat-transcript-remove-activity)
  (setq-local e-chat-activity--rendered-turn-id nil
              e-chat-activity--rendered-activity-size 0))

(defun e-chat-activity--running-status-turn-id ()
  "Return the turn id for the visible running status."
  e-chat-activity--rendered-turn-id)

(defun e-chat-activity--running-status-data (turn-id record)
  "Return semantic transcript projection for TURN-ID and RECORD.
The returned value is a fresh data projection; it contains no activity record
or transcript marker and is consumed only by the transcript projection port."
  (let* ((has-progress (and e-chat-activity--progress-turn-id
                            (equal turn-id e-chat-activity--progress-turn-id)))
         (final-rendered (and record (plist-get record :final-rendered)))
         (summary-text (and final-rendered
                            record
                            (e-chat-activity--activity-summary-text record)))
         (pending-summary (and record (plist-get record :pending-hook-summary)))
         (transient-data
          (and record
               (e-chat-activity--activity-records record)
               (e-chat-activity--activity-record-transient-data record)))
         (transient-text
          (and record
               (or (plist-get transient-data :text)
                   (e-chat-activity--transient-text record))))
         (pending-prefix (and pending-summary
                              (concat pending-summary "\n\n")))
         (text (if final-rendered
                   (concat (when summary-text
                             (concat summary-text "\n\n"))
                           pending-prefix)
                 (and transient-text
                      (concat pending-prefix transient-text))))
         (progress-tail-text
          (and (not final-rendered)
               has-progress
               (plist-get transient-data :progress-tail-text))))
    (list :progress-p has-progress
          :final-p final-rendered
          :summary summary-text
          :display-text text
          :prefix-text (and (not final-rendered) pending-prefix)
          :progress-glyph (e-chat-activity--progress-dots)
          :progress-tail progress-tail-text
          :kind (if summary-text 'activity-summary 'activity)
          :tools (e-chat-activity--activity-tool-items
                  record (not final-rendered))
          :children
          (and summary-text
               (e-chat-activity--activity-summary-child-records record))
          :details (and record
                        (plist-get record :details-text)))))

(defun e-chat-activity--turn-pending-hook-summary (turn-id)
  "Return capability-provided pending hook activity for TURN-ID, if any."
  (when (and e-chat-harness e-chat-session-id turn-id)
    (when-let ((prompt
                (seq-find
                 (lambda (message)
                   (and (eq (plist-get message :role) 'user)
                        (equal (plist-get message :turn-id) turn-id)))
                 (e-chat-service-messages e-chat-harness e-chat-session-id))))
      (let ((summary (plist-get (plist-get prompt :metadata)
                                :pending-summary)))
        (and (stringp summary) summary)))))

(defun e-chat-activity--running-status-display-text (data)
  "Return the buffer text represented by running-status DATA."
  (or (plist-get data :display-text)
      (plist-get data :progress-glyph)))

(defun e-chat-activity--render-running-status (turn-id record)
  "Render TURN-ID's active progress and RECORD transient summary together."
  (let ((data (e-chat-activity--running-status-data turn-id record)))
    (when (and turn-id
               (not (plist-get data :final-p))
               (e-chat-activity--active-activity-p record))
      (e-chat-activity--ensure-progress-interval turn-id))
    (setq e-chat-activity--rendered-turn-id turn-id
          e-chat-activity--rendered-activity-size
          (or (e-chat-transcript-project-activity turn-id data) 0))
    (run-hook-with-args 'e-chat-activity--running-status-rendered-hook turn-id)))

(defun e-chat-activity--render-turn-transient (turn-id &optional record)
  "Render RECORD's intermittent entries as a temporary block for TURN-ID."
  (setq record (or record (e-chat-activity--existing-turn-record turn-id)))
  (when record
    (plist-put record :details-text
               (e-chat-activity--turn-details-text turn-id record)))
  (e-chat-activity--profile-call
   'chat.render-turn-transient
   (list :session-id e-chat-session-id
         :turn-id turn-id
         :buffer-name (buffer-name))
   (lambda ()
     (e-chat-activity--render-running-status turn-id record))))

(defun e-chat-activity--cancel-pending-activity-redraw (&optional turn-id)
  "Cancel the pending activity redraw.
When TURN-ID is non-nil, cancel only a redraw for that turn."
  (when (and e-chat-activity--pending-activity-redraw-turn-id
             (or (not turn-id)
                 (equal turn-id e-chat-activity--pending-activity-redraw-turn-id)))
    (when (e-work-handle-p e-chat-activity--pending-activity-redraw-handle)
      (e-ui-work-cancel e-chat-activity--pending-activity-redraw-handle))
    (cl-incf e-chat-activity--activity-redraw-generation)
    (setq e-chat-activity--pending-activity-redraw-turn-id nil)
    (setq e-chat-activity--pending-activity-redraw-handle nil)
    (setq e-chat-activity--pending-activity-redraw-kind nil)
    (setq e-chat-activity--pending-activity-redraw-generation nil)))

(defun e-chat-activity--ensure-pending-activity-redraw-work ()
  "Ensure the pending activity redraw has scheduled UI work."
  (when (and e-chat-activity--pending-activity-redraw-turn-id
             e-chat-activity--pending-activity-redraw-generation
             (not e-chat-activity--activity-redraw-running)
             (not (e-work-handle-p e-chat-activity--pending-activity-redraw-handle)))
    (let ((turn-id e-chat-activity--pending-activity-redraw-turn-id)
          (generation e-chat-activity--pending-activity-redraw-generation))
      (setq e-chat-activity--pending-activity-redraw-handle
            (e-ui-work-schedule
             (e-ui-work-spec-create
              :id "chat_activity_redraw"
              :description "Redraw active chat turn activity."
              :owner 'activity-redraw
              :target-buffer (current-buffer)
              :key turn-id
              :generation generation
              :delay (e-chat-activity--activity-redraw-delay)
              :coalesce t
              ;; The chat surface owns its transcript viewport policy.  A
              ;; generic one-buffer focus snapshot cannot represent a focused
              ;; composer paired with a separately scrolling transcript.
              :focus-policy 'explicit
              :reentrancy-policy 'defer
              :apply
              (lambda (_job _handle)
                (setq e-chat-activity--pending-activity-redraw-handle nil)
                (e-chat-activity--run-pending-activity-redraw generation)))
             :on-event (lambda (&rest _)
                         (e-chat-surface-refresh-ui-work-diagnostics)))))))

(defun e-chat-activity--activity-redraw-delay ()
  "Return the coalescing delay for the next activity redraw.
A large visible transient block is throttled by
`e-chat-activity-redraw-large-block-factor' so it repaints less often than a
small one, since each repaint of a big block costs more."
  (let ((size e-chat-activity--rendered-activity-size))
    (if (and size
             (>= size e-chat-activity-redraw-large-block-chars))
        (* e-chat-activity-redraw-delay
           e-chat-activity-redraw-large-block-factor)
      e-chat-activity-redraw-delay)))

(defun e-chat-activity--run-pending-activity-redraw (&optional expected-generation)
  "Run and clear the pending activity redraw for this chat buffer."
  (when (or (null expected-generation)
            (equal expected-generation
                   e-chat-activity--pending-activity-redraw-generation))
    (if e-chat-activity--activity-redraw-running
        (when expected-generation
          ;; Leave the work pending for the outer redraw to schedule after it
          ;; exits.
          (setq e-chat-activity--pending-activity-redraw-handle nil))
      (let ((turn-id e-chat-activity--pending-activity-redraw-turn-id)
            (handle e-chat-activity--pending-activity-redraw-handle)
            (kind e-chat-activity--pending-activity-redraw-kind))
        (setq e-chat-activity--activity-redraw-running t)
        (unwind-protect
            (e-chat-surface-without-recenter
             (lambda ()
              (e-chat-activity--profile-call
               'chat.activity-redraw
               (list :session-id e-chat-session-id
                     :turn-id turn-id
                     :buffer-name (buffer-name)
                     :metadata (list :kind (and kind (symbol-name kind))
                                     :generation expected-generation))
               (lambda ()
                 (when (e-work-handle-p handle)
                   (e-ui-work-cancel handle))
                 (setq e-chat-activity--pending-activity-redraw-turn-id nil)
                 (setq e-chat-activity--pending-activity-redraw-handle nil)
                 (setq e-chat-activity--pending-activity-redraw-kind nil)
                 (setq e-chat-activity--pending-activity-redraw-generation nil)
                 (when turn-id
                   (pcase kind
                     ('progress
                      (e-chat-activity--render-progress-indicator turn-id))
                     (_
                      (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
                        (e-chat-activity--render-turn-transient turn-id record)))))))))
          (setq e-chat-activity--activity-redraw-running nil)
          (e-chat-activity--ensure-pending-activity-redraw-work))))))

(defun e-chat-activity--activity-redraw-kind (existing requested)
  "Return coalesced redraw kind from EXISTING and REQUESTED kinds."
  (cond
   ((eq existing 'activity) 'activity)
   ((eq requested 'activity) 'activity)
   (requested)
   (existing)
   (t 'activity)))

(defun e-chat-activity--request-activity-redraw (turn-id &optional kind)
  "Schedule one near-future activity redraw for TURN-ID.
When this chat buffer is displayed in no window, the repaint is withheld and
remembered in `e-chat-activity--deferred-activity-redraw'; the same applies
while a minibuffer is active.  The repaint is re-issued once the buffer is
visible and ordinary top-level interaction resumes.  Skipping cosmetic
transcript rewrites
keeps background sessions and progress animation from starving process output."
  (when turn-id
    (if (or (not (e-chat-surface-redraw-visible-p))
            (active-minibuffer-window))
        (setq e-chat-activity--deferred-activity-redraw
              (cons turn-id
                    (e-chat-activity--activity-redraw-kind
                     (cdr e-chat-activity--deferred-activity-redraw)
                     (or kind 'activity))))
      (setq e-chat-activity--pending-activity-redraw-turn-id turn-id)
      (setq e-chat-activity--pending-activity-redraw-kind
            (e-chat-activity--activity-redraw-kind
             e-chat-activity--pending-activity-redraw-kind
             (or kind 'activity)))
      (unless e-chat-activity--pending-activity-redraw-generation
        (setq e-chat-activity--pending-activity-redraw-generation
              (cl-incf e-chat-activity--activity-redraw-generation)))
      (e-chat-activity--ensure-pending-activity-redraw-work))))

(defun e-chat-activity--flush-deferred-activity-redraw ()
  "Issue this buffer's activity redraw when presentation is ready."
  (when (and e-chat-activity--deferred-activity-redraw
             (e-chat-surface-redraw-visible-p)
             (not (active-minibuffer-window)))
    (let ((turn-id (car e-chat-activity--deferred-activity-redraw))
          (kind (cdr e-chat-activity--deferred-activity-redraw)))
      (setq e-chat-activity--deferred-activity-redraw nil)
      (e-chat-activity--request-activity-redraw turn-id kind))))

(defun e-chat-activity--flush-deferred-activity-redraws (&rest _)
  "Flush presentation-ready activity redraws for every chat buffer."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (e-chat-activity--chat-buffer-p)
                   e-chat-activity--deferred-activity-redraw)
          (e-chat-activity--flush-deferred-activity-redraw))))))

(defun e-chat-activity--flush-deferred-activity-redraws-after-minibuffer (&rest _)
  "Flush activity redraws after Emacs finishes leaving the minibuffer.
`minibuffer-exit-hook' runs while `active-minibuffer-window' still identifies
the exiting minibuffer, so defer the coalesced flush by one event-loop turn."
  (run-at-time 0 nil #'e-chat-activity--flush-deferred-activity-redraws))

(defun e-chat-activity--progress-dots ()
  "Return the current active assistant progress glyph string."
  (aref e-chat-activity--progress-glyphs
        (mod e-chat-activity--progress-frame
             (length e-chat-activity--progress-glyphs))))

(defun e-chat-activity--active-activity-p (record)
  "Return non-nil when RECORD has an active provider activity round."
  (and record (e-chat-activity--active-round-record record)))

(defun e-chat-activity--service-active-turn-matches-p (turn-id)
  "Return non-nil when TURN-ID is the service's current running turn."
  (and turn-id
       e-chat-session-id
       (e-harness-p e-chat-harness)
       (equal turn-id
              (plist-get
               (e-chat-service-active-turn
                e-chat-harness e-chat-session-id)
               :id))))

(defun e-chat-activity--stale-progress-turn-p (turn-id)
  "Return non-nil when TURN-ID no longer matches harness running state."
  (when (and turn-id
             e-chat-session-id
             (e-harness-p e-chat-harness))
    (not (e-chat-activity--service-active-turn-matches-p turn-id))))

(defun e-chat-activity--cancel-progress-interval ()
  "Cancel the active assistant progress UI work interval."
  (when (e-work-handle-p e-chat-activity--progress-interval-handle)
    (e-ui-work-cancel e-chat-activity--progress-interval-handle))
  (setq e-chat-activity--progress-interval-handle nil))

(defun e-chat-activity--progress-interval-active-p (turn-id)
  "Return non-nil when TURN-ID has a live progress UI work interval."
  (and (equal e-chat-activity--progress-turn-id turn-id)
       (e-work-handle-p e-chat-activity--progress-interval-handle)
       (not (e-request-terminal-p
             (e-work-handle-lifecycle e-chat-activity--progress-interval-handle)))))

(defun e-chat-activity--ensure-progress-interval (turn-id)
  "Ensure TURN-ID has a live progress interval without rendering immediately."
  (unless (e-chat-activity--progress-interval-active-p turn-id)
    (let ((same-turn (equal e-chat-activity--progress-turn-id turn-id)))
      (e-chat-activity--cancel-progress-interval)
      (setq e-chat-activity--progress-turn-id turn-id)
      (unless same-turn
        (setq e-chat-activity--progress-frame 0))
      (setq e-chat-activity--progress-next-tick-time
            (+ (float-time) e-chat-progress-interval))
      (setq e-chat-activity--progress-interval-handle
            (e-ui-work-schedule-interval
             (e-ui-work-spec-create
              :id "chat_progress_indicator"
              :description "Advance active chat progress indicator."
              :owner 'progress-indicator
              :target-buffer (current-buffer)
              :key turn-id
              :generation e-chat-activity--progress-frame
              :focus-policy 'preserve
              :reentrancy-policy 'defer
              :apply
              (lambda (_job handle)
                (if (not (eq e-chat-activity--progress-interval-handle handle))
                    '(:status stopped)
                  (e-chat-activity--advance-progress-indicator)
                  (if (eq e-chat-activity--progress-interval-handle handle)
                      :continue
                    '(:status stopped)))))
             e-chat-progress-interval
             :on-event (lambda (&rest _)
                         (e-chat-surface-refresh-ui-work-diagnostics)))))))

(defun e-chat-activity--delete-progress-indicator ()
  "Delete the active assistant progress indicator."
  (e-chat-transcript-remove-activity))

(defun e-chat-activity--render-progress-indicator (turn-id)
  "Render active assistant progress indicator for TURN-ID."
  (let ((record (e-chat-activity--existing-turn-record turn-id)))
    (e-chat-activity--render-running-status turn-id record)))

(defun e-chat-activity--advance-progress-indicator ()
  "Advance and rerender the active assistant progress indicator."
  (when e-chat-activity--progress-turn-id
    (if (e-chat-activity--stale-progress-turn-p e-chat-activity--progress-turn-id)
        (let ((turn-id e-chat-activity--progress-turn-id))
          (e-chat-activity--settle-open-thinking turn-id
                                        (e-chat-activity--current-time-seconds)
                                        'done)
          (e-chat-activity--stop-progress-indicator turn-id)
          (e-chat-surface-set-status "idle" t))
      (let* ((now (float-time))
             (late-by (and e-chat-activity--progress-next-tick-time
                           (- now e-chat-activity--progress-next-tick-time)))
             (threshold (max 5.0 (* 3 e-chat-progress-interval))))
        (when (and late-by (> late-by threshold))
          (e-chat-surface-set-status
           (format "Emacs was blocked for %.0fs; checking turn state"
                   late-by)))
        (setq e-chat-activity--progress-next-tick-time
              (+ now e-chat-progress-interval)))
      (setq e-chat-activity--progress-frame (1+ e-chat-activity--progress-frame))
      (e-chat-activity--request-activity-redraw e-chat-activity--progress-turn-id 'progress))))

(defun e-chat-activity--start-progress-indicator (turn-id)
  "Start the active assistant progress indicator for TURN-ID.
The first visible frame uses the same scheduled projection as later progress
and activity updates.  Event dispatch only changes local state."
  (setq e-chat-activity--progress-frame 0)
  (e-chat-activity--ensure-progress-interval turn-id)
  (e-chat-activity--request-activity-redraw turn-id 'progress))

(defun e-chat-activity--stop-progress-indicator (&optional turn-id)
  "Stop and delete the active assistant progress indicator.
When TURN-ID is non-nil, only stop a matching active indicator."
  (when (and e-chat-activity--progress-turn-id
             (or (not turn-id)
                 (equal turn-id e-chat-activity--progress-turn-id)))
    (e-chat-activity--cancel-progress-interval)
    (let ((old-turn-id e-chat-activity--progress-turn-id))
      (setq e-chat-activity--progress-turn-id nil)
      (setq e-chat-activity--progress-frame 0)
      (setq e-chat-activity--progress-next-tick-time nil)
      (if-let ((record (and old-turn-id
                            (e-chat-activity--existing-turn-record old-turn-id))))
          (e-chat-activity--render-running-status old-turn-id record)
        (e-chat-transcript-remove-activity)))))

(defun e-chat-activity--append-intermittent-entry (turn-id title content &optional append source)
  "Append intermittent TITLE and CONTENT to TURN-ID.
When APPEND is non-nil, merge CONTENT into the previous entry with TITLE.
SOURCE identifies where the entry came from for duplicate suppression."
  (when (and turn-id content (not (string-empty-p content)))
    (let ((record (e-chat-activity--turn-record turn-id)))
      (e-chat-activity--add-intermittent-entry record title content append source)
      (e-chat-activity--request-activity-redraw turn-id 'activity))))

(defun e-chat-activity--format-tool-call (payload)
  "Return a compact display string for tool-call PAYLOAD."
  (let ((name (plist-get payload :name))
        (arguments (plist-get payload :arguments)))
    (string-join
     (delq nil
           (list (and name (format "%s" name))
                 (and arguments
                      (format "%S" arguments))))
     "\n")))

(defun e-chat-activity--tool-message-p (message)
  "Return non-nil when MESSAGE is a tool transcript message."
  (memq (plist-get message :role) '(tool-call tool)))

(defun e-chat-activity--record-replayed-message-time (record message)
  "Record MESSAGE's replay timestamp into RECORD."
  (when-let ((created-at (plist-get message :created-at)))
    (unless (plist-get record :started-at)
      (plist-put record :started-at created-at))
    (plist-put record :ended-at created-at))
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-hook-audit (record payload &optional source)
  "Record a generic hook audit PAYLOAD in RECORD.

SOURCE identifies replayed durable activity or a live event.  Capability-owned
message semantics come through the separate message-details contract; this
function records only lifecycle audit text."
  (when-let ((summary (plist-get payload :summary)))
    (e-chat-activity--add-intermittent-entry record "Hook audit" summary nil source))
  (when (plist-member payload :pending-summary)
    (let ((pending-summary (plist-get payload :pending-summary)))
      (plist-put record
                 :pending-hook-summary
                 (and (stringp pending-summary)
                      (not (string-empty-p pending-summary))
                      pending-summary))))
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-context-curated (record activity-event)
  "Record one Board-backed context curation from ACTIVITY-EVENT in RECORD."
  (let ((identity (plist-get activity-event :message-id))
        ;; Curation is committed after a provider response and before the
        ;; follow-up request starts.  Retain that observed round ordinal only
        ;; in this ephemeral projection so replay and live delivery compose in
        ;; the same chronological position.
        (round (e-chat-activity--last-round-record record)))
    (unless identity
      (signal 'e-chat-service-invalid-activity
              (list 'context-curated :missing-message-id)))
    (unless (cl-find identity (plist-get record :intermittent-entries)
                     :key (lambda (entry) (plist-get entry :activity-id))
                     :test #'equal)
      (e-chat-activity--append-activity-entry
       record
       (list :title "Context curated"
             :content
             (e-chat-service-format-context-curation
              (plist-get activity-event :payload))
             :kind 'context-curated
             :round (and round (plist-get round :round))
             :activity-id identity
             :source 'activity))))
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-turn-summary (record activity-event)
  "Record aggregate Board ACTIVITY-EVENT data in RECORD without settling twice."
  (let ((payload (plist-get activity-event :payload)))
    ;; Board emits this row only for provider-active turns.  Retain that
    ;; authoritative fact when bounded replay no longer includes the earlier
    ;; provider activity rows.
    (plist-put record :has-provider-activity t)
    (dolist (mapping '((:duration-seconds . :summary-duration-seconds)
                       (:tool-count . :summary-tool-count)
                       (:action-count . :summary-action-count)))
      (when (plist-member payload (car mapping))
        (plist-put record (cdr mapping) (plist-get payload (car mapping)))))
    (when-let ((created-at (plist-get activity-event :created-at)))
      (plist-put record :ended-at created-at)
      (unless (plist-get record :started-at)
        (when-let* ((duration (plist-get payload :duration-seconds))
                    (created-seconds
                     (e-chat-activity--time-seconds created-at)))
          (when (numberp duration)
            (plist-put record :started-at
                       (- created-seconds duration)))))))
  (e-chat-activity--refresh-turn-details record))

(defun e-chat-activity--record-activity-event (turn-id activity-event)
  "Record durable ACTIVITY-EVENT for TURN-ID without re-emitting it."
  (let ((record (e-chat-activity--turn-record turn-id)))
    (pcase (plist-get activity-event :event-type)
      ('turn-started
       (e-chat-activity--set-turn-time turn-id
                              :started-at
                              (plist-get activity-event :created-at)))
      ('provider-request-started
       (e-chat-activity--record-provider-started
        turn-id
        (plist-get activity-event :created-at)))
      ('provider-request-finished
       (e-chat-activity--record-provider-finished
        turn-id
        (plist-get activity-event :created-at)
        (plist-get (plist-get activity-event :payload) :status)))
      ('turn-retrying
       (e-chat-activity--record-turn-retrying
        turn-id (plist-get activity-event :payload)))
      ('turn-finished
       (e-chat-activity--set-turn-time turn-id
                              :ended-at
                              (plist-get activity-event :created-at)))
      ('reasoning-delta
       (let ((payload (plist-get activity-event :payload)))
         (e-chat-activity--record-reasoning-delta
          record
          (plist-get payload :content)
          (e-chat-activity--reasoning-append-p payload)
          'activity)))
      ('tool-started
       (e-chat-activity--record-tool-started
        record
        (plist-get activity-event :payload)
        'activity
        (plist-get activity-event :created-at)))
      ('tool-finished
       (e-chat-activity--record-tool-finished
        record
        (plist-get activity-event :payload)
        'activity
        (plist-get activity-event :created-at)))
      ('action-started
       (e-chat-activity--record-action-started
        record
        (plist-get activity-event :payload)
        'activity))
      ((or 'action-finished 'action-failed)
       (e-chat-activity--record-action-finished
        record
        (plist-get activity-event :payload)
        'activity))
      ('hook-audit
       (e-chat-activity--record-hook-audit record (plist-get activity-event :payload)
                                  'activity))
      ('context-curated
       (e-chat-activity--record-context-curated record activity-event))
      ;; Board summaries are aggregate data, not a second terminal edge.  The
      ;; detailed terminal activity above remains the failure/cancellation
      ;; authority, while successful output already carries its terminal fact.
      ('turn-summary
       (e-chat-activity--record-turn-summary record activity-event))
      ('tool-progress
       (e-chat-activity--record-tool-progress
        record
        (plist-get activity-event :payload)))
      ('turn-failed
       (when (e-chat-transcript-event-selected-participant-p activity-event)
         (e-chat-activity--settle-open-thinking
          turn-id
          (plist-get activity-event :created-at)
          'failed)
         (e-chat-activity--record-turn-failure
          turn-id
          (plist-get activity-event :payload))))
      ('turn-cancelled
       (when (e-chat-transcript-event-selected-participant-p activity-event)
         (e-chat-activity--settle-open-thinking
          turn-id
          (plist-get activity-event :created-at)
          'cancelled))))))

(defun e-chat-activity--replay-turn-ids (messages)
  "Return presentation turn ids represented by replayed MESSAGES."
  (let (turn-ids)
    (dolist (message messages)
      (when-let ((turn-id (plist-get message :turn-id)))
        (cl-pushnew turn-id turn-ids :test #'equal)))
    ;; Active-turn state belongs to the live Board controller.  Asking durable
    ;; storage whether this is a Board session first is redundant and, for v6
    ;; SQLite, would turn presentation replay into a forbidden aggregate read.
    (when-let* ((active-turn
                 (e-chat-service-active-turn
                  e-chat-harness e-chat-session-id))
                (turn-id (plist-get active-turn :id)))
      (cl-pushnew turn-id turn-ids :test #'equal))
    turn-ids))

(defun e-chat-activity--replay-message-tail (messages)
  "Return the message tail used to rebuild activity state.

The transcript owner applies the same bound while rendering durable rows.  The
activity owner repeats only this small value selection so it can reconstruct
its own state without importing transcript storage or record operations.  The
facade calls both owners with the same source snapshot, so their projections
remain aligned while their registries remain independent."
  (let ((limit e-chat-session-replay-message-limit))
    (if (and (integerp limit) (> limit 0) (> (length messages) limit))
        (nthcdr (- (length messages) limit) messages)
      messages)))

(defun e-chat-activity--validated-positive-limit (value option)
  "Return positive integer VALUE or signal a configuration error for OPTION."
  (if (and (integerp value) (> value 0))
      value
    (user-error "%s must be a positive integer" option)))

(defun e-chat-activity--tail-events (events limit)
  "Return at most LIMIT trailing activity EVENTS."
  (if (and (integerp limit) (> limit 0) (> (length events) limit))
      (nthcdr (- (length events) limit) events)
    events))

(cl-defun e-chat-activity--replay-events
    (messages &optional (activity-events nil activity-events-supplied-p))
  "Return bounded activity events relevant to replayed MESSAGES."
  (let* ((limit (e-chat-activity--validated-positive-limit
                 e-chat-session-replay-activity-event-limit
                 'e-chat-session-replay-activity-event-limit))
         (turn-ids (e-chat-activity--replay-turn-ids messages))
         (source-events
          (if activity-events-supplied-p
              activity-events
            (e-chat-service-activity-events
             e-chat-harness e-chat-session-id)))
         (events
          (and turn-ids
               (cl-remove-if-not
                (lambda (event)
                  (member (plist-get event :turn-id) turn-ids))
                source-events))))
    (e-chat-activity--tail-events events limit)))

(defun e-chat-activity--terminal-events (turn-id activity-events)
  "Return terminal failure/cancellation events for TURN-ID."
  (cl-remove-if-not
   (lambda (event)
     (and (equal (plist-get event :turn-id) turn-id)
          (memq (plist-get event :event-type)
                '(turn-failed turn-cancelled))))
   activity-events))

(defun e-chat-activity--render-replayed-terminal-event (turn-id activity-events)
  "Render replayed terminal activity for TURN-ID when needed."
  (let* ((terminal-events
          (e-chat-activity--terminal-events turn-id activity-events))
         (selected-event
          (cl-find-if #'e-chat-transcript-event-selected-participant-p
                      terminal-events)))
    (when selected-event
      (let ((record (e-chat-activity--turn-record turn-id))
            (event-type (plist-get selected-event :event-type)))
        (unless (or (plist-get record :final-rendered)
                    (plist-get record :failure-rendered))
          (e-chat-activity--render-turn-activity-events turn-id activity-events)
          (if (eq event-type 'turn-cancelled)
              (e-chat-activity--render-turn-cancellation
               turn-id (plist-get selected-event :created-at) t)
            (e-chat-activity--render-turn-failure
             turn-id
             (plist-get selected-event :created-at)
             (plist-get selected-event :payload)
             t)))))
    (dolist (activity-event terminal-events)
      (unless (eq activity-event selected-event)
        (e-chat-activity--render-observed-terminal-event
         turn-id
         (plist-get activity-event :created-at)
         (if (eq (plist-get activity-event :event-type) 'turn-cancelled)
             'cancelled
           'failed)
         (plist-get activity-event :payload)
         activity-event)))))

(defun e-chat-activity--event-turn-ids (activity-events)
  "Return TURN-IDs represented in ACTIVITY-EVENTS, preserving order."
  (let (turn-ids)
    (dolist (event activity-events)
      (when-let ((turn-id (plist-get event :turn-id)))
        (unless (member turn-id turn-ids)
          (push turn-id turn-ids))))
    (nreverse turn-ids)))

(defun e-chat-activity--turn-events (turn-id activity-events)
  "Return ACTIVITY-EVENTS belonging to TURN-ID."
  (cl-remove-if-not
   (lambda (event) (equal (plist-get event :turn-id) turn-id))
   activity-events))

(defun e-chat-activity--terminal-event-p (events)
  "Return non-nil when EVENTS contain a selected terminal event."
  (cl-some
   (lambda (event)
     (and (e-chat-transcript-event-selected-participant-p event)
          (memq (plist-get event :event-type)
                '(turn-finished turn-failed turn-cancelled))))
   events))

(defun e-chat-activity--render-replayed-active-activity (activity-events)
  "Render replayed non-terminal ACTIVITY-EVENTS as transient activity."
  (dolist (turn-id (e-chat-activity--event-turn-ids activity-events))
    (let* ((events (e-chat-activity--turn-events turn-id activity-events))
           (record (e-chat-activity--existing-turn-record turn-id)))
      (when (and record
                 (not (plist-get record :final-rendered))
                 (not (plist-get record :failure-rendered))
                 (not (e-chat-activity--terminal-event-p events))
                 (not (e-chat-activity--stale-progress-turn-p turn-id)))
        (e-chat-activity--render-turn-activity-events turn-id activity-events)))))

(cl-defun e-chat-activity--render-replay
    (messages &optional (activity-events nil activity-events-supplied-p))
  "Reconstruct activity state for bounded durable MESSAGES.

The transcript owner renders the durable rows and omitted-history marker.  This
operation only records activity state, updates semantic message-detail
projections, and renders the activity-owned transient/failure rows through
the transcript owner's activity projection port."
  (let* ((tail (e-chat-activity--replay-message-tail messages))
         (activity-events
          (if activity-events-supplied-p
              (e-chat-activity--replay-events tail activity-events)
            (e-chat-activity--replay-events tail)))
         (turn-index 0)
         turn-id)
    (dolist (message tail)
      (when (or (plist-get message :turn-id)
                (not turn-id)
                (eq (plist-get message :role) 'user))
        (let ((next-turn-id
               (or (plist-get message :turn-id)
                   (format "replayed-turn-%d" (1+ turn-index)))))
          (when (and turn-id (not (equal turn-id next-turn-id)))
            (e-chat-activity--render-replayed-terminal-event
             turn-id activity-events))
          (setq turn-index (1+ turn-index)
                turn-id next-turn-id)))
      (let* ((message-selected-p
              (e-chat-transcript-message-selected-participant-p message))
             (render-turn-id
              (if message-selected-p
                  turn-id
                (e-chat-transcript-observed-turn-id turn-id message)))
             (record (e-chat-activity--turn-record render-turn-id)))
        (e-chat-activity--record-replayed-message-time record message)
        (unless (e-chat-activity--tool-message-p message)
          (let ((details-text
                 (e-chat-activity--prepare-message render-turn-id message)))
            (when (plist-get message :id)
              (e-chat-transcript-update-message-details
               (plist-get message :id) details-text))
            (when (and (not (e-harness-message-hidden-p message))
                       (eq (plist-get message :role) 'assistant))
              (e-chat-activity--render-turn-activity-events
               turn-id activity-events))
            (when (and (eq (plist-get message :role) 'assistant)
                       message-selected-p)
              (e-chat-activity--finalize-turn-display render-turn-id))))))
    (e-chat-activity--render-replayed-active-activity activity-events)
    (when turn-id
      (e-chat-activity--render-replayed-terminal-event turn-id activity-events))))

(defun e-chat-activity--render-turn-activity-events (turn-id activity-events)
  "Render durable ACTIVITY-EVENTS for TURN-ID once."
  (when-let ((record (e-chat-activity--turn-record turn-id)))
    (unless (plist-get record :activity-rendered)
      (let (selected-event-p)
        (dolist (event activity-events)
          (when (equal (plist-get event :turn-id) turn-id)
            (let* ((selected-p (e-chat-transcript-event-selected-participant-p event))
                   (render-turn-id (e-chat-transcript-presentation-turn-id turn-id event)))
              (when selected-p
                (setq selected-event-p t))
              (e-chat-activity--record-activity-event render-turn-id event)
              (when-let ((render-record
                          (e-chat-activity--existing-turn-record render-turn-id)))
                (plist-put render-record :activity-rendered t)))))
        ;; An observed sibling can share the causal TURN-ID.  Do not mutate
        ;; the selected record merely because its sibling's activity was
        ;; replayed; only selected-owned activity establishes this marker.
        (when selected-event-p
          (plist-put record :activity-rendered t)
          (e-chat-activity--render-turn-transient turn-id record))
        (when (and (not selected-event-p)
                   e-chat-activity--progress-turn-id)
          (when-let ((selected-record
                      (e-chat-activity--existing-turn-record e-chat-activity--progress-turn-id)))
            (e-chat-activity--render-turn-transient
             e-chat-activity--progress-turn-id selected-record)))))))

(defun e-chat-activity--record-turn-failure (turn-id payload)
  "Record failed-turn PAYLOAD for TURN-ID."
  (when-let ((record (e-chat-activity--turn-record turn-id)))
    (plist-put record :failure-error
               (or (plist-get payload :error) "Turn failed"))
    (plist-put record :failure-details (plist-get payload :details))
    (e-chat-activity--refresh-turn-details record)))

(defun e-chat-activity--render-observed-terminal-event
    (turn-id created-at event-type payload &optional event)
  "Render an observed sibling terminal EVENT-TYPE without settling this chat.
The semantic row remains visible, but the selected participant's progress,
status, and composer are owned by the selected terminal path only."
  (let* ((render-turn-id (e-chat-transcript-observed-turn-id turn-id event))
         (selected-turn-id e-chat-activity--progress-turn-id)
         (selected-record (and selected-turn-id
                               (e-chat-activity--existing-turn-record selected-turn-id)))
         (record (e-chat-activity--existing-turn-record render-turn-id)))
    (e-chat-activity--set-turn-time render-turn-id :ended-at created-at)
    (e-chat-activity--settle-open-thinking render-turn-id created-at event-type)
    (when (eq event-type 'failed)
      (e-chat-activity--record-turn-failure render-turn-id payload))
    (e-chat-transcript-render-activity-notice
     (if (eq event-type 'cancelled)
         "Turn cancelled"
       (format "Turn failed: %s"
               (or (plist-get payload :error) "Turn failed")))
     nil
     render-turn-id
     (and record (e-chat-activity--turn-details-text render-turn-id record)))
    ;; Inserting an observed row removes the selected running-status block as
    ;; a physical-buffer operation.  Re-render only that transient; no selected
    ;; turn record or settlement state is changed here.
    (when (and selected-turn-id selected-record
               (equal e-chat-activity--progress-turn-id selected-turn-id))
      (e-chat-activity--render-turn-transient selected-turn-id selected-record))))

(defun e-chat-activity--render-turn-failure
    (turn-id created-at payload &optional ensure-composer)
  "Render failed TURN-ID with CREATED-AT and failure PAYLOAD."
  (e-chat-activity--set-turn-time turn-id :ended-at created-at)
  (e-chat-activity--settle-open-thinking turn-id created-at 'failed)
  (e-chat-activity--stop-progress-indicator turn-id)
  ;; Drop the failed turn's live transient activity ("Thinking..."/"Thought
  ;; for ...") and clear its running-status markers before inserting the
  ;; failure entry.  Without this the transient block and its separators
  ;; linger, and the next submitted prompt renders into the orphaned region
  ;; and appears to vanish.
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (e-chat-activity--delete-turn-transient record))
  (let* ((record (e-chat-activity--record-turn-failure turn-id payload))
         (error-message (or (plist-get payload :error) "Turn failed")))
    (e-chat-transcript-render-activity-notice
     (format "Turn failed: %s" error-message)
     ensure-composer
     turn-id
     (and record (e-chat-activity--turn-details-text turn-id record)))
    (when record
      (plist-put record :failure-rendered t))
    ;; Re-render the settled activity summary ("Turn took ..., N tool calls")
    ;; below the failure entry, so an abnormal end still surfaces duration and
    ;; tool-call counts.  `finalize-turn-display' is a no-op when the turn did
    ;; no provider work (`settled-activity-p' is nil), so failures before any
    ;; round add no empty summary.
    (e-chat-activity--finalize-turn-display turn-id)))

(defun e-chat-activity--render-turn-cancellation
    (turn-id created-at &optional ensure-composer)
  "Render selected TURN-ID as cancelled at CREATED-AT."
  (e-chat-activity--set-turn-time turn-id :ended-at created-at)
  (e-chat-activity--settle-open-thinking turn-id created-at 'cancelled)
  (e-chat-activity--cancel-pending-activity-redraw turn-id)
  (e-chat-activity--stop-progress-indicator turn-id)
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (e-chat-activity--delete-turn-transient record))
  (e-chat-surface-set-status "cancelled")
  (let ((record (e-chat-activity--existing-turn-record turn-id)))
    (e-chat-transcript-render-activity-notice
     "Turn cancelled" ensure-composer turn-id
     (and record (e-chat-activity--turn-details-text turn-id record)))
    ;; Persist the activity summary (duration, tool-call count) below the
    ;; cancellation, matching the failed-turn path.  No-op when the turn did
    ;; no provider work.
    (e-chat-activity--finalize-turn-display turn-id)))

(defun e-chat-activity--finalize-turn-display (turn-id)
  "Mark TURN-ID as having rendered its final response."
  (when-let ((record (e-chat-activity--turn-record turn-id)))
    (plist-put record :final-rendered t)
    (if (e-chat-activity--settled-activity-p record)
        (e-chat-activity--render-turn-transient turn-id record)
      (e-chat-activity--delete-turn-transient record))
    nil))

(defun e-chat-activity--settle-successful-turn (turn-id ended-at)
  "Settle successful activity state for TURN-ID at ENDED-AT.
This is the activity-side half of the terminal transition.  The facade may
perform shell composition after this operation, but it never edits activity
records or classifies activity events."
  (let ((output-tail-windows
         (e-chat-surface-capture-live-output-follow-windows)))
    (e-chat-activity--set-turn-time turn-id :ended-at ended-at)
    (e-chat-activity--settle-open-thinking turn-id ended-at 'done)
    (e-chat-activity--cancel-pending-activity-redraw turn-id)
    (e-chat-activity--stop-progress-indicator turn-id)
    (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
      (when (plist-get record :assistant-output-rendered)
        (e-chat-activity--finalize-turn-display turn-id)))
    (e-chat-surface-restore-output-tail-windows output-tail-windows))
  :settled)

(defun e-chat-activity--handle-event (event)
  "Apply the activity-owned transition for harness EVENT.
Return nil when the facade still owns a durable/shell action.  Activity event
classification and all activity record transitions live here so adding a new
provider/tool activity does not require a central per-record dispatch branch."
  (pcase (plist-get event :type)
    ('turn-started
     (when (e-chat-transcript-event-selected-participant-p event)
       (let ((turn-id (plist-get event :turn-id)))
         (e-chat-activity--set-turn-time
          turn-id :started-at (plist-get event :created-at))
         (when-let ((pending-summary
                     (e-chat-activity--turn-pending-hook-summary turn-id)))
           (plist-put (e-chat-activity--turn-record turn-id)
                      :pending-hook-summary pending-summary))
         (e-chat-activity--start-progress-indicator turn-id)
         (e-chat-surface-set-status (format "running %s" turn-id))))
     t)
    ('turn-finished
     (when (e-chat-transcript-event-selected-participant-p event)
       (e-chat-activity--settle-successful-turn
        (plist-get event :turn-id)
        (plist-get event :created-at))))
    ('turn-failed
     (if (e-chat-transcript-event-selected-participant-p event)
         (let ((output-tail-windows
                (e-chat-surface-capture-live-output-follow-windows)))
           (e-chat-activity--cancel-pending-activity-redraw
            (plist-get event :turn-id))
           (e-chat-surface-set-status "error")
           (e-chat-activity--render-turn-failure
            (plist-get event :turn-id)
            (plist-get event :created-at)
            (plist-get event :payload)
            t)
           (e-chat-surface-restore-output-tail-windows output-tail-windows))
       (e-chat-activity--render-observed-terminal-event
        (plist-get event :turn-id)
        (plist-get event :created-at)
        'failed
        (plist-get event :payload)
        event))
     t)
    ('turn-cancelled
     (if (e-chat-transcript-event-selected-participant-p event)
         (let* ((turn-id (plist-get event :turn-id))
                (created-at (plist-get event :created-at))
                (output-tail-windows
                 (e-chat-surface-capture-live-output-follow-windows)))
           (e-chat-activity--render-turn-cancellation turn-id created-at t)
           (e-chat-surface-restore-output-tail-windows output-tail-windows))
       (e-chat-activity--render-observed-terminal-event
        (plist-get event :turn-id)
        (plist-get event :created-at)
        'cancelled
        (plist-get event :payload)
        event))
     t)
    ('message-added
     ;; The durable transcript row remains a facade composition step, but the
     ;; activity owner decides whether the message is an activity-only tool
     ;; row and prepares its own detail projection.  Returning a small value
     ;; keeps message classification out of the facade without leaking an
     ;; activity record or transcript representation.
     (let* ((message (plist-get (plist-get event :payload) :message))
            (turn-id (plist-get event :turn-id))
            (render-turn-id
             (e-chat-transcript-presentation-turn-id turn-id event))
            (tool-message-p (e-chat-activity--tool-message-p message)))
       (list :operation 'message-added
             :render-p (not tool-message-p)
             :turn-id render-turn-id
             :details-text
             (unless tool-message-p
               (e-chat-activity--prepare-message render-turn-id message))
             :assistant-p (and (not tool-message-p)
                              (eq (plist-get message :role) 'assistant))
             :terminal-output-p
             (and (not tool-message-p)
                  (eq (plist-get message :role) 'assistant)
                  (plist-get message :terminal-output)
                  (e-chat-transcript-event-selected-participant-p event)))))
    ('message-updated
     (e-chat-activity--reconcile-message-display
      (plist-get (plist-get event :payload) :message))
     :message-updated)
    ('provider-request-started
     (let ((turn-id (e-chat-transcript-presentation-turn-id
                     (plist-get event :turn-id) event)))
       (when (e-chat-transcript-event-selected-participant-p event)
         (e-chat-activity-set-assistant-streaming nil)
         (e-chat-surface-set-status "waiting for provider"))
       (e-chat-activity--record-provider-started
        turn-id (plist-get event :created-at))
       (e-chat-activity--request-activity-redraw turn-id))
     t)
    ('provider-request-finished
     (let ((turn-id (e-chat-transcript-presentation-turn-id
                     (plist-get event :turn-id) event)))
       (e-chat-activity--record-provider-finished
        turn-id
        (plist-get event :created-at)
        (plist-get (plist-get event :payload) :status))
       (e-chat-activity--request-activity-redraw turn-id))
     t)
    ;; Raw reasoning is an audit/provider event, not classic visible activity.
    ;; Claim it here so the facade does not fall through to its generic System
    ;; event renderer on live delivery; replay already ignores this event in
    ;; the activity record owner above.
    ('reasoning-raw-delta t)
    ;; Frame consumption is private lifetime audit.  A package-bearing event
    ;; is projected to the content-free `context-curated' activity before it
    ;; reaches presentation; the raw event must never fall through to the
    ;; facade's generic System renderer.
    ('context-frame-consumed t)
    ('turn-retrying
     (let* ((turn-id (e-chat-transcript-presentation-turn-id
                      (plist-get event :turn-id) event))
            (payload (plist-get event :payload))
            (attempt (plist-get payload :attempt))
            (backoff (plist-get payload :backoff-seconds)))
       (e-chat-activity--record-turn-retrying turn-id payload)
       (when (e-chat-transcript-event-selected-participant-p event)
         (e-chat-surface-set-status
          (format "retry %s in %.0fs" (or attempt 1) (or backoff 0))))
       (e-chat-activity--request-activity-redraw turn-id 'activity))
     t)
    ('turn-steered
     (let* ((turn-id (e-chat-transcript-presentation-turn-id
                      (plist-get event :turn-id) event))
            (record (e-chat-activity--turn-record turn-id)))
       (e-chat-activity--record-steering-input
        record (plist-get (plist-get event :payload) :prompt-preview))
       (when (e-chat-transcript-event-selected-participant-p event)
         (e-chat-surface-set-status "steered"))
       (e-chat-activity--request-activity-redraw turn-id 'activity))
     t)
    ('assistant-delta
     (when (e-chat-transcript-event-selected-participant-p event)
       (e-chat-activity-set-assistant-streaming t)
       (e-chat-surface-set-status "streaming"))
     t)
    ((or 'reasoning-delta 'tool-started 'tool-finished 'action-started
         'action-finished 'action-failed 'hook-audit 'context-curated
         'tool-progress)
     (let* ((turn-id (e-chat-transcript-presentation-turn-id
                      (plist-get event :turn-id) event))
            (payload (plist-get event :payload))
            (record (e-chat-activity--turn-record turn-id))
            (event-type (plist-get event :type)))
       (pcase event-type
         ('reasoning-delta
          (e-chat-activity--record-reasoning-delta
           record
           (plist-get payload :content)
           (e-chat-activity--reasoning-append-p payload)
           'activity)
          (when (e-chat-transcript-event-selected-participant-p event)
            (e-chat-surface-set-status "reasoning")))
         ('tool-started
          (e-chat-activity--record-tool-started
           record payload 'activity (plist-get event :created-at))
          (when (e-chat-transcript-event-selected-participant-p event)
            (e-chat-surface-set-status "tool")))
         ('tool-finished
          (e-chat-activity--record-tool-finished
           record payload 'activity (plist-get event :created-at))
          (when (e-chat-transcript-event-selected-participant-p event)
            (e-chat-surface-set-status "tool done")))
         ('action-started
          (e-chat-activity--record-action-started record payload 'activity)
          (when (e-chat-transcript-event-selected-participant-p event)
            (e-chat-surface-set-status "action")))
         ((or 'action-finished 'action-failed)
          (e-chat-activity--record-action-finished record payload 'activity)
          (when (e-chat-transcript-event-selected-participant-p event)
            (e-chat-surface-set-status "action done")))
         ('hook-audit
          (e-chat-activity--record-hook-audit record payload 'activity))
         ('context-curated
          (e-chat-activity--record-context-curated record event))
         ('tool-progress
          (e-chat-activity--record-tool-progress record payload)
          (when (e-chat-transcript-event-selected-participant-p event)
            (e-chat-surface-set-status "tool output")))
         (_ nil))
       (e-chat-activity--request-activity-redraw turn-id 'activity))
     t)
    ('backend-empty-output
     (when (e-chat-transcript-event-selected-participant-p event)
       (e-chat-activity--cancel-pending-activity-redraw
        (plist-get event :turn-id))
       (e-chat-activity--stop-progress-indicator
        (plist-get event :turn-id))
       (e-chat-surface-set-status "done"))
     t)
    ('token-usage
     (when (e-chat-transcript-event-selected-participant-p event)
       (e-chat-surface-request-mode-line-status-refresh t))
     t)
    ('provider-anchor-candidate t)
    ('turn-summary
     (let* ((turn-id (e-chat-transcript-presentation-turn-id
                      (plist-get event :turn-id) event))
            (record (e-chat-activity--turn-record turn-id)))
       (e-chat-activity--record-turn-summary record event)
       (e-chat-activity--request-activity-redraw turn-id 'activity))
     t)
    ('session-reset
     (e-chat-activity-reset)
     (e-chat-surface-set-status "idle")
     :session-reset)
    (_ nil)))

(defun e-chat-activity--note-message-rendered
    (turn-id message terminal-p &optional ended-at)
  "Record that MESSAGE was rendered for TURN-ID.
When TERMINAL-P is non-nil, settle the activity side of the turn.  This also
reinstates the selected transient after an observed sibling message is
inserted, without exposing activity records to the facade."
  (let ((assistant-p (eq (plist-get message :role) 'assistant)))
    (when (and (not (e-chat-transcript-message-selected-participant-p message))
               e-chat-activity--progress-turn-id)
      (e-chat-activity--render-turn-transient
       e-chat-activity--progress-turn-id))
    (when assistant-p
      (plist-put (e-chat-activity--turn-record turn-id)
                 :assistant-output-rendered t)
      (when terminal-p
        (e-chat-activity--settle-successful-turn
         turn-id
         (or ended-at (e-chat-activity--current-time-seconds)))))))



;;; Public activity contract

(defun e-chat-activity-render-replay (messages &optional activity-events)
  "Render bounded transcript MESSAGES with ACTIVITY-EVENTS."
  (e-chat-activity--render-replay messages activity-events))

(defun e-chat-activity-run-pending-redraw (&optional expected-generation)
  "Run the pending activity redraw for EXPECTED-GENERATION."
  (e-chat-activity--run-pending-activity-redraw expected-generation))

(defun e-chat-activity-cancel-pending-redraw (&optional turn-id)
  "Cancel pending activity redraw for TURN-ID."
  (e-chat-activity--cancel-pending-activity-redraw turn-id))

(defun e-chat-activity-start-progress (turn-id)
  "Start activity progress presentation for TURN-ID."
  (e-chat-activity--start-progress-indicator turn-id))

(defun e-chat-activity-stop-progress (&optional turn-id)
  "Stop activity progress presentation for TURN-ID."
  (e-chat-activity--stop-progress-indicator turn-id))

(defun e-chat-activity-handle-event (event)
  "Apply EVENT's activity transition and return its composition result."
  (e-chat-activity--handle-event event))

(defun e-chat-activity-message-rendered
    (turn-id message terminal-p &optional ended-at)
  "Record rendered MESSAGE and optionally settle its terminal activity turn."
  (e-chat-activity--note-message-rendered
   turn-id message terminal-p ended-at))

(defun e-chat-activity-failed-turn-p (turn-id)
  "Return non-nil when TURN-ID has an activity failure recorded."
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (and (plist-get record :failure-error) t)))

(defun e-chat-activity-turn-display (turn-id)
  "Return semantic display state for TURN-ID.
The returned plist contains text and scalar status values only.  Activity
records, provider-round plists, and transcript block metadata remain private
to their owning components."
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (list :turn-id turn-id
          :summary-text (e-chat-activity--activity-summary-text record)
          :expanded-text (e-chat-activity--activity-expanded-text record)
          :transient-text (e-chat-activity--transient-text record)
          :details-text (plist-get record :details-text)
          :settled (and (e-chat-activity--settled-activity-p record) t)
          :failed (and (plist-get record :failure-error) t)
          :round-count (length (e-chat-activity--activity-records record))
          :round-statuses
          (mapcar (lambda (round) (plist-get round :status))
                  (e-chat-activity--activity-records record))
          :tool-count (e-chat-activity--activity-tool-count record)
          :action-count (e-chat-activity--activity-action-count record)
          :tool-items
          (mapcar
           (lambda (item)
             (list :id (plist-get item :id)
                   :name (or (plist-get item :name)
                             (plist-get item :call))
                   :call (plist-get item :call)
                   :output (plist-get item :output)
                   :progress (plist-get item :progress)))
           (e-chat-activity--activity-tool-items record)))))

(defun e-chat-activity-advance-progress ()
  "Advance the current activity progress projection once.
The operation is semantic: callers do not need to know how the animation or
its deferred redraw work is represented."
  (e-chat-activity--advance-progress-indicator))

(defun e-chat-activity-progress-state (&optional buffer)
  "Return scalar progress state for BUFFER's activity owner."
  (with-current-buffer (or buffer (current-buffer))
    (list :turn-id e-chat-activity--progress-turn-id
          :frame e-chat-activity--progress-frame
          :interval-active-p
          (and (e-work-handle-p e-chat-activity--progress-interval-handle)
               t)
          :next-tick-time e-chat-activity--progress-next-tick-time)))

(defun e-chat-activity-redraw-state (&optional buffer)
  "Return scalar deferred-redraw state for BUFFER's activity owner."
  (with-current-buffer (or buffer (current-buffer))
    (list :turn-id e-chat-activity--pending-activity-redraw-turn-id
          :kind e-chat-activity--pending-activity-redraw-kind
          :deferred-kind
          (cdr e-chat-activity--deferred-activity-redraw)
          :generation e-chat-activity--pending-activity-redraw-generation
          :deferred (and e-chat-activity--deferred-activity-redraw t)
          :pending-handle-p
          (and (e-work-handle-p e-chat-activity--pending-activity-redraw-handle)
               t))))

(defun e-chat-activity-redraw-delay ()
  "Return the current semantic activity redraw coalescing delay."
  (e-chat-activity--activity-redraw-delay))

(defun e-chat-activity-active-p (turn-id)
  "Return non-nil when TURN-ID has an active provider activity round."
  (when-let ((record (e-chat-activity--existing-turn-record turn-id)))
    (and (e-chat-activity--active-activity-p record) t)))

(defun e-chat-activity-replay-events (turn-id activity-events)
  "Replay ACTIVITY-EVENTS for TURN-ID and return semantic display state.
This operation is intended for embedding shells that need to reconstruct an
activity summary without constructing a composed chat facade.  It records the
events in the activity owner's local registry and returns the same scalar/text
contract as `e-chat-activity-turn-display'; no mutable record escapes."
  (when turn-id
    (dolist (event activity-events)
      (let ((event (copy-sequence event)))
        (when (plist-get event :type)
          (setq event
                (plist-put event :event-type (plist-get event :type))))
        (setq event (plist-put event :turn-id turn-id))
        (e-chat-activity--record-activity-event turn-id event)))
    (e-chat-activity-turn-display turn-id)))

(defun e-chat-activity-running-status-turn-id (&optional buffer)
  "Return the turn id represented by BUFFER's running status."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-activity--running-status-turn-id)))

(defun e-chat-activity-delete-turn-transient (&optional turn-id)
  "Remove the transient activity projection for TURN-ID.
TURN-ID is a semantic identity; activity records remain private to this owner."
  (e-chat-activity--delete-turn-transient turn-id))

(defun e-chat-activity-render-turn-transient (turn-id)
  "Render the current transient activity projection for TURN-ID."
  (e-chat-activity--render-turn-transient turn-id))

(defun e-chat-activity-set-assistant-streaming (streaming)
  "Set whether the current selected turn is streaming."
  (setq-local e-chat-activity--assistant-streaming-p (and streaming t)))

(defun e-chat-activity-assistant-streaming-p (&optional buffer)
  "Return whether BUFFER's activity owner is streaming."
  (with-current-buffer (or buffer (current-buffer))
    e-chat-activity--assistant-streaming-p))

(defun e-chat-activity-progress-turn-id (&optional buffer)
  "Return the active progress turn id for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'e-chat-activity--progress-turn-id)
         e-chat-activity--progress-turn-id)))

(defun e-chat-activity-flush-deferred-redraws (&rest args)
  "Flush activity redraws deferred by hidden or changing windows."
  (apply #'e-chat-activity--flush-deferred-activity-redraws args))

(defun e-chat-activity-flush-deferred-redraws-after-minibuffer (&rest args)
  "Flush activity redraws after a minibuffer exits."
  (apply #'e-chat-activity--flush-deferred-activity-redraws-after-minibuffer args))

(defun e-chat-activity-add-rendered-hook (function &optional append local)
  "Add FUNCTION to the post-progress-redraw hook in the current buffer."
  (add-hook 'e-chat-activity--running-status-rendered-hook function append local))

(defun e-chat-activity-reset ()
  "Reset transient activity state in the current chat buffer."
  (let ((turn-id e-chat-activity--progress-turn-id))
    (e-chat-activity--cancel-pending-activity-redraw turn-id)
    (e-chat-activity--cancel-progress-interval)
    (when turn-id
      (e-chat-transcript-remove-activity)))
  (setq-local e-chat-activity--progress-turn-id nil
              e-chat-activity--progress-frame 0
              e-chat-activity--progress-next-tick-time nil
              e-chat-activity--activity-redraw-running nil
              e-chat-activity--deferred-activity-redraw nil
              e-chat-activity--rendered-turn-id nil
              e-chat-activity--rendered-activity-size 0
              e-chat-activity--turn-registry (make-hash-table :test 'equal))
  t)

(provide 'e-chat-activity)

;;; e-chat-activity.el ends here

;;; e-bayesian-reasoning.el --- Bayesian reasoning disposition for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Harness-owned Bayesian calibration disposition.  The capability contributes
;; a compact standing-context nudge, a durable fetch-on-demand reference holding
;; the eight tenets in full, a hidden inline reasoning-mark block kind, and a
;; conditional `:turn-finished' stop-hook.  The hook is cheap: it acts only on a
;; turn that either carries a reasoning mark or trips a coarse bare-assertion
;; filter, runs a deterministic completeness check over the parsed mark's
;; fields (never a prose-calibration judgment), and on failure requests exactly
;; one corrective follow-up turn.  It never rewrites the model's reply.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-context)
(require 'e-hooks)
(require 'e-store)
(require 'e-structured-blocks)
(require 'subr-x)

(declare-function e-harness-request-follow-up "e-harness"
                  (harness session-id prompt &rest args))
(declare-function e-harness-messages "e-harness" (harness session-id))
(declare-function e-harness-session-activity-events "e-harness"
                  (harness session-id))
(declare-function e-harness-set-message-display "e-harness"
                  (harness session-id message-id display))
(declare-function e-harness-record-hook-audit "e-harness"
                  (harness session-id turn-id &rest args))

(defconst e-bayesian-reasoning-instructions
  "Treat belief as a quantity, not a verdict: attach a confidence to factual claims, keep at least one rival hypothesis alive, and prefer \"insufficient evidence\" to a guess. Read e://bayesian-reasoning/refs/tenets.md before concluding on a load-bearing claim. When you assert a load-bearing factual conclusion, emit exactly one reserved fenced reasoning block: under org output mode, `#+begin_reasoning' ... `#+end_reasoning'; under markdown output mode, a ```reasoning fence. Inside it write four lines: `claim:' the one-line conclusion, `confidence:' one of low, medium, or high, `alternatives:' at least one rival explanation or the literal `insufficient-evidence', and `evidence:' a comma-separated list of the `ev:' or `in:' evidence handles provided in context. Only an explicit `insufficient-evidence' abstention may leave evidence blank. This block is hidden from the rendered reply and read only by deterministic tooling."
  "Compact model-facing disposition for calibrated reasoning.")

(defconst e-bayesian-reasoning--fence-regexp
  "\\`\\(?:#\\+begin_reasoning\\|```reasoning\\)[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n?\\(?:#\\+end_reasoning\\|```\\)[ \t]*\n?\\'"
  "Matches a whole reasoning fence (org or markdown form) and captures its body.
Anchored to the full string so a caller must pass exactly the fenced text --
any surrounding prose makes the match fail, which is the desired `malformed'
outcome for a non-fence string.")

(defconst e-bayesian-reasoning--field-line-regexp
  "\\`[ \t]*\\([a-zA-Z_]+\\):[ \t]*\\(.*\\)[ \t]*\\'"
  "Matches one `key: value' line inside a reasoning fence body.")

(defconst e-bayesian-reasoning--confidence-bands '("low" "medium" "high")
  "Allowed values for the reasoning fence's `confidence' field.")

(defconst e-bayesian-reasoning--hook-id
  "60-bayesian-reasoning-turn-finished"
  "Stable identifier for the capability's claim-check hook.")

(defconst e-bayesian-reasoning--high-risk-request-regexp
  "\\b\\(why\\|cause\\|root cause\\|diagnos\\|explain\\|does .*work\\|current\\|latest\\|recommend\\|compare\\|should we\\|is .*loaded\\|what did\\)\\b"
  "Narrow input signal for requests that need a factual conclusion.")

(defun e-bayesian-reasoning--fence-matches (content open close)
  "Return match plists for one OPEN...CLOSE fenced form in CONTENT.
Each match is `(:start START :end END)', 0-based offsets spanning the whole
fence including its delimiters."
  (let (matches (start 0))
    (while (and (< start (length content))
               (string-match (regexp-quote open) content start))
      (let* ((match-start (match-beginning 0))
             (close-pos (string-match (regexp-quote close) content (match-end 0))))
        (if close-pos
            (progn
              (push (list :start match-start :end (+ close-pos (length close)))
                    matches)
              (setq start (+ close-pos (length close))))
          (setq start (length content)))))
    (nreverse matches)))

(defun e-bayesian-reasoning--reasoning-matcher (content)
  "Matcher for the `reasoning' structured-block kind.
Finds both the org fence (`#+begin_reasoning' ... `#+end_reasoning') and the
markdown fence (```reasoning ... ```) in CONTENT, since the matcher does not
know which output mode produced the reply; it must recognize whichever fence
the model actually emitted.  Returns matches sorted ascending by start, per
the registry's matcher contract."
  (let ((matches (append (e-bayesian-reasoning--fence-matches
                          content "#+begin_reasoning" "#+end_reasoning")
                         (e-bayesian-reasoning--fence-matches
                          content "```reasoning" "```"))))
    (sort matches (lambda (a b) (< (plist-get a :start) (plist-get b :start))))))

(defun e-bayesian-reasoning-parse-reasoning-block (string)
  "Parse a reasoning fence STRING into its field plist, or nil when malformed.
STRING must be exactly one whole fence (org or markdown form), such as the
`:text' of a block matched by `e-bayesian-reasoning--reasoning-matcher'.  On a
well-formed fence, returns a plist `(:claim :confidence :alternatives
:evidence)' with each value the trimmed string after its `key:'; a missing
field is nil in the plist.  Returns nil when STRING is not a whole fence, or
when it has no recognized `key: value' lines at all -- this is a deterministic
string parse, never a completeness judgment, which stays with the Slice 3
stop-hook checklist."
  (when (and (stringp string)
            (string-match e-bayesian-reasoning--fence-regexp string))
    (let ((body (match-string 1 string))
          (fields nil))
      (dolist (line (split-string body "\n"))
        (when (string-match e-bayesian-reasoning--field-line-regexp line)
          (let ((key (downcase (match-string 1 line)))
                (value (string-trim (match-string 2 line))))
            (cond
             ((equal key "claim") (setq fields (plist-put fields :claim value)))
             ((equal key "confidence")
              (setq fields (plist-put fields :confidence value)))
             ((equal key "alternatives")
              (setq fields (plist-put fields :alternatives value)))
             ((equal key "evidence")
              (setq fields (plist-put fields :evidence value)))))))
      fields)))

(defun e-bayesian-reasoning--evidence-refs (mark)
  "Return ordered evidence handles named by MARK's `evidence:' field.

The v2 grammar is deliberately small: a comma-separated list of `ev:' tool
result handles or `in:' user-input handles.  Keeping the parser's historical
field plist intact lets old transcripts still render, while the checker below
can reject their opaque evidence prose instead of treating it as provenance."
  (let ((evidence (plist-get mark :evidence)))
    (and evidence
         (not (string-empty-p evidence))
         (mapcar #'string-trim (split-string evidence "," t "[ \t\n]+")))))

(defun e-bayesian-reasoning--evidence-handle-p (reference)
  "Return non-nil when REFERENCE uses the v2 evidence-handle grammar."
  (and (stringp reference)
       (string-match-p "\\`\\(?:ev\\|in\\):[0-9A-HJKMNP-TV-Z]+\\'" reference)))

(defun e-bayesian-reasoning--message-index (messages message)
  "Return MESSAGE's position in MESSAGES, or nil when it is absent."
  (cl-position message messages :test #'eq))

(defun e-bayesian-reasoning--message-for-handle (messages reference)
  "Return the durable message named by v2 evidence REFERENCE in MESSAGES."
  (let ((id (and (stringp reference) (substring reference 3))))
    (seq-find (lambda (message) (equal (plist-get message :id) id)) messages)))

(defun e-bayesian-reasoning--resolve-evidence-refs (context mark)
  "Resolve MARK's v2 evidence references against CONTEXT's session transcript.

Returns `(:resolved ... :rejected ...)' where each resolved item snapshots only
stable provenance fields.  A resolvable handle proves that a cited input or
successful tool result existed before the response; it deliberately makes no
claim about the truth of the response."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (assistant (plist-get context :assistant-message))
         (messages (and harness session-id
                        (e-harness-messages harness session-id)))
         (assistant-index (and messages assistant
                               (e-bayesian-reasoning--message-index
                                messages assistant)))
         (seen (make-hash-table :test 'equal))
         resolved rejected)
    (dolist (reference (e-bayesian-reasoning--evidence-refs mark))
      (cond
       ((or (not (e-bayesian-reasoning--evidence-handle-p reference))
            (gethash reference seen))
        (push (list :reference reference
                    :reason (if (gethash reference seen) 'duplicate 'malformed))
              rejected))
       (t
        (puthash reference t seen)
        (let* ((message (e-bayesian-reasoning--message-for-handle messages reference))
               (message-index (and message
                                   (e-bayesian-reasoning--message-index
                                    messages message)))
               (kind (substring reference 0 2))
               (content (and message (plist-get message :content))))
          (cond
           ((null message)
            (push (list :reference reference :reason 'unknown) rejected))
           ((and assistant-index (>= message-index assistant-index))
            (push (list :reference reference :reason 'future) rejected))
           ((and (equal kind "ev")
                 (eq (plist-get message :role) 'tool)
                 (equal (plist-get content :status) 'ok))
            (push (list :reference reference
                        :message-id (plist-get message :id)
                        :turn-id (plist-get message :turn-id)
                        :source-kind 'tool-result
                        :tool-call-id (plist-get content :tool-call-id))
                  resolved))
           ((and (equal kind "in") (eq (plist-get message :role) 'user))
            (push (list :reference reference
                        :message-id (plist-get message :id)
                        :turn-id (plist-get message :turn-id)
                        :source-kind 'user-provided)
                  resolved))
           (t
            (push (list :reference reference :reason 'wrong-source) rejected)))))))
    (list :resolved (nreverse resolved) :rejected (nreverse rejected))))

(cl-defun e-bayesian-reasoning--current-turn-evidence-context
    (&key harness session-id turn-id &allow-other-keys)
  "Return compact v2 evidence handles available during TURN-ID.

The provider derives handles from immutable transcript messages.  It stores no
separate ledger and lists only the current prompt plus successful tool results
from this turn, keeping the prompt cost bounded."
  (when-let ((messages (and harness session-id
                            (e-harness-messages harness session-id))))
    (let (lines)
      (dolist (message messages)
        (when (equal (plist-get message :turn-id) turn-id)
          (pcase (plist-get message :role)
            ('user
             (push (format "in:%s — current user statement"
                           (plist-get message :id)) lines))
            ('tool
             (let ((content (plist-get message :content)))
               (when (equal (plist-get content :status) 'ok)
                 (push (format "ev:%s — successful tool result"
                               (plist-get message :id)) lines)))))))
      (when lines
        (list
         (list :role 'system
               :content
               (concat "Evidence handles for this turn. Cite only these exact "
                       "handles in a reasoning block's evidence field; a handle "
                       "establishes provenance, not truth.\n"
                       (mapconcat #'identity (nreverse lines) "\n"))))))))

(defun e-bayesian-reasoning--turn-user-content (context)
  "Return the current turn's user text from CONTEXT, when available."
  (when-let* ((harness (plist-get context :harness))
              (session-id (plist-get context :session-id))
              (turn-id (plist-get context :turn-id))
              (message (seq-find
                        (lambda (candidate)
                          (and (eq (plist-get candidate :role) 'user)
                               (equal (plist-get candidate :turn-id) turn-id)))
                        (e-harness-messages harness session-id))))
    (plist-get message :content)))

(defun e-bayesian-reasoning--high-risk-reasons (context)
  "Return deterministic high-risk reasons for the current request in CONTEXT."
  (let ((prompt (e-bayesian-reasoning--turn-user-content context)))
    (when (and (stringp prompt)
               (let ((case-fold-search t))
                 (string-match-p e-bayesian-reasoning--high-risk-request-regexp
                                 prompt)))
      '(input-classifier))))

(defconst e-bayesian-reasoning-tenets-reference
  (string-join
   '("# Bayesian reasoning tenets"
     ""
     "Eight core techniques for calibrated reasoning."
     "Each is stated first as the general principle, then as the behavior it should produce in an agent."
     ""
     "## 1. Start from an explicit prior"
     ""
     "Before looking at the specific evidence, ask how likely the hypothesis is in general."
     "A belief needs a starting value so that later evidence has something to move."
     "In an agent: state an initial expectation before reading the file, running the query, or trusting the user's framing, so the first plausible-looking detail cannot set the whole conclusion."
     ""
     "## 2. Think in degrees of belief, not verdicts"
     ""
     "Represent a conclusion as a probability or a confidence level, not a yes/no fact."
     "A 70% belief and a 99% belief should be acted on differently."
     "In an agent: prefer \"most likely X, but Y is still open\" over asserting X flatly, and let low confidence trigger a check rather than a claim."
     ""
     "## 3. Weigh evidence by its likelihood ratio, not its vividness"
     ""
     "Evidence should shift belief only to the degree it is more expected under one hypothesis than another."
     "Ask both P(evidence | hypothesis) and P(evidence | not hypothesis)."
     "In an agent: a log line, a stack trace, or a matching name only counts as strong evidence if it would be unlikely under the alternatives; a detail that fits every explanation is not evidence at all."
     ""
     "## 4. Keep the base rate in view"
     ""
     "Rare things stay rare even when a symptom points at them."
     "Neglecting the base rate is the most common way a confident-sounding inference goes wrong."
     "In an agent: when a striking but uncommon cause suggests itself, weigh it against how often that cause actually occurs before ranking it first."
     ""
     "## 5. Carry several hypotheses at once, plus a catch-all"
     ""
     "Maintain a small set of competing explanations and an explicit \"none of these / something else\" option."
     "The probabilities across the set should stay coherent, since they compete for the same belief mass."
     "In an agent: resist locking onto the first explanation; list two or three and note what the residual unknown could be, which directly counters tunnel vision."
     ""
     "## 6. Update incrementally: today's posterior is tomorrow's prior"
     ""
     "Fold in one piece of evidence at a time and carry the revised belief forward."
     "Do not re-derive everything from scratch, and do not throw out prior belief because of one new fact."
     "In an agent: adjust confidence step by step as tool results arrive, rather than swinging from certain-of-A to certain-of-B on a single observation."
     ""
     "## 7. Seek the most discriminating evidence, including disconfirming evidence"
     ""
     "Prefer the test whose outcome would most separate the live hypotheses."
     "Actively look for what would prove the leading guess wrong, not just what would confirm it."
     "In an agent: choose the next command or query by how much it would change the ranking, and deliberately check the case that would falsify the current favorite before concluding."
     ""
     "## 8. Calibrate and state uncertainty honestly"
     ""
     "Aim for confidence that matches reality: claims made at 80% should be right about 80% of the time."
     "Report the confidence and the assumptions behind it."
     "In an agent: attach a real confidence to conclusions, surface the assumptions that would change the answer, and treat \"I don't have enough to say\" as a valid, preferred output over a fabricated specific.")
   "\n")
  "Detailed tenets reference exposed as e://bayesian-reasoning/refs/tenets.md.")

(defun e-bayesian-reasoning--claim-audit-resource (harness session-id)
  "Render Bayesian hook-audit records for SESSION-ID as an e:// resource."
  (if (not (and harness session-id))
      "No session is selected, so no claim audits are available.\n"
    (let ((audits (seq-filter
                   (lambda (event)
                     (eq (plist-get (plist-get event :payload) :owner)
                         'bayesian-reasoning))
                   (e-harness-session-activity-events harness session-id))))
      (if (null audits)
          "No Bayesian claim audits were recorded for this session.\n"
        (concat
         "# Bayesian claim audits\n\n"
         (mapconcat
          (lambda (event)
            (let* ((payload (plist-get event :payload))
                   (details (plist-get payload :details)))
              (format
               "## Turn %s\n- outcome: %s\n- truth status: %s\n- correction: %s\n- resolved references: %S\n- rejected references: %S\n"
               (plist-get event :turn-id)
               (plist-get payload :outcome)
               (plist-get payload :truth-status)
               (or (plist-get details :correction) 'none)
               (plist-get details :resolved)
               (plist-get details :rejected))))
          audits
          "\n"))))))

(defun e-bayesian-reasoning--resource-provider ()
  "Return context-aware resource provider for Bayesian references and audits."
  (lambda (store capability &rest context)
    (e-store-register
     store
     (e-capability-id capability)
     "refs/tenets.md"
     :description "The eight Bayesian reasoning tenets in full."
     :content e-bayesian-reasoning-tenets-reference)
    (let ((harness (plist-get context :harness))
          (session-id (plist-get context :session-id)))
      (e-store-register
       store
       (e-capability-id capability)
       "claim-audits"
       :description "Durable provenance and correction outcomes for Bayesian claim checks."
       :reader (lambda (_entry _range)
                 (e-bayesian-reasoning--claim-audit-resource harness session-id))))))

;;;; Slice 3: conditional turn-finished stop-hook

(defconst e-bayesian-reasoning--follow-up-marker
  "bayesian-reasoning-follow-up"
  "Turn-metadata marker on a corrective follow-up turn.
The hook tags the follow-up it requests so it never re-fires enforcement on
the turn it generated, which would otherwise oscillate.")

(defconst e-bayesian-reasoning--specific-regexp
  "\\(?:[$€£][0-9]\\|[0-9][0-9.,]*%?\\|[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\|[A-Z][a-zA-Z0-9_]*[A-Z][a-zA-Z0-9_]*\\)"
  "Coarse bare-assertion tripwire: a number, money, ISO date, or CamelCase name.
Deliberately dumb -- it only decides whether an unmarked reply looks like it
asserted something concrete, so the hook checks it.  False positives cost one
extra follow-up, never a wrong answer, so it is tuned loosely.")

(defcustom e-bayesian-reasoning-enable-bare-assertion-gate nil
  "When non-nil, fire the coarse bare-assertion tripwire on unmarked replies.
Off by default: the tripwire matches any number, date, or CamelCase name, and
ordinary prose is full of those, so enabling it draws a correction on nearly
every normal answer.  The default gate is the self-emitted reasoning mark (gate
1), which fires only when the model actually annotates a load-bearing claim.
Turn this on only where the extra false-positive follow-ups are acceptable."
  :type 'boolean
  :group 'e)

(defun e-bayesian-reasoning--assistant-content (context)
  "Return the finished turn's assistant text from a turn-finished CONTEXT."
  (let ((message (plist-get context :assistant-message)))
    (when message
      (let ((content (plist-get message :content)))
        (cond ((stringp content) content)
              ((listp content) (or (plist-get content :text) ""))
              (t ""))))))

(defun e-bayesian-reasoning--follow-up-turn-p (context)
  "Return non-nil when CONTEXT's finished turn is a corrective follow-up.
The marker rides the turn's user (prompt) message, since that is where a
queued follow-up's `:metadata' lands -- the assistant reply carries no turn
metadata.  Reading the prompt message keeps the hook from recursing on the
turn it generated."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id)))
    (and harness session-id turn-id
         (seq-some
          (lambda (message)
            (and (eq (plist-get message :role) 'user)
                 (equal (plist-get message :turn-id) turn-id)
                 (equal (plist-get (plist-get message :metadata)
                                   :bayesian-reasoning)
                        e-bayesian-reasoning--follow-up-marker)))
          (e-harness-messages harness session-id)))))

(defun e-bayesian-reasoning--marks (content)
  "Return parsed reasoning marks found in CONTENT, newest matcher order.
Each entry is the field plist from `e-bayesian-reasoning-parse-reasoning-block'
for one matched fence."
  (delq nil
        (mapcar (lambda (match)
                  (e-bayesian-reasoning-parse-reasoning-block
                   (substring content
                              (plist-get match :start)
                              (plist-get match :end))))
                (e-bayesian-reasoning--reasoning-matcher content))))

(defun e-bayesian-reasoning--mark-gap (mark)
  "Return the first failing completeness check for reasoning MARK, or nil.
This is a deterministic field-completeness test over the parsed mark, never a
judgment about whether the stated confidence is accurate: confidence must be a
known band; at least one alternative must be named; and every non-abstaining
factual claim must cite evidence handles."
  (let ((confidence (plist-get mark :confidence))
        (alternatives (plist-get mark :alternatives))
        (evidence (plist-get mark :evidence)))
    (cond
     ((not (member confidence e-bayesian-reasoning--confidence-bands))
      "state a confidence of low, medium, or high")
     ((or (null alternatives) (string-empty-p (string-trim alternatives)))
      "name at least one alternative, or `insufficient-evidence'")
     ((and (not (equal (string-trim alternatives) "insufficient-evidence"))
           (or (null evidence) (string-empty-p (string-trim evidence))))
      "cite at least one `ev:' or `in:' evidence handle, or abstain")
     (t nil))))

(defun e-bayesian-reasoning--turn-check (context)
  "Return the Bayesian audit decision for a finished turn, or nil when skipped.

The decision separates marker format, evidence provenance, and correction
policy.  In particular, `references-resolved' never means that the claim is
true: the hook only established that cited durable records preceded it."
  (let* ((content (e-bayesian-reasoning--assistant-content context))
         (marks (and content (e-bayesian-reasoning--marks content)))
         (high-risk-reasons (e-bayesian-reasoning--high-risk-reasons context)))
    (cond
     ((null content) nil)
     ((> (length marks) 1)
      (list :outcome 'format-gap
            :gap "emit exactly one reasoning block"
            :details (list :mark-count (length marks)
                           :high-risk-reasons high-risk-reasons)))
     ((null marks)
      (when (or high-risk-reasons
                (and e-bayesian-reasoning-enable-bare-assertion-gate
                     (let ((case-fold-search nil))
                       (string-match-p e-bayesian-reasoning--specific-regexp
                                       content))))
        (list :outcome 'format-gap
              :gap "add a reasoning block or explicitly abstain with `insufficient-evidence'"
              :details (list :mark-count 0
                             :high-risk-reasons high-risk-reasons
                             :strict-tripwire
                             (and (null high-risk-reasons)
                                  e-bayesian-reasoning-enable-bare-assertion-gate)))))
     (t
      (let* ((mark (car marks))
             (format-gap (e-bayesian-reasoning--mark-gap mark))
             (alternatives (string-trim (or (plist-get mark :alternatives) ""))))
        (cond
         (format-gap
          (list :outcome 'format-gap
                :gap format-gap
                :details (list :mark mark :high-risk-reasons high-risk-reasons)))
         ((equal alternatives "insufficient-evidence")
          (list :outcome 'abstained
                :details (list :mark mark :high-risk-reasons high-risk-reasons)))
         (t
          (let ((resolution (e-bayesian-reasoning--resolve-evidence-refs context mark)))
            (if (plist-get resolution :rejected)
                (list :outcome 'evidence-gap
                      :gap "replace opaque or unresolved evidence with available `ev:' or `in:' handles"
                      :details (append (list :mark mark
                                             :high-risk-reasons high-risk-reasons)
                                       resolution))
              (list :outcome 'references-resolved
                    :details (append (list :mark mark
                                           :high-risk-reasons high-risk-reasons)
                                     resolution)))))))))))

(defun e-bayesian-reasoning--turn-gap (context)
  "Return the corrective gap string for CONTEXT's finished turn, or nil.
Gate: acts only on a turn that carries a reasoning mark, or -- when unmarked --
trips the coarse bare-assertion tripwire.  A trivial or already-well-marked
turn returns nil and the hook does nothing."
  (let ((check (e-bayesian-reasoning--turn-check context)))
    (when (memq (plist-get check :outcome) '(format-gap evidence-gap))
      (plist-get check :gap))))

(defun e-bayesian-reasoning--follow-up-prompt (gap)
  "Return the corrective follow-up prompt naming the specific GAP.
The goal is a better answer, not a shorter one.  The prompt tells the model to
keep the detail its evidence supports and to revise only the unsupported claim,
so the correction improves the reply instead of replacing it with a terse
restatement."
  (concat
   "Your previous answer made a load-bearing factual claim without complete "
   "calibration. Specifically: " gap ". Revise that answer: keep the detail "
   "and structure the evidence supports, and change only the unsupported "
   "claims -- drop them or soften them to match the evidence -- then add a "
   "complete reasoning block. If the evidence does not support a confident "
   "claim, answer with `insufficient-evidence' for that claim while keeping "
   "the rest. Do not fabricate a confidence or evidence you do not have, and "
   "do not discard supported detail merely to shorten the reply."))

(defun e-bayesian-reasoning--audit-summary (outcome correction)
  "Return presentation text for Bayesian OUTCOME and CORRECTION.

The harness persists this as generic event metadata; shells do not need to
know this capability's marker grammar to render it."
  (cond
   ((eq outcome 'references-resolved) "Evidence references resolved")
   ((eq outcome 'abstained) "Claim explicitly abstained for insufficient evidence")
   ((eq correction 'queued) "Claim check needs revision")
   ((eq outcome 'verification-unavailable) "Claim check unavailable")
   (t "Claim check recorded")))

(defun e-bayesian-reasoning--record-audit
    (harness session-id turn-id outcome details correction)
  "Persist one terminal Bayesian audit with OUTCOME and CORRECTION."
  (e-harness-record-hook-audit
   harness session-id turn-id
   :owner 'bayesian-reasoning
   :hook-id e-bayesian-reasoning--hook-id
   :outcome outcome
   :summary (e-bayesian-reasoning--audit-summary outcome correction)
   :details (append details (list :correction correction))))

(defun e-bayesian-reasoning--turn-finished-hook (value context)
  "Conditional `:turn-finished' hook enforcing the reasoning mark.
Returns VALUE unchanged always -- the hook never rewrites the reply.  On a
gated failure it requests exactly one corrective follow-up turn through
`e-harness-request-follow-up', tagged so it does not recurse, and hides both
the machine-authored follow-up prompt and the superseded first attempt so only
the model's revised reply is shown; the first attempt stays in the transcript
for audit.  Every performed check writes a durable hook-audit record."
  (unless (e-bayesian-reasoning--follow-up-turn-p context)
    (when-let* ((check (e-bayesian-reasoning--turn-check context))
                (harness (plist-get context :harness))
                (session-id (plist-get context :session-id))
                (turn-id (plist-get context :turn-id)))
      (let ((outcome (plist-get check :outcome))
            (details (plist-get check :details))
            (gap (plist-get check :gap)))
        (if (not (memq outcome '(format-gap evidence-gap)))
            (e-bayesian-reasoning--record-audit
             harness session-id turn-id outcome details 'none)
          (if (not (fboundp 'e-harness-request-follow-up))
              (e-bayesian-reasoning--record-audit
               harness session-id turn-id 'verification-unavailable
               (append details (list :reason 'follow-up-unavailable)) 'unavailable)
            (condition-case err
                (progn
                  (e-harness-request-follow-up
                   harness session-id
                   (e-bayesian-reasoning--follow-up-prompt gap)
                   :metadata (list :bayesian-reasoning
                                   e-bayesian-reasoning--follow-up-marker
                                   :display 'hidden))
                  (e-bayesian-reasoning--record-audit
                   harness session-id turn-id outcome details 'queued)
                  ;; Hide the reply only after the correction is safely queued.
                  (when-let ((message-id (plist-get
                                          (plist-get context :assistant-message)
                                          :id)))
                    (when (fboundp 'e-harness-set-message-display)
                      (e-harness-set-message-display
                       harness session-id message-id 'hidden))))
              (error
               (e-bayesian-reasoning--record-audit
                harness session-id turn-id 'verification-unavailable
                (append details (list :error (error-message-string err)))
                'unavailable)
               (signal (car err) (cdr err))))))))
  value))

(cl-defun e-bayesian-reasoning-capability-create
    (&key (id 'bayesian-reasoning) (name "Bayesian Reasoning"))
  "Create the Bayesian Reasoning capability."
  (e-capability-create
   :id id
   :name name
   :instruction-priority 255
   :instructions e-bayesian-reasoning-instructions
   :resources (list (e-bayesian-reasoning--resource-provider))
   :context-providers
   (list (e-context-provider-create
          :name 'bayesian-reasoning-evidence-handles
          :priority 255
          :cache-placement 'dynamic-context
          :build #'e-bayesian-reasoning--current-turn-evidence-context))
   :structured-blocks
   (list (e-structured-block-create
          :kind 'reasoning
          :matcher #'e-bayesian-reasoning--reasoning-matcher
          :display 'hidden
          :parser #'e-bayesian-reasoning-parse-reasoning-block))
   :hooks
   (list (e-hook-create
          :id "60-bayesian-reasoning-turn-finished"
          :point :turn-finished
          :description "Conditionally request one calibration follow-up."
          :handler #'e-bayesian-reasoning--turn-finished-hook))))

(provide 'e-bayesian-reasoning)

;;; e-bayesian-reasoning.el ends here

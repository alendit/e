;;; e-bayesian-reasoning.el --- Bayesian reasoning disposition for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Harness-owned Bayesian calibration disposition.  The capability contributes
;; a compact standing-context nudge plus a durable, fetch-on-demand reference
;; holding the eight tenets in full.  It is stateless: the reasoning-mark and
;; stop-hook enforcement described in the plan are later slices, not this one.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-store)
(require 'e-structured-blocks)
(require 'subr-x)

(defconst e-bayesian-reasoning-instructions
  "Treat belief as a quantity, not a verdict: attach a confidence to factual claims, keep at least one rival hypothesis alive, and prefer \"insufficient evidence\" to a guess. Read e://bayesian-reasoning/refs/tenets.md before concluding on a load-bearing claim. When you assert a load-bearing factual conclusion, emit exactly one reserved fenced reasoning block: under org output mode, `#+begin_reasoning' ... `#+end_reasoning'; under markdown output mode, a ```reasoning fence. Inside it write four lines: `claim:' the one-line conclusion, `confidence:' one of low, medium, or high, `alternatives:' at least one rival explanation or the literal `insufficient-evidence', and `evidence:' references or blank. This block is hidden from the rendered reply and read only by deterministic tooling."
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

(defun e-bayesian-reasoning--resource-provider ()
  "Return resource provider for the tenets reference."
  (lambda (store capability)
    (e-store-register
     store
     (e-capability-id capability)
     "refs/tenets.md"
     :description "The eight Bayesian reasoning tenets in full."
     :content e-bayesian-reasoning-tenets-reference)))

(cl-defun e-bayesian-reasoning-capability-create
    (&key (id 'bayesian-reasoning) (name "Bayesian Reasoning"))
  "Create the Bayesian Reasoning capability."
  (e-capability-create
   :id id
   :name name
   :instruction-priority 255
   :instructions e-bayesian-reasoning-instructions
   :resources (list (e-bayesian-reasoning--resource-provider))
   :structured-blocks
   (list (e-structured-block-create
          :kind 'reasoning
          :matcher #'e-bayesian-reasoning--reasoning-matcher
          :display 'hidden
          :parser #'e-bayesian-reasoning-parse-reasoning-block))))

(provide 'e-bayesian-reasoning)

;;; e-bayesian-reasoning.el ends here

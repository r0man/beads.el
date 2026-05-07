;;; audit.el --- CLI flag audit for beads.el  -*- lexical-binding: t; -*-
;;
;; Run via: eldev eval -R --file .cli-audit/20260429-172656/audit.el
;; (Actually: load via -f loadable arg.)
;;
;; Reads:
;;   .cli-audit/20260429-172656/classes.json
;;   .cli-audit/20260429-172656/help/<path>.txt    (cached `bd P --help' output)
;;
;; Writes:
;;   .cli-audit/20260429-172656/flags.md  (per-class slot drift report)
;;   .cli-audit/20260429-172656/inheritance.md (global flags inheritance)
;;
;; Policy note (see CLAUDE.md "Top-level group commands"):
;;   The 24 top-level group commands (`admin`, `ado`, `audit`, `config`,
;;   `dep`, `dolt`, `epic`, `federation`, `formula`, `gate`, `github`,
;;   `gitlab`, `hooks`, `jira`, `label`, `linear`, `merge-slot`, `mol`,
;;   `notion`, `repo`, `rules`, `swarm`, `vc`, `worktree`) plus the
;;   mid-level group `dolt.remote` are *intentionally* not represented
;;   as `beads-defcommand` classes. They are exposed via
;;   `transient-define-prefix' parent menus only. Future revisions of
;;   the upstream coverage report should treat their absence from
;;   classes.json as in-policy and not flag them as gaps.

(require 'cl-lib)
(require 'json)

(add-to-list 'load-path (expand-file-name "lisp"))
(require 'beads)

;; Force-load every beads-command-* module so all classes are defined.
(dolist (m (directory-files "lisp" nil "^beads-command-.+\\.el$"))
  (let ((feat (intern (file-name-sans-extension m))))
    (require feat nil 'noerror)))

(defconst audit-run-dir ".cli-audit/20260429-172656")

;; Intentional cli_path collisions (machine-readable).
;;
;; Some CLI paths are deliberately targeted by more than one
;; `beads-defcommand' class -- typically because the underlying CLI
;; subcommand has mutually-exclusive operating modes selected by flags
;; (e.g. `bd admin compact --analyze' vs `--apply' vs `--auto') and we
;; expose ONE class per mode so the transient surfaces only the slots
;; relevant to the chosen mode.
;;
;; Future audit revisions should consult this list before reporting a
;; collision in `coverage.md' / `REPORT.md' so the policy lives in code
;; instead of in prose. Each entry carries a rationale and the expected
;; member classes; a collision whose member set differs from the entry
;; should still be flagged (intent has drifted).
(defconst audit--intentional-collisions
  '(("admin.compact"
     :rationale "bd admin compact has mutually-exclusive operating modes (--stats, --analyze, --apply, --auto, --dolt). Each Emacs class models one mode so the transient surfaces only the relevant flags. See lisp/beads-command-compact.el header."
     :classes (beads-command-admin-compact
               beads-command-compact-stats
               beads-command-compact-analyze
               beads-command-compact-apply
               beads-command-compact-auto)))
  "Alist of (CLI-PATH . PLIST) for cli_path collisions that are intentional.
PLIST keys: `:rationale' (string), `:classes' (list of expected
member class symbols).  Future audit tooling should suppress these
collisions from the actionable list and re-flag only when the
member set drifts from the recorded intent.")

(defun audit--intentional-collision-p (cli-path classes)
  "Return non-nil if (CLI-PATH . CLASSES) matches an intentional collision.
CLASSES is the list of class symbols that target CLI-PATH."
  (when-let ((entry (cdr (assoc cli-path audit--intentional-collisions))))
    (let ((expected (plist-get entry :classes))
          (actual (sort (copy-sequence classes) #'string<)))
      (equal (sort (copy-sequence expected) #'string<)
             actual))))

;;; ============================================================
;;; CLI help parsing
;;; ============================================================

(defun audit--read-file (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun audit--parse-flags-block (block)
  "Parse a Flags-style BLOCK string into list of plists.
Each plist has :long :short :type :description.
:type is `boolean' if the flag has no metavar, else the metavar string."
  (let (records
        ;; A flag line begins with leading spaces followed by `-X,'
        ;; or just `--`.
        (lines (split-string block "\n" t)))
    ;; Group continuation lines (description that wraps onto next line)
    ;; under the previous flag.
    (dolist (line lines)
      (cond
       ;; Flag line.
       ((string-match
         (concat "^[ \t]+"
                 "\\(?:-\\([A-Za-z]\\), \\)?"   ; optional short
                 "--\\([A-Za-z0-9][A-Za-z0-9-]*\\)"
                 ;; metavar: lowercase identifier (cobra convention).
                 ;; Must be followed by 2+ spaces (separator before desc).
                 "\\(?:[ \t]+\\([a-z][a-zA-Z0-9_]*\\(?:\\[\\][a-z]+\\)?\\)\\)?"
                 "[ \t][ \t]+\\(.*\\)$")
         line)
        (push (list :long  (match-string 2 line)
                    :short (match-string 1 line)
                    :type  (or (match-string 3 line) "boolean")
                    :description (or (match-string 4 line) ""))
              records))
       ;; Continuation line: indented further than a flag column. Append
       ;; to last record's description.
       ((and records (string-match "^[ \t]\\{6,\\}\\(.*\\)$" line))
        (let ((last (car records)))
          (setf (plist-get last :description)
                (concat (plist-get last :description) " "
                        (string-trim (match-string 1 line))))
          (setcar records last)))))
    (nreverse records)))

(defun audit--split-help (help-text)
  "Split HELP-TEXT into (FLAGS-BLOCK GLOBAL-FLAGS-BLOCK USAGE-LINE HAS-GLOBAL-P).
Either flags block may be nil if absent."
  (let* ((flags-start (string-match "^Flags:\n" help-text))
         (global-start (string-match "^Global Flags:\n" help-text))
         (usage-match (string-match
                       "^Usage:\n[ \t]+\\(.*\\)\n" help-text))
         (usage-line (and usage-match (match-string 1 help-text)))
         flags-block global-block)
    (when flags-start
      (let ((from (+ flags-start (length "Flags:\n")))
            (to (or global-start (length help-text))))
        (setq flags-block (substring help-text from to))))
    (when global-start
      (let ((from (+ global-start (length "Global Flags:\n"))))
        ;; Read until blank line or end.
        (setq global-block
              (substring help-text from
                         (or (string-match "\n[A-Z]" help-text from)
                             (length help-text))))))
    (list flags-block global-block usage-line (not (null global-start)))))

(defun audit--parse-positionals (usage-line)
  "Extract positional arg names from USAGE-LINE.
Returns list of plists with :name and :variadic.
Example: \"bd close [id...] [flags]\" -> ((:name \"id\" :variadic t))"
  (when usage-line
    (let ((pos 0) results)
      (while (string-match
              "\\(?:\\[\\([a-zA-Z][a-zA-Z0-9_-]*\\)\\(\\.\\.\\.\\)?\\]\\|<\\([a-zA-Z][a-zA-Z0-9_-]*\\)\\(\\.\\.\\.\\)?>\\)"
              usage-line pos)
        (let ((name (or (match-string 1 usage-line) (match-string 3 usage-line)))
              (variadic (or (match-string 2 usage-line) (match-string 4 usage-line)))
              (required (match-string 3 usage-line)))
          (when (and name (not (member name '("flags" "command"))))
            (push (list :name name
                        :variadic (and variadic t)
                        :required (and required t))
                  results)))
        (setq pos (match-end 0)))
      (nreverse results))))

;;; ============================================================
;;; Class slot introspection
;;; ============================================================

(defconst audit-global-slots
  '(actor allow-stale db dolt-auto-commit lock-timeout no-auto-flush
          no-auto-import no-daemon no-db profile quiet readonly sandbox
          verbose json no-pager directory global cli-command)
  "Slots inherited from `beads-command-global-options' plus class metadata.
Excluded from per-class flag audits because they're handled separately.")

(defun audit--class-slot-info (class)
  "Return list of plists describing each non-global slot of CLASS."
  (let* ((all-slots (beads-meta-command-slots class))
         (non-global (cl-remove-if (lambda (s) (memq s audit-global-slots))
                                   all-slots))
         results)
    (dolist (slot non-global)
      (let* ((info (beads-meta-slot-info class slot))
             (props (plist-get info :custom-props))
             (long (or (alist-get :long-option props)
                       ;; default: slot name as string
                       (symbol-name slot)))
             (short (alist-get :short-option props))
             (positional (alist-get :positional props))
             (positional-rest (alist-get :positional-rest props))
             (required (alist-get :required props))
             (option-type (alist-get :option-type props))
             (eieio-type (plist-get info :type))
             (doc (or (alist-get :documentation props)
                      ;; Fall back to EIEIO docstring.
                      (audit--slot-doc class slot))))
        (push (list :slot slot
                    :long long
                    :short short
                    :positional positional
                    :positional-rest positional-rest
                    :required required
                    :option-type option-type
                    :type eieio-type
                    :documentation doc)
              results)))
    (nreverse results)))

(defun audit--slot-doc (class slot)
  "Return docstring for SLOT in CLASS via EIEIO descriptor."
  (let* ((cobj (cl--find-class class))
         (slots-vec (and cobj (eieio--class-slots cobj))))
    (when slots-vec
      (cl-loop for sd across slots-vec
               when (eq (cl--slot-descriptor-name sd) slot)
               return (alist-get :documentation
                                 (cl--slot-descriptor-props sd))))))

;;; ============================================================
;;; Type comparison
;;; ============================================================

(defun audit--slot-cli-type (slot)
  "Heuristic: derive CLI type string (\"boolean\" or non-boolean) from SLOT info."
  (let ((opt (plist-get slot :option-type))
        (typ (plist-get slot :type)))
    (cond
     ((eq opt :boolean) "boolean")
     ((eq opt :string) "string")
     ((eq opt :integer) "int")
     ((eq typ 'boolean) "boolean")
     ;; (or null string) etc.
     ((and (consp typ) (eq (car typ) 'or)
           (memq 'boolean typ)) "boolean")
     ((and (consp typ) (eq (car typ) 'or)
           (memq 'integer typ)) "int")
     (t "string"))))

(defun audit--cli-flag-is-boolean (flag)
  (string= (plist-get flag :type) "boolean"))

(defun audit--first-sentence (s)
  "Return the first sentence of S (up to . / newline)."
  (when s
    (let ((s (string-trim s)))
      (cond
       ((string-match "\\([^.\n]+?\\)\\(?:\\.\\|\n\\|$\\)" s)
        (string-trim (match-string 1 s)))
       (t s)))))

(defun audit--norm-desc (s)
  "Normalize a description for fuzzy comparison."
  (when s
    (downcase (replace-regexp-in-string
               "\\s-+" " "
               (string-trim
                (replace-regexp-in-string "\\.\\'" "" (audit--first-sentence s)))))))

;;; ============================================================
;;; Per-class audit
;;; ============================================================

(defun audit--compare (class-rec)
  "Compare a class record to its CLI help. Return list of finding strings."
  (let* ((class    (intern (alist-get 'class class-rec)))
         (cli-path (alist-get 'cli_path class-rec))
         (file     (alist-get 'file class-rec))
         (line     (alist-get 'line class-rec))
         (help-path (format "%s/help/%s.txt" audit-run-dir cli-path))
         findings)
    (cond
     ((not (file-exists-p help-path))
      (push (format "[error] cached help missing: %s" help-path) findings))
     ((not (cl--find-class class))
      (push (format "[error] class %S not loaded" class) findings))
     (t
      (let* ((help (audit--read-file help-path))
             (parts (audit--split-help help))
             (flags-block (nth 0 parts))
             (usage-line  (nth 2 parts))
             (cli-flags (and flags-block (audit--parse-flags-block flags-block)))
             (cli-positionals (audit--parse-positionals usage-line))
             ;; Filter out --help (always present).
             (cli-flags (cl-remove-if
                         (lambda (f) (string= (plist-get f :long) "help"))
                         cli-flags))
             (slots (audit--class-slot-info class))
             ;; Build maps.
             (slot-by-long (cl-loop for s in slots
                                    when (and (plist-get s :long)
                                              (not (plist-get s :positional))
                                              (not (plist-get s :positional-rest)))
                                    collect (cons (plist-get s :long) s)))
             (cli-by-long (cl-loop for f in cli-flags
                                   collect (cons (plist-get f :long) f))))
        ;; Missing slot: in CLI, not in class.
        (dolist (cli cli-flags)
          (let ((long (plist-get cli :long)))
            (unless (assoc long slot-by-long)
              (push (format "[missing slot] `--%s` (%s) — %s"
                            long
                            (plist-get cli :type)
                            (plist-get cli :description))
                    findings))))
        ;; Extra slot: in class, not in CLI.
        (dolist (pair slot-by-long)
          (let* ((long (car pair)) (slot (cdr pair)))
            (unless (assoc long cli-by-long)
              (push (format "[extra slot] `%s` (slot `%s`) — %s"
                            long
                            (plist-get slot :slot)
                            (or (plist-get slot :documentation) ""))
                    findings))))
        ;; Per-flag drift checks.
        (dolist (pair slot-by-long)
          (let* ((long (car pair))
                 (slot (cdr pair))
                 (cli  (cdr (assoc long cli-by-long))))
            (when cli
              ;; Short option mismatch.
              ;;
              ;; In beads.el, `:short-option` doubles as the transient
              ;; menu key (see lisp/beads-meta.el:32). The actual CLI
              ;; invocation only uses --long-option (see
              ;; `beads-meta-build-command-line', lisp/beads-meta.el:1011).
              ;; So we only flag a real mismatch when the CLI itself
              ;; has a short option that disagrees with the slot's.
              ;; Slots that pick a `:short-option` purely as a transient
              ;; key (when CLI has no short) are not flagged.
              (let ((cli-short (plist-get cli :short))
                    (slot-short (plist-get slot :short)))
                (when (and cli-short (not (equal cli-short slot-short)))
                  (push (format
                         "[wrong short] `--%s` — CLI advertises -%s, slot has %s"
                         long
                         cli-short
                         (if slot-short (concat "-" slot-short) "(none)"))
                        findings)))
              ;; Type sense check.
              (let ((cli-type (plist-get cli :type))
                    (slot-type (audit--slot-cli-type slot)))
                (unless (or (string= cli-type slot-type)
                            ;; Accept non-string CLI metavars as "string-ish"
                            (and (not (string= cli-type "boolean"))
                                 (not (string= slot-type "boolean"))))
                  (push (format
                         "[wrong type] `--%s` — CLI %s, slot %s"
                         long cli-type slot-type)
                        findings)))
              ;; Description drift (first-sentence, case-insensitive).
              (let ((cli-desc (audit--norm-desc (plist-get cli :description)))
                    (slot-desc (audit--norm-desc (plist-get slot :documentation))))
                (when (and cli-desc slot-desc
                           (not (string= cli-desc slot-desc))
                           ;; Cheap edit-distance proxy: skip if same prefix.
                           (or (< (length cli-desc) 8)
                               (< (length slot-desc) 8)
                               (not (or (string-prefix-p
                                         (substring cli-desc 0
                                                    (min 30 (length cli-desc)))
                                         slot-desc)
                                        (string-prefix-p
                                         (substring slot-desc 0
                                                    (min 30 (length slot-desc)))
                                         cli-desc)))))
                  (push (format
                         "[drift desc] `--%s`\n      CLI:   %s\n      slot:  %s"
                         long
                         (or (plist-get cli :description) "")
                         (or (plist-get slot :documentation) ""))
                        findings))))))
        ;; Positional checks are intentionally not performed — Usage
        ;; line syntax (`<id>...`, `<key=value>`, `--flag <value>`)
        ;; is too ambiguous to parse reliably. Beads.el slots
        ;; routinely collapse a CLI's required+variadic positional
        ;; pair into a single list-of-string slot, so a count match
        ;; would generate noise. Reviewers should spot-check
        ;; positional handling manually for any flagged class.
        (ignore cli-positionals))))
    (cons (list :class class :cli-path cli-path :file file :line line)
          (nreverse findings))))

;;; ============================================================
;;; Globals inheritance audit
;;; ============================================================

(defun audit--global-options-class-p (class)
  "T if CLASS has `beads-command-global-options' in its precedence list."
  (memq 'beads-command-global-options
        (mapcar (lambda (c)
                  (if (symbolp c) c (eieio--class-name c)))
                (eieio--class-precedence-list (cl--find-class class)))))

(defun audit--inheritance (class-rec)
  "Return inheritance verdict for a class record."
  (let* ((class    (intern (alist-get 'class class-rec)))
         (cli-path (alist-get 'cli_path class-rec))
         (help-path (format "%s/help/%s.txt" audit-run-dir cli-path))
         (has-globals
          (and (file-exists-p help-path)
               (string-match-p "^Global Flags:" (audit--read-file help-path))))
         (inherits (audit--global-options-class-p class)))
    (list :class class
          :cli-path cli-path
          :has-globals has-globals
          :inherits inherits
          :file (alist-get 'file class-rec)
          :line (alist-get 'line class-rec))))

;;; ============================================================
;;; Main
;;; ============================================================

(defun audit-main ()
  (let* ((classes-json (audit--read-file (format "%s/classes.json" audit-run-dir)))
         (records (json-parse-string classes-json :object-type 'alist
                                     :array-type 'list))
         flag-results
         inh-results)
    (dolist (rec records)
      (push (audit--compare rec) flag-results)
      (push (audit--inheritance rec) inh-results))
    (setq flag-results (nreverse flag-results))
    (setq inh-results (nreverse inh-results))
    ;; Write flags.md
    (with-temp-file (format "%s/flags.md" audit-run-dir)
      (insert "# Slot Drift — Per-Class Findings\n\n")
      (insert (format "Source: 216 classes audited against `bd <cmd> --help`.\n"))
      (insert "Global flags handled separately in `inheritance.md`.\n\n")
      (let ((clean 0) (with-issues 0) (issues 0))
        (dolist (res flag-results)
          (let* ((meta (car res))
                 (findings (cdr res))
                 (cls (plist-get meta :class))
                 (path (plist-get meta :cli-path)))
            (cond
             ((null findings)
              (cl-incf clean))
             (t
              (cl-incf with-issues)
              (cl-incf issues (length findings))
              (insert (format "### %S (`%s`)\n" cls path))
              (insert (format "**File:** %s:%s\n\n"
                              (plist-get meta :file) (plist-get meta :line)))
              (dolist (f findings)
                (insert (format "- %s\n" f)))
              (insert "\n")))))
        (insert (format "\n---\n\n## Summary\n- Clean classes: %d\n- Classes with findings: %d\n- Total findings: %d\n"
                        clean with-issues issues))))
    ;; Write inheritance.md
    (with-temp-file (format "%s/inheritance.md" audit-run-dir)
      (insert "# Global Options Inheritance Audit\n\n")
      (insert "A `bd <cmd> --help` showing a `Global Flags:` section ⇒ the class should inherit `beads-command-global-options`.\n\n")
      ;; Diff the global-options class itself against any one help file's
      ;; Global Flags section (they're identical across commands).
      (let* ((sample (audit--read-file (format "%s/help/close.txt" audit-run-dir)))
             (parts (audit--split-help sample))
             (gblock (nth 1 parts))
             (cli-globals (and gblock (audit--parse-flags-block gblock)))
             (cli-globals (cl-remove-if
                           (lambda (f) (string= (plist-get f :long) "help"))
                           cli-globals))
             (cobj (cl--find-class 'beads-command-global-options))
             (slots-vec (and cobj (eieio--class-slots cobj)))
             slot-longs)
        (when slots-vec
          (cl-loop for sd across slots-vec
                   do (let* ((props (cl--slot-descriptor-props sd))
                             (l (alist-get :long-option props)))
                        (when l (push l slot-longs)))))
        (let ((cli-longs (mapcar (lambda (f) (plist-get f :long)) cli-globals)))
          (insert "## `beads-command-global-options` slot drift\n\n")
          (insert "The class itself defines the slots inherited by every command. Comparing against `bd close --help` Global Flags (representative sample):\n\n")
          (insert "**Missing slots (CLI advertises, class lacks):**\n\n")
          (let ((missing (cl-set-difference cli-longs slot-longs :test #'string=)))
            (if (null missing) (insert "_None._\n\n")
              (dolist (m missing)
                (let ((flag (cl-find m cli-globals :key (lambda (f) (plist-get f :long)) :test #'string=)))
                  (insert (format "- `--%s` (%s) — %s\n"
                                  m (plist-get flag :type)
                                  (plist-get flag :description)))))
              (insert "\n")))
          (insert "**Extra slots (class has, CLI no longer advertises):**\n\n")
          (let ((extra (cl-set-difference slot-longs cli-longs :test #'string=)))
            (if (null extra) (insert "_None._\n\n")
              (dolist (e extra)
                (insert (format "- `--%s` (slot present, not in current bd Global Flags)\n" e)))
              (insert "\n")))))
      (let (should-inherit-but-doesnt
            inherits-but-shouldnt)
        (dolist (r inh-results)
          (cond
           ((and (plist-get r :has-globals) (not (plist-get r :inherits)))
            (push r should-inherit-but-doesnt))
           ((and (not (plist-get r :has-globals)) (plist-get r :inherits))
            (push r inherits-but-shouldnt))))
        (insert "## Should inherit, doesn't\n\n")
        (if (null should-inherit-but-doesnt)
            (insert "_None._\n\n")
          (dolist (r should-inherit-but-doesnt)
            (insert (format "- `%S` (`%s`) — %s:%s\n"
                            (plist-get r :class)
                            (plist-get r :cli-path)
                            (plist-get r :file)
                            (plist-get r :line)))))
        (insert "\n## Inherits but shouldn't\n\n")
        (if (null inherits-but-shouldnt)
            (insert "_None._\n\n")
          (dolist (r inherits-but-shouldnt)
            (insert (format "- `%S` (`%s`) — %s:%s\n"
                            (plist-get r :class)
                            (plist-get r :cli-path)
                            (plist-get r :file)
                            (plist-get r :line)))))
        (insert (format "\n---\n\n## Counts\n- Total classes: %d\n- Should inherit but doesn't: %d\n- Inherits but shouldn't: %d\n"
                        (length inh-results)
                        (length should-inherit-but-doesnt)
                        (length inherits-but-shouldnt)))))
    (princ (format "DONE. flag-results=%d inh-results=%d\n"
                   (length flag-results) (length inh-results)))))

(audit-main)

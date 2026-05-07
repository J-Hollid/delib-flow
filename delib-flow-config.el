;;; delib-flow-config.el --- Configuration for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; User-facing customization and package defaults for delib-flow.

;;; Code:

(defgroup delib-flow nil
  "Guided AI workflow control for Org."
  :group 'tools
  :prefix "delib-flow-")

(defcustom delib-flow-my-projects-file nil
  "Path to the authoritative My Projects Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-zk-root nil
  "Path to the searchable zettelkasten root."
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom delib-flow-prompt-library-file nil
  "Path to the prompt library Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-example-structures-file nil
  "Path to the example structures Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-audit-log-file nil
  "Path to the audit log Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-audit-archive-directory nil
  "Directory where selected active-run audit snapshots are archived.

Each saved archive is written as a standalone one-run Org file.  This archive
is separate from `delib-flow-audit-log-file', which remains the rolling live
audit log when configured."
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom delib-flow-inbox-file nil
  "Path to the inbox Org file used by `delib-flow-start-from-inbox'."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-inbox-outline-path nil
  "Outline path inside `delib-flow-inbox-file' whose child entries form the inbox queue.

When nil, `delib-flow-start-from-inbox' offers top-level headings from the file.
When non-nil, it must be a list of heading titles such as '(\"Inbox\") or
'(\"Capture\" \"Inbox\"), and the picker will offer the direct child headings
under that node."
  :type '(choice (const :tag "Top-level headings" nil)
                 (repeat string)))

(defcustom delib-flow-audit-payload-policy 'full
  "Policy for retaining detailed stage payloads in the audit log.

`full' persists input packages and raw outputs as-is.
`redacted' persists payloads after deterministic string redaction.
`metadata-only' omits payload bodies and keeps only stage metadata."
  :type '(choice (const :tag "Full payloads" full)
                 (const :tag "Redacted payloads" redacted)
                 (const :tag "Metadata only" metadata-only)))

(defcustom delib-flow-audit-redaction-profile 'strict
  "Deterministic redaction profile used when audit payload policy is `redacted'."
  :type '(choice (const :tag "Standard" standard)
                 (const :tag "Strict" strict)))

(defcustom delib-flow-general-note-template
  "#+title: ${title}\n#+filetags: :delib-flow:reference:\n\n- Filed from delib-flow\n- Source artifact: ${source-artifact}\n"
  "Template used for deterministic general PKM note creation.

Supported placeholders are `${title}', `${source-artifact}', and `${note-type}'."
  :type 'string)

(defcustom delib-flow-project-support-note-template
  "#+title: ${title}\n#+filetags: :project:support:\n\n- Filed from delib-flow\n- Source artifact: ${source-artifact}\n"
  "Template used for deterministic project support note creation.

Supported placeholders are `${title}', `${source-artifact}', and `${note-type}'."
  :type 'string)

(defcustom delib-flow-next-action-capture-template
  "%(delib-flow-capture-project-child-heading)\n%(delib-flow-capture-project-child-properties)"
  "Org capture pattern used when filing next-action items into a matched project.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-waiting-for-capture-template
  "%(delib-flow-capture-project-child-heading)\n%(delib-flow-capture-project-child-properties)"
  "Org capture pattern used when filing waiting-for items into a matched project.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-project-capture-template
  "%(delib-flow-capture-project-heading)\n%(delib-flow-capture-project-tags-property)%(delib-flow-capture-project-first-item-entry)\n"
  "Org capture pattern used when filing new project proposals.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-general-note-capture-template
  "#+title: %(delib-flow-capture-note-title)\n#+filetags: :delib-flow:reference:\n\n- Filed from delib-flow\n- Source artifact: %(delib-flow-capture-source-artifact)\n"
  "Org capture pattern used when filing general PKM notes.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-project-support-note-capture-template
  "#+title: %(delib-flow-capture-note-title)\n#+filetags: :project:support:\n\n- Filed from delib-flow\n- Source artifact: %(delib-flow-capture-source-artifact)\n"
  "Org capture pattern used when filing project-support notes.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-general-note-org-roam-capture-key nil
  "Org-roam capture template key used when filing general PKM notes.

When non-nil, reference-note filing resolves the target path and staged
content from the matching `org-roam-capture-templates' entry instead of the
package-local deterministic note template."
  :type '(choice (const :tag "Disabled" nil) string))

(defcustom delib-flow-project-support-note-org-roam-capture-key nil
  "Org-roam capture template key used when filing project-support notes.

When non-nil, reference-note filing resolves the target path and staged
content from the matching `org-roam-capture-templates' entry instead of the
package-local deterministic note template."
  :type '(choice (const :tag "Disabled" nil) string))

(defvar org-roam-directory nil
  "Org-roam root directory.

Declared here so delib-flow can safely interact with org-roam in batch
contexts before org-roam itself has been loaded.")

(defcustom delib-flow-default-local-model nil
  "Default local model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-default-cloud-model nil
  "Default cloud model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-cloud-reroutable-stage-ids
  '(run-cloud-stage filter-reference-material extract-actions
                    extract-waiting-for suggest-reference-notes)
  "Stage identifiers that may be executed through the reviewed cloud path.

Entries must be stage ids present in `delib-flow--stage-descriptor-alist'."
  :type '(repeat symbol))

(defcustom delib-flow-cloud-policy-profile 'standard
  "Default cloud sanitization policy profile.

Supported profiles are `standard' and `strict'."
  :type '(choice (const :tag "Standard" standard)
                 (const :tag "Strict" strict)))

(defcustom delib-flow-cloud-provider-policy-alist
  '((default :enabled t :policy-profile standard))
  "Provider-specific cloud policy configuration.

Each entry is keyed by a provider name string or the symbol `default' and may
include `:enabled' and `:policy-profile' properties."
  :type 'sexp)

(defcustom delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter
  "Function used to execute local stages.

The function receives a stage descriptor and an assembled input package,
and returns raw stage output."
  :type 'function)

(defcustom delib-flow-local-stage-async-adapter nil
  "Optional function used to execute local stages asynchronously.

The function receives a stage descriptor, an assembled input package, an
ON-SUCCESS callback, and an ON-ERROR callback. ON-SUCCESS receives the
raw stage output. ON-ERROR receives a user-facing error string. The
adapter may return an opaque handle such as a process object."
  :type '(choice (const :tag "Disabled" nil) function))

(defcustom delib-flow-cloud-stage-adapter #'delib-flow--default-cloud-stage-adapter
  "Function used to execute cloud stages.

The function receives a stage descriptor and an assembled input package,
and returns raw stage output."
  :type 'function)

(defconst delib-flow-control-buffer-name "*delib-flow*"
  "Name of the main delib-flow control buffer.")

(defconst delib-flow-filing-workspace-buffer-name "*delib-flow filing*"
  "Name of the focused delib-flow filing workspace buffer.")

(defconst delib-flow-source-view-buffer-name "*delib-flow source*"
  "Name of the cleaned delib-flow source view buffer.")

(defconst delib-flow-note-part-editor-buffer-name "*delib-flow note part*"
  "Name of the focused note-part editor buffer.")

(defconst delib-flow-field-editor-buffer-name "*delib-flow field*"
  "Name of the dedicated operator field editor buffer.")

(defconst delib-flow--meeting-source-keywords
  '("meeting" "standup" "sync" "retro" "planning" "check-in" "kickoff"
    "agenda" "minutes" "attendees" "1:1")
  "Keywords used to classify meeting-note source items.")

(defconst delib-flow--meeting-source-section-labels
  '("agenda:" "attendees:" "notes:" "decisions:" "action items:" "next steps:")
  "Structured section labels used to classify meeting-note source items.")

(defconst delib-flow--reminder-source-keywords
  '("reminder" "remember to" "don't forget" "dont forget" "follow up"
    "follow-up" "ping" "check on")
  "Keywords used to classify reminder source items.")

(defconst delib-flow--issue-note-source-keywords
  '("broken" "fix" "repair" "bug" "issue" "website" "page" "exercise"
    "exercises" "drill" "drills" "link" "links" "not working" "wrong"
    "problem" "failing" "failure")
  "Keywords used to classify issue-note source items.")

(defconst delib-flow--fleeting-note-source-keywords
  '("idea" "thought" "brainstorm" "note to self" "question" "questions"
    "wonder if" "maybe" "explore" "possible")
  "Keywords used to classify fleeting-note source items.")

(defconst delib-flow--project-proposal-title-prefix-pattern
  (concat
   "\\`\\(?:"
   "presentation or article on"
   "\\|article on"
   "\\|presentation on"
   "\\|idea[: -]+"
   "\\|thought[: -]+"
   "\\|note to self[: -]+"
   "\\|reminder[: -]+"
   "\\|question[: -]+"
   "\\)\\s-*")
  "Prefix pattern stripped from proposed project titles.")

(defconst delib-flow--project-proposal-tag-stopwords
  '("a" "an" "and" "are" "article" "for" "from" "idea" "in" "into" "my"
    "note" "of" "on" "or" "presentation" "project" "reminder" "the"
    "thought" "to" "up" "with")
  "Words that are too generic to use as proposed project tags.")

(defconst delib-flow--tag-suggestion-stopwords
  '("a" "an" "and" "article" "as" "at" "by" "for" "from" "idea" "in" "into"
    "it" "my" "note" "of" "on" "or" "presentation" "reminder" "the" "to"
    "up" "with" "write" "your" "email")
  "Words ignored when deriving deterministic tag suggestions.")

(defconst delib-flow--entity-noise-terms
  '("it" "join" "give" "want" "week" "hello" "thanks" "regards"
    "newsletter" "cohort" "update")
  "Low-value entity terms filtered from inspect entities and tag suggestions.")

(defconst delib-flow--spinner-frames ["|" "/" "-" "\\"]
  "Frames used for simple in-flight cockpit spinners.")

(defconst delib-flow--control-sections
  '("Now"
    "Current result"
    "Filing preview"
    "Next actions"
    "Current context"
    "Details")
  "Top-level sections rendered in the control buffer.")

(defconst delib-flow--filing-workspace-sections
  '("Selected artifact"
    "Working draft"
    "Support for this draft"
    "Next actions"
    "Final save target"
    "Other candidates"
    "Back / more actions")
  "Top-level sections rendered in the focused filing workspace.")

(defconst delib-flow--project-filing-workspace-sections
  '("Project package"
    "Do here now"
    "Package consequence"
    "Included items"
    "Extracted but not yet included"
    "Targets and staged output"
    "Leave workspace")
  "Top-level sections rendered in the focused project filing workspace.")

(defconst delib-flow--initial-section-anchor-alist
  '((now . "delib-section-now")
    (next-actions . "delib-section-next-actions")
    (current-result . "delib-section-current-result")
    (current-context . "delib-section-current-context")
    (filing-preview . "delib-section-filing-preview")
    (details . "delib-section-details"))
  "Stable anchor identifiers for top-level control-buffer sections.")

(defconst delib-flow--control-key-help-alist
  '(("n / p" . "Move to the next or previous top-level cockpit section.")
    ("TAB / S-TAB" . "Move to the next or previous rendered action.")
    ("1-9/0/letters" . "Run the action with the matching on-screen shortcut.")
    ("RET/a" . "Run the action at point.")
    ("." . "Open the local context menu for the current section.")
    ("F" . "Open the focused filing workspace for the active filing loop.")
    ("B" . "Return to the main cockpit from a focused workspace.")
    ("A" . "Accept the current inspect or project-match review.")
    ("E" . "Show the selected draft in the consequence pane.")
    ("I" . "Show focused support for the selected artifact in the consequence pane.")
    ("P" . "Show the planned filing target in the consequence pane.")
    ("V" . "Show the exact staged filing content in the consequence pane.")
    ("L" . "Jump back to the active decision loop.")
    ("K" . "Jump to the latest consequence preview or result.")
    ("U" . "Jump to the latest stage history details.")
    ("m" . "Open completion for manual project selection.")
    ("s" . "Open completion for filing artifact selection.")
    ("z" . "Toggle narrow-screen focus mode for the active decision loop.")
    ("r" . "Retry the current inspect or project-match stage.")
    ("g" . "Refresh the control buffer.")
    ("j" . "Open the audit log at the active run.")
    ("J" . "Open the audit log at the latest stage.")
    ("q" . "Abort the active run.")
    ("?" . "Show the full control-key help.")
    ("D" . "Open the latest stage debug inspection.")
    ("C" . "Open the debug comparison view.")
    ("W" . "Open the debug walkthrough.")
    ("H" . "Apply the debug helper for the active review block.")
    ("N" . "Advance the debug walkthrough to its next checkpoint.")
    ("R" . "Restart the debug walkthrough from baseline."))
  "Control-buffer keys and their user-facing descriptions.")

(defconst delib-flow--filing-workspace-key-help-alist
  '(("n / p" . "Move to the next or previous filing workspace section.")
    ("TAB / S-TAB" . "Move to the next or previous filing-local action.")
    ("RET/a" . "Run the filing-local action at point.")
    ("." . "Open the filing-workspace local menu for this section.")
    ("O" . "Open the cleaned source that the current extraction and note draft should stay grounded on.")
    ("E" . "Show the selected draft in the consequence pane.")
    ("I" . "Show attached support in the consequence pane.")
    ("P" . "Show the planned filing target in the consequence pane.")
    ("V" . "Show the exact staged filing content in the consequence pane.")
    ("F" . "Reopen the focused filing workspace.")
    ("B / q" . "Return to the main cockpit.")
    ("g" . "Refresh the filing workspace.")
    ("?" . "Show filing-workspace help."))
  "Filing-workspace keys and their user-facing descriptions.")

(provide 'delib-flow-config)

;;; delib-flow-config.el ends here

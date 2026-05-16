;;; refbox-org-slipbox.el --- org-slipbox notes for refbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Maintainer: Ayan Das <bvits@riseup.net>
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (refbox "0.4.5") (org-slipbox "0.10.0"))
;; Keywords: outlines, bib, convenience
;; URL: https://github.com/b-vitamins/refbox-org-slipbox

;; This file is not part of GNU Emacs.

;; refbox-org-slipbox is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; refbox-org-slipbox is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with refbox-org-slipbox.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Optional org-slipbox note-source integration for refbox.
;;
;; The package maps refbox bibliography keys to org-slipbox refs, opens and
;; creates slipbox notes through org-slipbox's indexed RPC/capture surfaces, and
;; registers itself as a refbox note source through `refbox-org-slipbox-mode'.
;; It does not scan Org buffers from completion paths.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'refbox)
(require 'org-slipbox)

(defgroup refbox-org-slipbox nil
  "Org-slipbox note-source integration for refbox."
  :group 'refbox
  :group 'org-slipbox
  :prefix "refbox-org-slipbox-")

(defcustom refbox-org-slipbox-reference-function
  #'refbox-org-slipbox-default-reference
  "Function mapping a refbox KEY and REFERENCE to an org-slipbox ref.
The function receives the bibliography key string and the refbox candidate
plist.  The default stores citation references as @KEY."
  :type 'function
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-note-title-template
  "${author editor}, ${title}"
  "Refbox template used for new org-slipbox note titles."
  :type 'string
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-capture-fields
  '((:refbox-title . ("title"))
    (:refbox-author . ("author" "editor"))
    (:refbox-date . ("date" "year" "issued"))
    (:refbox-pages . ("pages"))
    (:refbox-type . ("=type=")))
  "Bibliography fields passed to org-slipbox capture templates.
Each item maps a capture variable keyword to the ordered bibliography
fields used to populate it."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-capture-templates nil
  "Capture templates passed to `org-slipbox-capture-ref'.
When nil, org-slipbox uses `org-slipbox-capture-templates'."
  :type '(choice (const :tag "Use org-slipbox defaults" nil)
                 sexp)
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-capture-template-keys nil
  "Capture template keys passed to `org-slipbox-capture-ref'."
  :type '(choice (const :tag "Prompt normally" nil)
                 (repeat string))
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-set-active-source t
  "When non-nil, enabling `refbox-org-slipbox-mode' selects this note source."
  :type 'boolean
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-all-items-limit 200
  "Maximum org-slipbox ref records returned for global note selection.
Exact per-reference note lookup is indexed and cache-backed.  This limit only
applies to `refbox-open-note' when it is asked to list notes without a
reference."
  :type 'integer
  :group 'refbox-org-slipbox)

(defcustom refbox-org-slipbox-preload-limit 10000
  "Maximum org-slipbox ref records fetched to preload Refbox indicators.

The preload path is used by Refbox completion to avoid one exact ref lookup per
visible bibliography candidate.  If the org-slipbox ref set exceeds this limit,
uncached refs still fall back to exact indexed lookup."
  :type 'integer
  :group 'refbox-org-slipbox)

(defconst refbox-org-slipbox-source-name 'org-slipbox
  "Refbox note source name used by refbox-org-slipbox.")

(defconst refbox-org-slipbox-notes-config
  '(:name "Org Slipbox Notes"
    :items refbox-org-slipbox-note-items
    :all-items refbox-org-slipbox-all-items
    :hasitems refbox-org-slipbox-has-notes
    :preload refbox-org-slipbox-preload
    :open refbox-org-slipbox-open-note
    :create refbox-org-slipbox-create-note
    :create-label refbox-org-slipbox-create-label
    :transform refbox-org-slipbox-display)
  "Refbox note source configuration for org-slipbox.")

(defconst refbox-org-slipbox--cache-miss
  (make-symbol "refbox-org-slipbox-cache-miss")
  "Sentinel used for note cache misses.")

(defvar refbox-org-slipbox--note-cache nil
  "Hash table mapping org-slipbox refs to exact ref records.")

(defvar refbox-org-slipbox--note-cache-stamp nil
  "Database stamp corresponding to `refbox-org-slipbox--note-cache'.")

(defvar refbox-org-slipbox--preload-stamp nil
  "Cache stamp for the latest broad org-slipbox ref preload.")

(defvar refbox-org-slipbox--previous-notes-source nil
  "Refbox note source active before `refbox-org-slipbox-mode' changed it.")

(defun refbox-org-slipbox--listify (value)
  "Return JSON-derived VALUE as a list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun refbox-org-slipbox-default-reference (key _reference)
  "Return the default org-slipbox ref for bibliography KEY."
  (concat "@" (string-remove-prefix "@" (substring-no-properties key))))

(defun refbox-org-slipbox-reference (key reference)
  "Return org-slipbox ref for KEY and REFERENCE."
  (let ((value (funcall refbox-org-slipbox-reference-function key reference)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (user-error "Refbox-org-slipbox reference function returned no ref"))
    (substring-no-properties value)))

(defun refbox-org-slipbox--cache-stamp ()
  "Return a cache stamp for the active org-slipbox database."
  (let ((database (and (boundp 'org-slipbox-database-file)
                       (expand-file-name org-slipbox-database-file))))
    (list database
          (and database
               (file-exists-p database)
               (file-attribute-modification-time
                (file-attributes database 'integer)))
          (and (boundp 'org-slipbox-directory)
               org-slipbox-directory))))

(defun refbox-org-slipbox-clear-cache ()
  "Clear cached org-slipbox note lookup data."
  (interactive)
  (setq refbox-org-slipbox--note-cache (make-hash-table :test 'equal)
        refbox-org-slipbox--note-cache-stamp
        (refbox-org-slipbox--cache-stamp)
        refbox-org-slipbox--preload-stamp nil))

(defun refbox-org-slipbox--cache ()
  "Return the current note lookup cache, resetting it if the DB changed."
  (let ((stamp (refbox-org-slipbox--cache-stamp)))
    (unless (and refbox-org-slipbox--note-cache
                 (equal stamp refbox-org-slipbox--note-cache-stamp))
      (setq refbox-org-slipbox--note-cache (make-hash-table :test 'equal)
            refbox-org-slipbox--note-cache-stamp stamp
            refbox-org-slipbox--preload-stamp nil))
    refbox-org-slipbox--note-cache))

(defun refbox-org-slipbox--record-reference (record)
  "Return the org-slipbox ref string from RECORD."
  (plist-get record :reference))

(defun refbox-org-slipbox--record-node (record)
  "Return the org-slipbox node plist from RECORD or RECORD itself."
  (or (plist-get record :node)
      (and (plist-member record :node_key) record)
      (user-error "Invalid org-slipbox note record: %S" record)))

(defun refbox-org-slipbox--records-for-ref (org-slipbox-ref)
  "Return exact org-slipbox ref records for ORG-SLIPBOX-REF."
  (let ((cache (refbox-org-slipbox--cache)))
    (let ((cached (gethash org-slipbox-ref cache refbox-org-slipbox--cache-miss)))
      (if (not (eq cached refbox-org-slipbox--cache-miss))
          cached
        (let* ((response (org-slipbox-rpc-search-refs org-slipbox-ref 200))
               (records
                (seq-filter
                 (lambda (record)
                   (equal (refbox-org-slipbox--record-reference record)
                          org-slipbox-ref))
                 (refbox-org-slipbox--listify
                  (plist-get response :refs)))))
          (puthash org-slipbox-ref records cache))))))

(defun refbox-org-slipbox--cache-records (records)
  "Merge org-slipbox ref RECORDS into the exact-ref cache."
  (let ((cache (refbox-org-slipbox--cache)))
    (dolist (record records)
      (when-let ((reference (refbox-org-slipbox--record-reference record)))
        (puthash reference
                 (cons record
                       (remove record (gethash reference cache)))
                 cache)))))

(defun refbox-org-slipbox-preload (references)
  "Preload note lookup data for Refbox REFERENCES."
  (let* ((refs (delete-dups
                (delq nil
                      (mapcar
                       (lambda (reference)
                         (when-let ((key (refbox-reference-field reference "key")))
                           (refbox-org-slipbox-reference key reference)))
                       references))))
         (cache (refbox-org-slipbox--cache))
         (missing (cl-remove-if
                   (lambda (reference)
                     (not (eq (gethash reference cache refbox-org-slipbox--cache-miss)
                              refbox-org-slipbox--cache-miss)))
                   refs))
         (stamp (list (refbox-org-slipbox--cache-stamp)
                      refbox-org-slipbox-preload-limit)))
    (when (and missing
               (not (equal stamp refbox-org-slipbox--preload-stamp)))
      (let* ((response (org-slipbox-rpc-search-refs
                        ""
                        refbox-org-slipbox-preload-limit))
             (records (refbox-org-slipbox--listify
                       (plist-get response :refs))))
        (refbox-org-slipbox--cache-records records)
        (when (< (length records) refbox-org-slipbox-preload-limit)
          (dolist (reference refs)
            (unless (gethash reference cache)
              (puthash reference nil cache))))
        (setq refbox-org-slipbox--preload-stamp stamp)))))

(defun refbox-org-slipbox--candidate-for-key (key reference)
  "Return a refbox candidate for KEY and REFERENCE when available."
  (cond
   ((and (listp reference) (plist-member reference :key))
    reference)
   ((stringp key)
    (ignore-errors (refbox-entry-by-key key)))
   (t nil)))

(defun refbox-org-slipbox-note-items (key reference)
  "Return org-slipbox note records for bibliography KEY and REFERENCE."
  (refbox-org-slipbox--records-for-ref
   (refbox-org-slipbox-reference key reference)))

(defun refbox-org-slipbox-has-notes (key reference)
  "Return non-nil when KEY and REFERENCE have org-slipbox notes."
  (not (null (refbox-org-slipbox-note-items key reference))))

(defun refbox-org-slipbox-all-items ()
  "Return a bounded list of org-slipbox ref records."
  (let ((records
         (refbox-org-slipbox--listify
          (plist-get
           (org-slipbox-rpc-search-refs
            ""
            refbox-org-slipbox-all-items-limit)
           :refs))))
    (dolist (record records)
      (let ((reference (refbox-org-slipbox--record-reference record)))
        (when reference
          (puthash reference
                   (cons record
                         (remove record
                                 (gethash reference
                                          (refbox-org-slipbox--cache))))
                   (refbox-org-slipbox--cache)))))
    records))

(defun refbox-org-slipbox-display (item)
  "Return display text for org-slipbox note ITEM."
  (let* ((reference (or (refbox-org-slipbox--record-reference item) ""))
         (node (refbox-org-slipbox--record-node item))
         (title (or (plist-get node :title) ""))
         (file (or (plist-get node :file_path) "")))
    (string-join
     (seq-filter
      (lambda (part) (not (string-empty-p part)))
      (list title reference file))
     "  ")))

(defun refbox-org-slipbox-open-note (item)
  "Visit org-slipbox note ITEM."
  (org-slipbox-node-visit (refbox-org-slipbox--record-node item)))

(defun refbox-org-slipbox--note-title (key reference)
  "Return new org-slipbox note title for KEY and REFERENCE."
  (let* ((candidate (refbox-org-slipbox--candidate-for-key key reference))
         (title (and candidate
                     (refbox-template-format
                      refbox-org-slipbox-note-title-template
                      candidate))))
    (if (or (null title) (string-empty-p (string-trim title)))
        key
      (string-trim title))))

(defun refbox-org-slipbox--capture-variables (key reference title slipbox-ref)
  "Return capture variables for KEY, REFERENCE, TITLE, and SLIPBOX-REF."
  (let ((candidate (refbox-org-slipbox--candidate-for-key key reference))
        variables)
    (when candidate
      (dolist (entry refbox-org-slipbox-capture-fields)
        (when-let* ((name (car-safe entry))
                    (fields (cdr-safe entry))
                    (field (refbox-get-field-with-value fields candidate))
                    (value (cdr field)))
          (setq variables (plist-put variables name value)))))
    (setq variables
          (plist-put variables :refbox-key key))
    (setq variables
          (plist-put variables :refbox-citekey key))
    (setq variables
          (plist-put variables :refbox-reference slipbox-ref))
    (setq variables
          (plist-put variables :note-title title))
    variables))

(defun refbox-org-slipbox-create-label (key reference)
  "Return create label for bibliography KEY and REFERENCE."
  (refbox-org-slipbox--note-title key reference))

(defun refbox-org-slipbox-create-note (key reference)
  "Create or visit an org-slipbox note for bibliography KEY and REFERENCE."
  (let* ((slipbox-ref (refbox-org-slipbox-reference key reference))
         (title (refbox-org-slipbox--note-title key reference))
         (variables
          (refbox-org-slipbox--capture-variables
           key reference title slipbox-ref))
         (node
          (org-slipbox-capture-ref
           slipbox-ref
           title
           refbox-org-slipbox-capture-templates
           refbox-org-slipbox-capture-template-keys
           variables)))
    (refbox-org-slipbox-clear-cache)
    (list :reference slipbox-ref :node node)))

(defun refbox-org-slipbox--key-from-ref (reference)
  "Return a refbox key parsed from org-slipbox REFERENCE."
  (when (and (stringp reference)
             (string-prefix-p "@" reference)
             (> (length reference) 1))
    (substring reference 1)))

;;;###autoload
(defun refbox-org-slipbox-ref-add (&optional reference)
  "Add a selected Refbox REFERENCE as an org-slipbox ref at point."
  (interactive)
  (let* ((reference (or reference (refbox-read-reference)))
         (key (refbox-reference-field reference "key"))
         (slipbox-ref (refbox-org-slipbox-reference key reference)))
    (org-slipbox-ref-add slipbox-ref)
    (refbox-org-slipbox-clear-cache)
    slipbox-ref))

;;;###autoload
(defun refbox-org-slipbox-open-current-refs (&optional prompt)
  "Open Refbox resources for refs attached to the current org-slipbox node.
With PROMPT, select one or more current refs before opening."
  (interactive "P")
  (let* ((node (org-slipbox-node-at-point t))
         (refs (refbox-org-slipbox--listify (plist-get node :refs)))
         (keys (delq nil (mapcar #'refbox-org-slipbox--key-from-ref refs))))
    (unless keys
      (user-error "No Refbox refs for this org-slipbox node"))
    (refbox-open
     (if prompt
         (refbox-select-refs
          :filter (lambda (key) (member key keys)))
       keys))))

(defun refbox-org-slipbox-setup ()
  "Register and optionally select the org-slipbox Refbox note source."
  (refbox-register-notes-source
   refbox-org-slipbox-source-name
   refbox-org-slipbox-notes-config)
  (unless (eq refbox-notes-source refbox-org-slipbox-source-name)
    (setq refbox-org-slipbox--previous-notes-source refbox-notes-source))
  (when refbox-org-slipbox-set-active-source
    (setq refbox-notes-source refbox-org-slipbox-source-name))
  (refbox-org-slipbox-clear-cache))

(defun refbox-org-slipbox-reset ()
  "Unregister the org-slipbox Refbox note source."
  (when (and refbox-org-slipbox-set-active-source
             (eq refbox-notes-source refbox-org-slipbox-source-name))
    (setq refbox-notes-source
          (or refbox-org-slipbox--previous-notes-source 'file)))
  (setq refbox-org-slipbox--previous-notes-source nil)
  (refbox-remove-notes-source refbox-org-slipbox-source-name)
  (refbox-org-slipbox-clear-cache))

;;;###autoload
(define-minor-mode refbox-org-slipbox-mode
  "Toggle org-slipbox note-source integration for Refbox."
  :global t
  :group 'refbox-org-slipbox
  :lighter " refbox-org-slipbox"
  (if refbox-org-slipbox-mode
      (refbox-org-slipbox-setup)
    (refbox-org-slipbox-reset)))

(provide 'refbox-org-slipbox)

;;; refbox-org-slipbox.el ends here

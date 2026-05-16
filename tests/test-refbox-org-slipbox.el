;;; test-refbox-org-slipbox.el --- Tests for refbox-org-slipbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ayan Das

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for refbox-org-slipbox.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (package-root (expand-file-name ".." test-dir))
       (refbox-root
        (or (getenv "REFBOX_DIR")
            (expand-file-name "../../refbox" package-root)))
       (org-slipbox-root
        (or (getenv "ORG_SLIPBOX_DIR")
            (expand-file-name "../../org-slipbox" package-root))))
  (add-to-list 'load-path package-root)
  (add-to-list 'load-path refbox-root)
  (add-to-list 'load-path org-slipbox-root))

(require 'refbox-org-slipbox)

(ert-deftest refbox-org-slipbox-test-default_reference_uses_at_key ()
  "Default reference mapping should store citation keys as @KEY refs."
  (should
   (equal (refbox-org-slipbox-default-reference "smith2024" nil)
          "@smith2024"))
  (should
   (equal (refbox-org-slipbox-default-reference "@smith2024" nil)
          "@smith2024")))

(ert-deftest refbox-org-slipbox-test-note_items_are_exact_and_cached ()
  "Note lookup should filter exact refs and cache indexed RPC results."
  (let ((calls 0)
        (org-slipbox-database-file
         (make-temp-file "refbox-org-slipbox-db"))
        (org-slipbox-directory "/tmp/slipbox"))
    (unwind-protect
        (progn
          (refbox-org-slipbox-clear-cache)
          (cl-letf (((symbol-function 'org-slipbox-rpc-search-refs)
                     (lambda (query limit)
                       (setq calls (1+ calls))
                       (should (equal query "@smith2024"))
                       (should (= limit 200))
                       '(:refs [(:reference "@smith2024"
                                  :node (:node_key "file:smith.org"
                                         :title "Smith"
                                         :file_path "smith.org"
                                         :line 1))
                                 (:reference "@smith2024-extra"
                                  :node (:node_key "file:extra.org"
                                         :title "Extra"
                                         :file_path "extra.org"
                                         :line 1))]))))
            (let ((first (refbox-org-slipbox-note-items "smith2024" nil))
                  (second (refbox-org-slipbox-note-items "smith2024" nil)))
              (should (= calls 1))
              (should (equal first second))
              (should (= (length first) 1))
              (should
               (equal (plist-get (refbox-org-slipbox--record-node (car first))
                                 :node_key)
                      "file:smith.org"))
              (should (refbox-org-slipbox-has-notes "smith2024" nil)))))
      (delete-file org-slipbox-database-file))))

(ert-deftest refbox-org-slipbox-test-cache_invalidates_when_database_changes ()
  "Cache should reset when the org-slipbox database mtime changes."
  (let ((calls 0)
        (org-slipbox-database-file
         (make-temp-file "refbox-org-slipbox-db"))
        (org-slipbox-directory "/tmp/slipbox"))
    (unwind-protect
        (progn
          (refbox-org-slipbox-clear-cache)
          (cl-letf (((symbol-function 'org-slipbox-rpc-search-refs)
                     (lambda (_query _limit)
                       (setq calls (1+ calls))
                       '(:refs []))))
            (refbox-org-slipbox-note-items "one" nil)
            (set-file-times org-slipbox-database-file
                            (time-add (current-time) 1))
            (refbox-org-slipbox-note-items "one" nil)
            (should (= calls 2))))
      (delete-file org-slipbox-database-file))))

(ert-deftest refbox-org-slipbox-test-preload_batches_indicator_lookup ()
  "Preload should avoid one exact org-slipbox RPC per visible candidate."
  (let ((calls nil)
        (refbox-org-slipbox-preload-limit 10)
        (org-slipbox-database-file
         (make-temp-file "refbox-org-slipbox-db"))
        (org-slipbox-directory "/tmp/slipbox"))
    (unwind-protect
        (progn
          (refbox-org-slipbox-clear-cache)
          (cl-letf (((symbol-function 'org-slipbox-rpc-search-refs)
                     (lambda (query limit)
                       (push (list query limit) calls)
                       (should (equal query ""))
                       (should (= limit 10))
                       '(:refs [(:reference "@smith2024"
                                  :node (:node_key "file:smith.org"
                                         :title "Smith"
                                         :file_path "smith.org"
                                         :line 1))]))))
            (refbox-org-slipbox-preload
             '((:key "smith2024")
               (:key "doe2025")))
            (should (refbox-org-slipbox-has-notes "smith2024" '(:key "smith2024")))
            (should-not (refbox-org-slipbox-has-notes "doe2025" '(:key "doe2025")))
            (should (= (length calls) 1))))
      (delete-file org-slipbox-database-file))))

(ert-deftest refbox-org-slipbox-test-open_note_visits_indexed_node ()
  "Opening a note item should visit the org-slipbox node payload."
  (let ((visited nil)
        (item '(:reference "@smith2024"
                :node (:node_key "file:smith.org"
                       :title "Smith"
                       :file_path "smith.org"
                       :line 3))))
    (cl-letf (((symbol-function 'org-slipbox-node-visit)
               (lambda (node &optional other-window)
                 (setq visited (list node other-window)))))
      (refbox-org-slipbox-open-note item)
      (should
       (equal visited
              '((:node_key "file:smith.org"
                 :title "Smith"
                 :file_path "smith.org"
                 :line 3)
                nil))))))

(ert-deftest refbox-org-slipbox-test-create_note_uses_capture_ref_contract ()
  "Note creation should call org-slipbox capture with refbox metadata."
  (let ((captured nil)
        (candidate '(:key "smith2024")))
    (cl-letf (((symbol-function 'refbox-template-format)
               (lambda (template reference)
                 (should (equal template
                                refbox-org-slipbox-note-title-template))
                 (should (eq reference candidate))
                 "Smith, Book"))
              ((symbol-function 'refbox-get-field-with-value)
               (lambda (fields reference)
                 (should (eq reference candidate))
                 (cond
                  ((member "title" fields) '("title" . "Book"))
                  ((member "author" fields) '("author" . "Smith"))
                  (t nil))))
              ((symbol-function 'org-slipbox-capture-ref)
               (lambda (reference title templates keys variables)
                 (setq captured
                       (list reference title templates keys variables))
                 '(:node_key "file:smith.org"
                   :title "Smith, Book"
                   :file_path "smith.org"
                   :line 1))))
      (let ((result (refbox-org-slipbox-create-note "smith2024" candidate)))
        (should
         (equal (car captured) "@smith2024"))
        (should
         (equal (cadr captured) "Smith, Book"))
        (should
         (equal (plist-get (nth 4 captured) :refbox-reference)
                "@smith2024"))
        (should
         (equal (plist-get (nth 4 captured) :refbox-title)
                "Book"))
        (should
         (equal (plist-get result :reference) "@smith2024"))))))

(ert-deftest refbox-org-slipbox-test-ref_add_adds_selected_key_to_current_node ()
  "Ref add should select a refbox reference and add its @KEY ref."
  (let (added)
    (cl-letf (((symbol-function 'refbox-read-reference)
               (lambda (&rest _args) '(:key "doe2026")))
              ((symbol-function 'org-slipbox-ref-add)
               (lambda (reference)
                 (setq added reference))))
      (should
       (equal (refbox-org-slipbox-ref-add) "@doe2026"))
      (should (equal added "@doe2026")))))

(ert-deftest refbox-org-slipbox-test-open_current_refs_uses_node_refs ()
  "Current-node helper should open refbox resources for @ refs."
  (let (opened)
    (cl-letf (((symbol-function 'org-slipbox-node-at-point)
               (lambda (&optional assert)
                 (should assert)
                 '(:node_key "file:note.org"
                   :refs ["@smith2024" "https://example.test" "@doe2026"])))
              ((symbol-function 'refbox-open)
               (lambda (references)
                 (setq opened references))))
      (refbox-org-slipbox-open-current-refs)
      (should (equal opened '("smith2024" "doe2026"))))))

(ert-deftest refbox-org-slipbox-test-mode_registers_and_restores_source ()
  "Minor mode should register, select, unregister, and restore note source."
  (let ((refbox-notes-sources nil)
        (refbox-notes-source 'file)
        (refbox-org-slipbox-set-active-source t))
    (unwind-protect
        (progn
          (refbox-org-slipbox-mode 1)
          (should (assq 'org-slipbox refbox-notes-sources))
          (should (eq refbox-notes-source 'org-slipbox))
          (refbox-org-slipbox-mode -1)
          (should-not (assq 'org-slipbox refbox-notes-sources))
          (should (eq refbox-notes-source 'file)))
      (refbox-org-slipbox-mode -1))))

(provide 'test-refbox-org-slipbox)

;;; test-refbox-org-slipbox.el ends here

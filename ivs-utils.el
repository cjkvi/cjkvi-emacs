;;; ivs-utils.el --- IVS editing utilities  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Tools for Editing Ideographic Variation Sequences
;; Created: 2013-01-01
;; Keywords: Ideographic Variation Sequence
;; Version: 0.0.1
;; Package-version: 0.0.1
;; URL: http://github.com/cjkvi/

;;; Commentary:
;; Typical settings: 
;; (autoload 'ivs-edit "ivs-utils" nil t)
;; (global-set-key (kbd "M-J") 'ivs-edit)

;;; Code:

(eval-when-compile (require 'cl))
(require 'ivs-tables)

(defvar ivs-preferred-collections '(adobe-japan1 hanyo-denshi)
  "List of preferred IVS collections.")

;;;###autoload
(defun ivs-edit ()
  "Insert and Verify IVS after the current point."
  (interactive)
  (cl-flet ((plist-string (plist char)
              (loop for x in plist by 'cddr concat (list char x))))
    (let* ((char (char-after (point)))
           (ivs  (char-after (1+ (point))))
           (aj1-plist (and (memq 'adobe-japan1 ivs-preferred-collections)
                           (aref ivs-aj1-char-table char)))
           (hd-plist  (and (memq 'hanyo-denshi ivs-preferred-collections)
                           (aref ivs-hd-char-table char)))
           (aj1 (plist-get aj1-plist ivs))
           (hd  (plist-get hd-plist ivs)))
      (cond (aj1 (message "Adobe-Japan1 CID+%d" aj1))
            (hd (message "Hanyo-Denshi %s" hd))
            (t
             ;; (message "aj1-plist=%s hd-plist=%s" aj1-plist hd-plist)
             (when (or aj1-plist hd-plist) (delete-char 1))
             (when aj1-plist (insert "【" (plist-string aj1-plist char)))
             (when (and aj1-plist (null hd-plist)) (insert  "】"))
             (when (and aj1-plist hd-plist) (insert  "｜"))
             (when (and (null aj1-plist) hd-plist) (insert  "《"))
             (when hd-plist (insert (plist-string hd-plist char) "》")))))))

;;;###autoload
(defun ivs-aj1-to-tex-cid-region (from to)
  "Convert IVS in REGION to LaTeX \\CID{XXXX} expressions."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cC\\)\\([󠄀-󠇯]\\)" nil t)
        (let* ((ivs-plist (aref ivs-aj1-char-table 
                                (string-to-char (match-string 1))))
               (cid (plist-get ivs-plist (string-to-char (match-string 2)))))
          (when cid (replace-match (format "\\\\CID{%d}" cid))))))))

;;;###autoload
(defun ivs-aj1-from-tex-cid-region (from to)
  "Convert LaTeX `\\CID{XXXX}' expressions in REGION to IVS."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\\\CID{\\([0-9]+\\)}" nil t)
        (let ((ivs (gethash (string-to-number (match-string 1)) 
                            ivs-aj1-cid-table)))
          (when ivs (replace-match ivs)))))))

;;;###autoload
(defun ivs-aj1-to-hd-region (from to)
  "Convert Adobe-Japan1 IVS to Hanyo-Denshi IVS in REGION."
  (interactive "r*")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cC\\)\\([󠄀-󠇯]\\)" nil t)
        (let* ((char (string-to-char (match-string 1)))
               (ivs (string-to-char (match-string 2)))
               (ivs-plist (aref ivs-aj1-char-table char))
               (cid (plist-get ivs-plist ivs))
               (hd (assoc-default cid ivs-aj1-hd-alist))
               (hd-ivs (gethash hd ivs-hd-id-table)))
          ;;(message "ivs=%s ivs-plist=%s cid=%s hd=%s hd-ivs=%s"
          ;;         ivs ivs-plist cid hd hd-ivs)
          (if hd-ivs (replace-match hd-ivs)
            (replace-match "" nil nil nil 1)))))))

;;;###autoload
(defun ivs-hd-to-aj1-region (from to)
  "Convert Hanyo-Denshi IVS to Adobe-Japan1 IVS in REGION."
  (interactive "r*")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cC\\)\\([󠄀-󠇯]\\)" nil t)
        (let* ((char (string-to-char (match-string 1))) 
               (ivs (string-to-char (match-string 2)))
               (ivs-plist (aref ivs-hd-char-table char))
               (hd (plist-get ivs-plist ivs))
               (cid (car (rassoc hd ivs-aj1-hd-alist)))
               (aj1-ivs (gethash cid ivs-aj1-cid-table)))
          ;;(message "ivs=%s ivs-plist=%s hd=%s cid=%s aj1-ivs=%s"
          ;;         ivs ivs-plist hd cid aj1-ivs)
          (if aj1-ivs (replace-match aj1-ivs)
            (replace-match "" nil nil nil 1)))))))

(defun ivs-aj1-non-aj1-search (to)
  "Search non-Adobe-Japan1 Kanji character for highlighting."
  (let (point char)
    (while (and (null point) (re-search-forward "\\cC" to t))
      (when (and (null (aref ivs-aj1-char-table
                             (setq char (string-to-char (match-string 0)))))
                 (<= #x3400 char))
        (setq point (point))))
    ;;(message "match-data = %s" (match-data))
    point))

;;;###autoload
(defun ivs-aj1-highlight-non-aj1 ()
  "Highlight Kanji characters do not belong to Adobe-Japan1 character set."
  (interactive)
  (font-lock-add-keywords
   nil '((ivs-aj1-non-aj1-search 0 highlight t))))

(provide 'ivs-utils)

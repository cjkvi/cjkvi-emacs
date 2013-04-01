;;; NAME.el --- DESCRIPTION  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: 
;; Created: 
;; Keywords:
;; Version: 0.0.1
;; Package-version: 0.0.1
;; URL: http://github.com/kawabata/

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'ivs-tables)

(defun ivs-aj1-to-tex-cid-region (from to)
  "Convert IVS in REGION to LaTeX CID commands"
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

(defun ivs-aj1-from-tex-cid-region (from to)
  "Convert CID in REGION to IVS."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\\\CID{\\([0-9]+\\)}" nil t)
        (let ((ivs (gethash (string-to-number (match-string 1)) ivs-aj1-cid-table)))
          (when ivs (replace-match ivs)))))))

(defun ivs-aj1-to-hd-region (from to)
  "Convert Adobe-Japan1 to Hanyo-Denshi in REGION."
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
          ;(message "ivs=%s ivs-plist=%s cid=%s hd=%s hd-ivs=%s"
          ;         ivs ivs-plist cid hd hd-ivs)
          (if hd-ivs (replace-match hd-ivs)))))))

(defun ivs-hd-to-aj1-region (from to)
  "Convert Hanyo-Denshi to Adobe-Japan1 in REGION."
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
          ;(message "ivs=%s ivs-plist=%s hd=%s cid=%s aj1-ivs=%s"
          ;         ivs ivs-plist hd cid aj1-ivs)
          (if aj1-ivs (replace-match aj1-ivs)))))))

(defun ivs-aj1-non-aj1-search (to)
  "Search for non-AJ1 Kanji character and highlight them."
  (let (point char)
    (while (and (null point) (re-search-forward "\\cC" to t))
      (when (and (null (aref ivs-aj1-char-table
                             (setq char (string-to-char (match-string 0)))))
                 (<= #x3400 char))
        (setq point (point))))
    ;;(message "match-data = %s" (match-data))
    point))

(defun ivs-aj1-highlight-non-aj1 ()
  "Highlight Kanji characters that are not belong to AJ1 character set."
  (interactive)
  (font-lock-add-keywords
   nil '((ivs-aj1-non-aj1-search 0 highlight t))))

(provide 'ivs-utils)

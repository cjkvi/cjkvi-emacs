;;; ids-edit.el ... IDS process management
;; Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>

;; Keywords: dictionary

;;; Code:

;;; Customizable Variables

;; 注意！ sqlite3 からは、LANGが、ja_JP.utf-8 では動作しなくなった。

(defvar ids-edit-kdp-sl3
  (concat (file-name-directory (locate-library "ids-edit")) "/kdp.sl3"))
(defvar ids-edit-unihan-sl3
  (concat (file-name-directory (locate-library "ids-edit")) "/unihan.sl3"))

(defvar ids-edit-sl3-program "sqlite3")
(defvar ids-edit-sl3-program-options nil)
(defvar ids-edit-sl3-environments '("LANG=en_US.utf-8"))
(defvar ids-edit-sl3-coding-system 
  (if (eq system-type 'darwin) 'utf-8-mac 'utf-8))

(defvar ids-edit-sl3-prompt "sqlite>") ;; \nsqlite> はダメ。

;;; Internal Variables

(defvar ids-edit-process nil)

;;; Constants

(defconst ids-edit-sql-utf8-format 
  "select distinct ch.v from ch where ch.k in (%s) limit 200")

(defconst ids-edit-sql-ucs-format 
  "select distinct * from (%s) limit 200")

(defconst ids-edit-sql-count-format 
  "select count(*) from (%s)")

(defconst ids-edit-sql-ids-format 
  "select ids.k from ids where ids.v glob \"%s\"")

(defconst ids-edit-sql-strokes-format 
  "select strokes.k from strokes where strokes.v between %d and %d ")

(defconst ids-edit-sql-japanese-format
  "select unihan.kIRG_JSource.k from unihan.kIRG_JSource where unihan.kIRG_JSource.v is not null ")

(defconst ids-edit-sql-char-strokes-format
  "select strokes.v from strokes where strokes.k = \"%X\" ")

(defconst ids-edit-sql-char-ids-format
  "select ids.v from ids where ids.k in (select ch.k from ch where ch.v = \"%c\") ")

(defconst ids-edit-sql-similar-format
  "select ch.v from ch where ch.k in (select similar.k from similar where similar.v = \"%s\" union select similar.v from similar where similar.k = \"%s\") ")

(defconst ids-edit-parse-regexp
  "\\([?*⿰-⿻⺀-⻳〢㇀㇉㐀-鿿豈-﫿𠀀-𫜴-]+\\)\\([0-9][0-9]?\\)?\\(-[0-9]?[0-9]?\\)?\\(J\\)?")

(defconst ids-edit-kanji-regexp "[⺀-⻳〢㇀㇉㐀-鿿豈-﫿𠀀-𫜴-]")

(easy-mmode-define-minor-mode 
 ids-edit-mode
 "IDS Edit Mode"
 nil
 "ids"
 '(("\M-Y" . "从") ("\M-H" . "■") ("\M-K" . "⇒") ("\M-0" . "⿰")
   ("\M-1" . "⿱") ("\M-2" . "⿲") ("\M-3" . "⿳") ("\M-4" . "⿴")
   ("\M-5" . "⿵") ("\M-6" . "⿶") ("\M-7" . "⿷") ("\M-8" . "⿸")
   ("\M-9" . "⿹") ("\M--" . "⿺") ("\M-=" . "⿻"))
 (setq tab-always-indent nil)
 (setq indent-tabs-mode t)
 )

(defun ucs-pua-to-big5-eudc (char)
  (let* ((x (- char #xee1b))
         (y (% x 157))
         (y (+ y (if (< y 63) 64 98))))
    (format "CDP-%02X%02X" (+ (/ x 157) #x80) y)))

(defun ids-txt-cleanup ()
  (interactive)
  (goto-char (point-min))
  (delete-matching-lines "^#")
  (while (re-search-forward "	#.*$" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "[-]" nil t)
    (replace-match 
     (concat "&" (ucs-pua-to-big5-eudc (string-to-char (match-string 0))) ";"))))

;;;
;;; Process Function
;;;

(defun ids-edit-get-process ()
  (let ((coding-system-for-read ids-edit-sl3-coding-system)
        (coding-system-for-write ids-edit-sl3-coding-system)
        (process-environment
         (append ids-edit-sl3-environments
                 process-environment)))
    (unless (and ids-edit-process 
                 (eq (process-status ids-edit-process) 'run))
      (condition-case nil
          (if ids-edit-process 
              (ids-edit-kill-process ids-edit-process))
        (error nil))
      (message "Creating new ids-edit process. args=%s"
               (append ids-edit-sl3-program-options
                       (list ids-edit-kdp-sl3)))
      (setq ids-edit-process 
            (apply 'start-process "ids-edit" 
                   (generate-new-buffer "*ids-edit*")
                   ids-edit-sl3-program 
                   (append ids-edit-sl3-program-options
                           (list ids-edit-kdp-sl3))))
      (accept-process-output ids-edit-process 5)
      )))

(defun ids-edit-kill-process ()
  (set-process-filter ids-edit-process nil)
  (delete-process ids-edit-process)
  (kill-buffer (process-buffer ids-edit-process))
  (setq ids-edit-process nil))

(defun ids-edit-command (command)
  (ids-edit-get-process)
  (let (finished start-point output)
    (with-current-buffer (process-buffer ids-edit-process)
      (goto-char (point-max))
      (setq start-point (point))
      ;(message "command=%s;" command)
      (process-send-string ids-edit-process (concat command ";\n"))
      (while (null finished)
        (accept-process-output ids-edit-process 5)
        ;(message "output=%s" (buffer-substring start-point (point)))
        (forward-line -2)
        (if (< (point) start-point) (goto-char start-point))
        (when (search-forward ids-edit-sl3-prompt nil t)
          (setq finished t
                output (buffer-substring start-point (match-beginning 0))))))
    ;(message "command=`%s'" command)
    ;(message "output1=`%s'" output)
    (setq output (ids-edit-remove-echo output command))
    ;(message "output2=`%s'" output)
    (if (equal output "") nil
      (split-string (ids-edit-remove-echo output command) "\n"))))

;; 原因は不明だが、sqlite3はEmacsからサブプロセスで起動すると、送信した文字列を
;; エコーで返す場合がある。（Emacsの仕様ではないし、sqlite3単独では起きない。）
;; それに備えて、エコーを除去する
(defun ids-edit-remove-echo (output command)
  ;(message "output=%s command=%s" output command)
  (if (string-match (regexp-quote (concat command ";\n")) output)
      (setq output (substring output (match-end 0))))
  (setq output (replace-regexp-in-string "\n\n" "\n" output))
  (setq output (replace-regexp-in-string "^\n" "" output))
  (setq output (replace-regexp-in-string "\n$" "" output))
  output)

(defun ids-edit-char-strokes (char)
  "Get strokes of specified CHAR."
  (let ((output
         (ids-edit-command
          (format ids-edit-sql-char-strokes-format char))))
    (mapcar 'string-to-number output)
    ))

(defun ids-edit-char-ids (char)
  "Get IDS of specified CHAR."
  (let ((output
         (ids-edit-command
          (format ids-edit-sql-char-ids-format char))))
    (mapconcat
     'identity
     (split-string (regexp-opt output t) "[\\():?]" t) "")))

;;;
;;; Strokes Calculation Function
;;;

(defun ids-edit-mapthread (function seq1 &rest seqrest)
  ;; ind-util.el にはバグあり。
  (if seqrest
      (mapcar 
       (lambda (x)
         (apply 
          'ids-edit-mapthread 
          `(lambda (&rest y) (apply (quote ,function) ,x y))
          seqrest))
       seq1)
  (mapcar function seq1)))

(defun ids-edit-strokes (ids)
  "Calculate total strokes of IDS."
  (when (null (string-match "？" ids))
    (let* ((chars (string-to-list (replace-regexp-in-string "[^㐀-鿆𠀀-𫜴]" "" ids)))
           (chars-uniq (remove-duplicates chars))
           ;; IDS中の各漢字の数を数える。
           (char-count (mapcar (lambda (x) (cons x (count x chars))) chars-uniq))
           ;; (漢字 . 漢字数) を、(画数リスト . 漢字数) に変換。
           (strokes-count (mapcar (lambda (x) (cons (ids-edit-char-strokes (car x))
                                                    (cdr x)))
                                  char-count))
           ;; 掛け算により、画数リストに変換。
           (strokes (mapcar (lambda (x)
                              (mapcar (lambda (y) (* (cdr x) y)) (car x)))
                            strokes-count))
           ;; 各画数リストを足しあわせる。
           (strokes-list (flatten (apply 'ids-edit-mapthread '+ strokes))))
      strokes-list)))

;;;
;;; char-to-code, code-to-char functions
;;;

(defun kdp-util-char-to-code (char)
  (if (and (< #xf100 char) (< char #xf700))
      (let* ((x (- char #xee1b))
             (y (% x 157))
             (y (+ y (if (< y 63) 64 98))))
        (format "CDP-%02X%02X" (+ (/ x 157) #x80) y))
    (format "%X" char)))

(defun kdp-util-code-to-char (code)
  (if (string-match "^CDP" code)
      (let* ((x (string-to-number (substring str 4 6) 16))
             (y (string-to-number (substring str 6 8) 16))
             (x (+ (* (- x #x80) 157) (if (< y 129) (- y 64) (- y 98)))))
        (+ x #xee1b))
    (string-to-number code 16)))

;;;
;;; Main Function
;;;

(defun ids-edit-construct-sql (query &optional count)
  "Construct QUERY string.
It returns a list of `ids', `stroke range' and `option'.
If it does not match, return nil."
  (when (string-match (concat "^" ids-edit-parse-regexp "$") query)
    (let* ((ids (match-string 1 query))
           (from (match-string 2 query))
           (to (match-string 3 query))
           (flag (match-string 4 query))
           strokes
           ids-sql
           strokes-sql
           flag-sql)
      (setq strokes (if (or from to) (ids-edit-strokes ids))
            from    (if from (string-to-number from) 0)
            to      (if to (if (= (length to) 1) 99 ;; e.g. "20-" -> "20-99"
                             (- 0 (string-to-number to)))
                      from)
            ids-sql (format ids-edit-sql-ids-format (concat "*" ids "*")))
      (when strokes
        (setq strokes-sql
              (mapconcat (lambda (x)
                           (format ids-edit-sql-strokes-format (+ x from) (+ x to)))
                         strokes " intersect ")))
      (when flag 
        (setq flag-sql ids-edit-sql-japanese-format))
      (format 
       (if count ids-edit-sql-count-format
           ids-edit-sql-utf8-format )
       (concat ids-sql
               (if strokes-sql (concat " intersect " strokes-sql))
               (if flag (concat " intersect " flag-sql))))
      )))

;;;
;;; Edit Function
;;;

;;;###autoload
(defun ids-edit-char (arg)
  "Edit Kanji Components.
If ARG is provided, decompose char even if char exists at point."
  (interactive "P")
  (cond
   ((and (null arg)
         (looking-at ids-edit-parse-regexp))
    ;; Forward matching
    (let ((end (match-end 0))
          (result (ids-edit-command (ids-edit-construct-sql (match-string 0)))))
      (if (null result)
          (message "No such pattern!")
        (delete-region (point) end)
        (if (< (length result) 2)
            (apply 'insert result)
          (insert "《")
          (apply 'insert result)
          (insert "》")))))
   ((looking-back ids-edit-kanji-regexp)
    (let* ((char (char-before (point)))
           (decomposition (ids-edit-char-ids char)))
      (when decomposition
        (backward-delete-char 1)
        (insert decomposition))))))

(provide 'ids-edit)

;;; ids-edit.el ends here

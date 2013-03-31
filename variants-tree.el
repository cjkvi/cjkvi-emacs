(require 'variants)

(defun variants-tree-expander (widget)
  "WIDGETの子となる木のリストを返す。"
  (let* ((char     (widget-get widget :char))
         (parents  (cons char (widget-get widget :parents)))
         (node-aj1 (variants-tree-ivs 
                    char ivs-aj1-table 
                    "%c【Adobe-Japan1 IVS】" "%c%c (CID+%d)"))
         (node-hd  (variants-tree-ivs 
                    char ivs-hd-table
                    "%c【汎用電子 IVS】" "%c%c (%s)"))
         (variants-table (variants-char-table char))
         (variants)
         (nodes))
    (maphash (lambda (key val) (push key variants)) variants-table)
    (if node-aj1 (push node-aj1 nodes))
    (if node-hd  (push node-hd nodes))
    (maphash
     (lambda (char attributes)
       (when (not (memq char parents))
         (push 
          (list 'tree-widget 
                :tag (format "%s %s" (if (characterp char) (char-to-string char)
                                       char)
                             (variants-to-names attributes))
                :char char
                :open nil :parents (union parents variants)
                          :expander 'variants-tree-expander)
          nodes)))
     variants-table)
    (nreverse nodes)))

(defun variants-tree-ivs (char ivs-table name-format ivs-format)
  (let ((aj1 (gethash char ivs-table))
        children)
    (when aj1
      (while aj1
        (push 
         (list 'tree-widget
               :tag (format ivs-format char (car aj1) (cadr aj1))
               :open t :has-children nil)
         children)
        (setq aj1 (cddr aj1)))
      `(tree-widget :tag ,(format name-format char)
                    :open nil
                    ,@(nreverse children)))))

(defun variants-to-names (list)
  (mapconcat 
   (lambda (x) 
     (or (assoc-default x variants-name)
         (symbol-name x)))
   list "・"))

(defun variants-tree  (&optional char)
  (interactive
   (let* ((char (char-after (point)))
          (mnemonics (category-set-mnemonics (char-category-set char))))
     (list (string-to-char
            (read-string "Char? : " (if (string-match "C" mnemonics)
                                        (char-to-string char)))))))
  (switch-to-buffer "*異体字検索*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapcar #'delete-overlay (car all)))
  (widget-insert "【異体字検索】\n\n")
  (widget-create 'tree-widget
                 :tag (format "%c (%04X)" char char)
                 :char char
                 :parents nil
                 :open nil ; t
                 :expander 'variants-tree-expander
                 :has-children t)
  (widget-insert "\n")
  ;; Insert the Close button
  (widget-create 'push-button
                 :keymap tree-widget-button-keymap ; Emacs
                 :notify 'variants-tree-close
                 "Close")
  (use-local-map widget-keymap)
  (widget-setup)
  (help-mode)
  (goto-char (point-min))
  (widget-forward 1))

(defun variants-tree-close (&rest ignore)
  "Close the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'variants-tree)

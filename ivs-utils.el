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
(require 'ivs-table)
;; ivs-aj1-char-table / ivs-aj1-cid-table
;; ivs-hd-char-table / ivs-hd-id-table

;; This data is converted from aj16-gsub-jp04.txt.
;; Copyright (c) Adobe-Corporation
(defvar ivs-aj1-trad-table
  #s(hash-table data 
    (1125 4108 1137 4849 1143 7330 1145 4523 1171 4468 1181 5603
     1193 6957 1201 4534 1203 13320 1204 5918 1221 7109 1254 4430
     1259 5255 1268 6577 1274 7258 1276 13321 1281 4469 1297
     13322 1304 7416 1310 4557 1312 4911 1315 8503 1316 5342 1317
     5367 1321 7369 1323 13323 1337 13324 1338 5926 1342 4170
     1345 4194 1362 13325 1384 5720 1393 4171 1397 4518 1401
     13326 1403 4912 1410 13327 1414 6136 1426 13328 1427 13329
     1440 6497 1446 5047 1447 5023 1450 5366 1454 6647 1462 4619
     1463 4707 1464 5276 1475 7651 1479 13330 1482 13331 1492
     5937 1511 4293 1512 4320 1518 8436 1531 5347 1533 13332 1535
     5490 1544 6162 1549 6650 1550 6688 1556 7083 1557 7095 1564
     4715 1579 13333 1591 13334 1596 5348 1598 5381 1601 13335
     1616 4189 1620 4941 1624 5619 1670 6300 1676 5030 1677 5038
     1679 13336 1702 4676 1708 4984 1713 5630 1719 8637 1721
     13337 1727 5121 1736 13338 1752 13339 1760 4308 1766 7251
     1796 4291 1798 8611 1816 4792 1817 4851 1821 13340 1826 5473
     1830 6083 1831 6138 1834 6364 1836 6538 1840 6858 1841 7181
     1842 7396 1843 6465 1848 13341 1853 6157 1862 4196 1867 4271
     1869 4466 1877 5310 1878 5279 1881 5652 1882 13342 1885 6112
     1893 7107 1894 7190 1895 7259 1899 4449 1966 5064 1983 4749
     1987 4828 2029 7045 2030 5853 2040 6482 2051 4467 2052 13343
     2055 8717 2113 5526 2117 5857 2120 5898 2126 4276 2153 5326
     2164 13344 2166 7121 2176 4329 2178 4887 2181 5219 2187 6569
     2189 6774 2194 5355 2225 13345 2227 6078 2233 13346 2243
     7458 2247 4210 2265 6885 2282 5527 2286 4632 2293 6441 2296
     4636 2301 13347 2302 13348 2304 13349 2317 6966 2339 4537
     2345 5061 2359 13350 2376 4794 2380 5506 2381 5650 2382 6115
     2389 13351 2391 6230 2418 4244 2421 13352 2423 7700 2425
     8595 2426 13353 2430 8622 2432 5067 2449 4558 2453 4641 2476
     13354 2478 5585 2485 8581 2486 5917 2498 6717 2512 4100 2514
     4267 2517 4530 2518 4607 2522 5197 2524 5443 2525 13355 2526
     5727 2527 5928 2529 6731 2530 6963 2532 4457 2542 6656 2552
     4633 2555 13356 2565 13357 2566 8580 2586 5805 2591 7152
     2596 4471 2606 6048 2610 6954 2613 6923 2615 7273 2618 5072
     2619 5285 2633 8534 2656 6222 2665 7145 2666 7457 2689 5015
     2692 5945 2693 13358 2704 4642 2707 4940 2713 5446 2716 5509
     2723 6152 2730 6766 2731 6809 2734 7005 2743 5899 2758 13359
     2768 13360 2770 4331 2774 4532 2778 13361 2782 4986 2784
     5006 2789 13362 2794 5600 2799 6096 2803 6368 2807 6603 2813
     7250 2815 8423 2816 13363 2817 6290 2818 6461 2819 13364
     2824 13365 2832 4657 2835 6147 2852 4521 2862 7274 2864 4643
     2867 4725 2874 5497 2886 6293 2892 2893 2898 5033 2900 5522
     2902 7732 2927 4408 2928 13366 2930 4966 2942 6279 2946 4470
     2948 4775 2949 5079 2962 5774 2967 6924 2985 5105 2988 6554
     2992 7044 2996 8548 2998 13367 3007 4759 3011 13368 3012
     13369 3020 6226 3032 5066 3039 13370 3049 7746 3063 4533
     3091 13371 3098 6919 3118 7039 3128 6874 3130 7437 3131 4186
     3150 8636 3153 5855 3160 7441 3176 5200 3178 5336 3181 5430
     3182 3183 3184 5723 3225 8452 3232 5648 3233 6727 3237 13373
     3239 4651 3268 6137 3273 13374 3277 6761 3278 6893 3312 4870
     3316 6261 3324 5140 3335 4753 3336 4968 3349 13375 3354 6767
     3358 6537 3380 7425 3395 5785 3397 7282 3400 4957 3423 13376
     3433 13377 3438 6572 3440 13378 3454 13379 3455 5891 3490
     5179 3519 5531 3522 13380 3523 7788 3524 13381 3525 8564
     3552 13382 3569 8583 3575 4971 3577 4128 3595 13383 3597
     13384 3602 5952 3617 5075 3621 6929 3625 13385 3634 13386
     3638 5922 3653 4638 3673 6614 3675 6741 3709 13387 3723 6201
     3734 13388 3751 4143 3754 6408 3756 5469 3796 13389 3815
     5645 3821 13390 3835 4776 3840 6466 3841 6724 3845 6453 3870
     13391 3879 4103 3880 7208 3881 6299 3882 6726 3891 5009 3895
     5292 3906 6706 3922 4141 3924 8627 3930 4101 3933 13392 3937
     6648 3964 13393 3965 3966 3970 13394 3974 4215 3980 5651
     3992 8594 4005 4527 4006 13395 4008 13396 4013 4292 4017
     5900 4022 7142 4024 7463 4025 13397 4026 13398 4032 4929
     4037 13399 4041 13400 4045 5597 4049 4286 4051 13401 4053
     8489 4054 5293 4060 6007 4064 8635 4067 5894 4069 13402 4081
     4080 4087 5551 3206 (7297 13372) 3627 (4277 5689 6886)))
  "GSUB `trad' table from CID to CID." )

(defun ivs-aj1-trad-region (from to &optional arg)
  "Convert Japanese Kanji to traditional form."
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cC\\)" nil t)
        (let* ((char (string-to-char (match-string 1)))
               (ivs-plist (aref ivs-aj1-char-table char))
               trad)
          (while ivs-plist
            (let* ((cid (cadr ivs-plist))
                   (trad-cid (gethash cid ivs-aj1-trad-table)))
              (if trad-cid
                  (setq ivs-plist nil trad (gethash trad-cid ivs-aj1-cid-table))
                (callf cddr ivs-plist))))
          (when trad (replace-match trad))))
      (unless (null arg)
        (goto-char (point-min))
        (re-search-forward "\\([󠄀-󠇯]\\)" nil t)
        (replace-match "")))))

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

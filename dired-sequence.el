;;; dired-sequence.el --- Handle files that are named in a sequential manner in dired

;; License: GPL-3+

;;; Commentary:
;; 

;; Media files, like image files and sound files are often named
;; according to numerical patterns.  Some types of operations on media
;; files are lossy, resulting in missing files, files not being named
;; in the intended order, etc.

;; One example is image files scanned from a book.  Each page gets a
;; file name like 0001.djvu for instance.  If the scanner accidentaly
;; double-feeds pages, image file names no longer match book page
;; numbers. Another example is converting compact discs to audio
;; files.

;; This library is intended to help fix issues like these.

;;Some concepts:

;;Sequence Expression: Used both for matching filenames, and generating
;;new filenames. Therefore a string compliant with the "format"
;;function rather than a normal regexp is used.  example "00-%4d.djvu" ,
;;"%4d" is interpreted for pattern matching.  This will match filenames
;;looking like "00-0001.djvu" etc.  

;;Gap: A gap in the sequential numbering of files, some files are
;;aparently missing in the middle of a sequence

;;Offset: Used when offseting sequential filenames up or down in
;;sequence.

;;TODO:

;;Maybe it would be useful with a more complete regexp syntax for mark
;;expressions, but so far the current syntax is convenient and has
;;worked well.

;;It would be nice to be able to set a bunch of section names in
;; one go, maybe similar to fsdired.

;;; History:

;; 2009-08-7: v0.1

;;; Code:

(defvar dired-sequence-history nil)
(defvar dired-sequence-default nil)

(defun dired-sequence-read-sequence-expression ()
  "Read a sequence-expression from minibuffer.
suitable for interactive declarations"
  (setq dired-sequence-default
        (read-string
         (format "Sequence expression(%s): " dired-sequence-default)
         nil 'dired-sequence-history dired-sequence-default)))

(defun dired-sequence-find-gap (sequence-expression)
  "Move cursor to next sequence gap.
Argument SEQUENCE-EXPRESSION see commentary."
  ;;TODO dont error when reacing end of file list without finding a gap
  (interactive
   (list (dired-sequence-read-sequence-expression)))
  (dired-sequence-mark-helper sequence-expression
                              (lambda() (dired-next-line 1) )))

(defun dired-sequence-list-gaps (sequence-expression)
  "List which files are missing in sequence."
  (interactive
   (list (dired-sequence-read-sequence-expression)))
  (dired-sequence-find-gap sequence-expression)
  
  )


(defun dired-sequence-mark (sequence-expression)
  "Mark files that are sequential, until a gap is encountered.
SEQUENCE-EXPRESSION see commentary."
  (interactive
   (list (dired-sequence-read-sequence-expression)))
  (dired-sequence-mark-helper sequence-expression
                              (lambda()(dired-mark 1))))

(defun dired-sequence-rename-offset (sequence-expression offset)
  "Iterate previously marked files.
According to SEQUENCE-EXPRESSION, offset the filenames by OFFSET
using wdired."
  (interactive
   (progn
     (unless (eq major-mode 'wdired-mode) (error "Dired buffer must be editable")) ;;TODO ask if to switch to wdired, refactor
     (list (dired-sequence-read-sequence-expression)
           (string-to-int (read-string "Offset: " "1")))))
  (dired-map-over-marks
   (progn
     (dired-move-to-filename)
     (let*
         ((newname (dired-sequence-offset-filename sequence-expression offset)))
       (insert newname)
       (kill-line)
       ))
   nil
   ))

(defun dired-sequence-rename-sequential (sequence-expression count step)
  "rename files sequentialy in the order presented in the dired buffer.
  ignoring current naming. this is for files with odd sequencing like \"xaaa\" \"xaab\"
  which is in fact used by \"tiffsplit\""
  (interactive 
   (progn
     (unless (eq major-mode 'wdired-mode) (error "Dired buffer must be editable"))
     (list (dired-sequence-read-sequence-expression)
           (string-to-int (read-string "Start: " "1"))
           (string-to-int (read-string "Step: " "1")))));TODO should be prefix arg
  (dired-map-over-marks
   (progn
     (dired-move-to-filename)
     (let*
         ((newname (format sequence-expression count))
         
          )
       (insert newname)
       (kill-line)
       (setq count (+ step count))
       ))
   nil
   ))

(defun dired-sequence-to-sequence-rename (sequence-expression-1 sequence-expression-2)
  "SEQUENCE-EXPRESSION-1 extracts an ordinal from a filename. SEQUENCE-EXPRESSION-2 is then used to rename the file.
Sequences like \"Chapter 1\" to \"Chapter 10\" which dont sort
well on audio devices, can then be renamed to \"Chapter 01\" to
\"Chapter 10\".
"
  (interactive 
   (progn
     (unless (eq major-mode 'wdired-mode) (error "Dired buffer must be editable"))
     (list (dired-sequence-read-sequence-expression)
           (dired-sequence-read-sequence-expression)
           )))
  (let
      ((seq-1-regexp (dired-sequence-format-string-to-regexp sequence-expression-1)))
    (dired-map-over-marks ;;TODO refactor
     (progn
       (dired-move-to-filename)
       (let*
           ((s (re-search-forward (dired-sequence-format-string-to-regexp sequence-expression-1 ))) ;;TODO limit to EOL
            (ordinal (string-to-number (match-string 1)))
            (newname (format sequence-expression-2 ordinal))
            )
         (dired-move-to-filename)         
         (insert newname)
         (kill-line)
         ))
     nil
     )))
  


(defun dired-sequence-mark-helper (sequence-expression fn)
  "Mark acording to given rule.
Argument SEQUENCE-EXPRESSION see commentary.
Argument FN function to call for each file in the sequence."
  (cond
   ((string-match (dired-sequence-format-string-to-regexp sequence-expression)
                  (dired-get-filename 'no-dir));;ensure 1st dired entry matches!
    (let ((filename-matching t))
      (while filename-matching
        ;;TODO special case when last entry sucessfully matched! No error then!
        (let*
            ((next-expected (dired-sequence-offset-filename sequence-expression 1)))
          (funcall fn );;for instance move to next line
          (setq filename-matching
                (eq 0 (string-match next-expected (dired-get-filename 'no-dir))))))))
   (t (error "sequence expression doesnt match 1st dired entry"))))
   

(defun dired-sequence-offset-filename (sequence-expression offset)
  "Get file name at point, interpret is as part of a sequence.
described by SEQUENCE-EXPRESSION, offset the filename by OFFSET"
  ;;file at point
  (let*
      ((cur-file-name (dired-get-filename 'no-dir))
       (rxp (dired-sequence-format-string-to-regexp  sequence-expression))
       ;;figure out start val of counter
       (mymatch (string-match rxp cur-file-name))
       ;;TODO do something cleverer when mymatch is nil
       (matchval (match-string 1 cur-file-name))
       (counter (+ offset (string-to-int matchval)))
       ;;see if current file matches,  else quit
       (expected-cur-file-name (format sequence-expression counter))
       )
    ;;(message "cur-file-name:%s rxp:%s mymatch:%s counter:%s expected-cur-file-name:%s" cur-file-name rxp mymatch counter expected-cur-file-name)
    expected-cur-file-name)
  )


(defun dired-sequence-format-string-to-regexp (format-string)
  "Xxx%0dyy to xxx([0-9]{d})yyy.
Argument FORMAT-STRING is a simple version of the argument of the format function.
00-%04d.jpg is valid for instance, more complex variants currently not.
"
  (let*
      ((matchstr (string-match "\\([^%]*\\)%\\([0-9]*\\)d\\(.*\\)" format-string))
       (before-% (match-string 1 format-string))
       (after-% (match-string 3 format-string))
       (pre-width (match-string 2 format-string))
       
       )

    ;;TODO append $ to regexp
    (if (equal "" pre-width)
        (concat  before-% "\\([0-9]+\\)" after-% )
      ;;TODO (number-to-string (string-to-number ...))fugly way to strip leading 0
      (concat  before-% "\\([0-9]\\{" (number-to-string (string-to-number pre-width)) "\\}\\)" after-% ))
    ))


(provide 'dired-sequence)

;;; dired-sequence.el ends here

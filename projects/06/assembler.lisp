;
; NAME
;
; assembler.lisp - Hack assembler written in Lisp
;
; DESCRIPTION
;
; This is a assembler written in Lisp designed for the assembly
; language described in the book "The Elements of Computing Systems"
; by Noam Nissan
; and Shimon Schocken.
;
; SYNOPSIS
;
; Start your favorite Lisp interpreter
;
;   $ sbcl
;   *
;
; Then load this file and assemble the code.
;
;   * (load "assembler.lisp")
;   * (asmfile "pong/Pong.asm" "Pong.hack")
;
; COPYRIGHT
;
; Copyright &copy; 2013, Jeremiah Mahler.  All Rights Reserved.<br>
; This project is free software and released under
; the [GNU General Public License][gpl].
;
; [gpl]: http://www.gnu.org/licenses/gpl.html
;

; {{{ Stage I: construct symbol table

(defun rmcomm (str)
  "Remove comments and spaces from a single string"
  (remove #\Return
    (remove #\Newline
      (remove #\Space (subseq str 0 (search "//" str))))))

(defun cleanlines (lines)
  "Remove all the comments, spaces and blank lines"
  (let ((line (car lines)))
	(if (null line)
	  nil
	  (let ((line2 (rmcomm line)))
		(if (string-equal "" line2)
		  (cleanlines (cdr lines))
		  (cons line2 (cleanlines (cdr lines))))))))

(defun findaddrs (lines &optional (n 0))
  "Find the (ADDRESS) entries in the clean lines"
  (let ((line (car lines)))
	(if (null line)
	  nil
	  (if (eql #\( (char line 0))
		(cons (list (subseq line 1 (position #\) line)) n)
			  (findaddrs (cdr lines) n))
		(findaddrs (cdr lines) (+ n 1))))))

(defun rmaddrs (lines addrs &optional (n 0))
  "Remove the found addresses"
  (if (null lines)
	nil
	(let* ((line (car lines))
		   (pos (cadar addrs)))
	  (if (eq n pos)
		(rmaddrs (cdr lines) (cdr addrs) n)
		(cons line (rmaddrs (cdr lines) addrs (+ n 1)))))))

(defun gen (start end &optional (inc 1))
  "generate a range of numbers"
  (if (> start end)
	nil
	(cons start (gen (+ start inc) end inc))))

; standard addresses
(defparameter *stdaddrs*
  '(("SP"     0)
	("LCL"    1)
	("ARG"    2)
	("THIS"   3)
	("THAT"   4)
	("THAT"   5)
	("SCREEN" 16384)
	("KBD"    24576)))

; memory addresses to allocate for non-standard labels
(defparameter *labelalloc* 16)

(setq *stdaddrs* (concatenate 'list *stdaddrs*
	(mapcar (lambda (x)
			  (list (format nil "R~A" x) x))
			  (gen 0 (- *labelalloc* 1)))))

(defun subaddrs (lines addrs)
  "Substitute for the known addresses"
  (if (null lines)
	nil
	(let ((line (car lines)))
	  (if (not (eql #\@ (char line 0)))
		(cons line (subaddrs (cdr lines) addrs))
		(let* ((label (subseq line 1))
			   (match (find-if (lambda (a)
							(string-equal (car a) label)) addrs)))
		  (if (null match)
			(cons line (subaddrs (cdr lines) addrs))
			(cons (concatenate 'string "@" (format nil "~A" (cadr match)))
				  (subaddrs (cdr lines) addrs))))))))

(defun findlabels (lines)
  "Find the labels that need memory addresses assigned."
  (let ((line (car lines)))
	(if (null line)
	  nil
	  (if (not (eql #\@ (char line 0)))
		(findlabels (cdr lines))
		(if (digit-char-p (char line 1))
		  (findlabels (cdr lines))
		  (cons (subseq line 1) (findlabels (cdr lines))))))))

(defun alloclabels (lines &optional (alloc *labelalloc*))
  (if (null lines)
	nil
	(cons (list (car lines) alloc) (alloclabels (cdr lines) (+ alloc 1)))))

; }}}

; {{{ Stage II, assemble

(defun dup (x n)
  "Duplicate x n times"
  (if (<= n 0)
	nil
	(cons x (dup x (- n 1)))))

(defun asmem (asm)
  "Assemble @1 -> 00000001"
  (let* ((nstr (subseq asm 1))
		 (n (parse-integer nstr))
		 (bstr (format nil "~B" n))
		 (l (length bstr)))
	(concatenate 'string (dup #\0 (- 16 l)) bstr)))

(defparameter *yccmds*
  '(("0" "0" "101010")
	("1" "0" "111111")
	("-1" "0" "111010")
	("D" "0" "001100")
	("A" "0" "110000")
	("M" "1" "110000")
	("!D" "0" "001101")
	("!A" "0" "110001")
	("!M" "1" "110001")
	("-D" "0" "001111")
	("-A" "0" "110011")
	("-M" "1" "110011")
	("D+1" "0" "011111")
	("A+1" "0" "110111")
	("M+1" "1" "110111")
	("D-1" "0" "001110")
	("A-1" "0" "110010")
	("M-1" "1" "110010")
	("D+A" "0" "000010")
	("D+M" "1" "000010")
	("D-A" "0" "010011")
	("D-M" "1" "010011")
	("A-D" "0" "000111")
	("M-D" "1" "000111")
	("D&A" "0" "000000")
	("D&M" "1" "000000")
	("D|A" "0" "010101")
	("D|M" "1" "010101")))

(defun xccmd (xasm)
  "ADM -> 111 (ddd)"
  (labels ((find1 (a s)
			  (if (find a s :test #'string-equal)
			    "1" "0")))
	(concatenate 'string
	  (find1 "A" xasm)
	  (find1 "D" xasm)
	  (find1 "M" xasm))))

(defun asmccmd (asm)
  "Assemble a C-command: D=D+A, etc"
  (let* ((eqps (position #\= asm))
		 (y (subseq asm (+ eqps 1)))
		 (x (subseq asm 0 eqps)))
	(apply #'concatenate 'string
		   (append
			 '("1" "11")
			 (cdr (find y *yccmds* :test #'string-equal :key #'car))
			 (list (xccmd x))
			 '("000")))))

(defvar *jcmds*
  ; match jjj
  '(("JGT" "001")
	("JEQ" "010")
	("JGE" "011")
	("JLT" "100")
	("JNE" "101")
	("JLE" "110")
	("JMP" "111")))

(defun asmjdac (d)
  "Get the a and c componens given d (ADM)"
  (if (string-equal d "001")
	'("1" "110000")
	(if (string-equal d "100")
	  '("0" "110000")
	  (if (string-equal d "000")
		'("0" "101010")
		'("0" "001100")))))

(defun asmjcmd (asm)
  "Assemble a jump (D;JEQ) command"
  (let* ((eqps (position #\; asm))
		 (y (subseq asm (+ eqps 1)))
		 (x (subseq asm 0 eqps))
		 (d (xccmd x))
		 (ac (asmjdac d)))
	(apply #'concatenate 'string
		   (append
			 '("1" "11")
			 ac
			 '("000")
			 (cdr (find y *jcmds* :test #'string-equal :key #'car))))))

(defun asmline (asm)
  "Assemble a line of code"
  (if (find #\@ asm)
	(asmem asm)
	(if (find #\= asm)
	  (asmccmd asm)
	  (if (find #\; asm)
		(asmjcmd asm)
		'(UNKNOWN COMMAND)))))
; }}}

(defun asmfile (infile outfile)
  "Assemble a file of code"
  (with-open-file (in infile :direction :input)
	(with-open-file (out outfile :direction :output)
	  (let ((lines)
			(addrs)
			(lbls))
		(do ((line (read-line in nil 'eof)
				   (read-line in nil 'eof)))
		    ((eql line 'eof))
			(setq lines (cons line lines)))
		(setq lines (reverse lines))
		(setq lines (cleanlines lines))
		(setq addrs (findaddrs lines))
		(setq addrs (concatenate 'list addrs *stdaddrs*))
		(setq lines (rmaddrs lines addrs))
		(setq lines (subaddrs lines addrs))
		(setq lbls (remove-duplicates (findlabels lines)
										:test #'string-equal
										:from-end t))
		(setq lbls (alloclabels lbls))
		(setq lines (subaddrs lines lbls))
		; now the code can be assembled
		(setq lines (mapcar #'asmline lines))
		(mapc (lambda (line) (format out "~A~%" line)) lines)
		'done))))

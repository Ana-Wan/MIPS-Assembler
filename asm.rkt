#lang racket

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.
(define (scan str)
  (scan-func str asmtrlst 'start asmfinal))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)

(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.

(define-struct token (kind lexeme) #:transparent)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (asmtrlst, 'start, asmfinal).
;; Definitions of asmtrlst and asmfinal follow.

;; functions used in defining sample transition table

(define (one-to-nine? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (hex-digit? ch)
  (or
   (char-numeric? ch)
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))

(define (chartest ch)
  (lambda (x) (char=? x ch)))

;; sample transition table

(define asmtrlst
  (list
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start char-alphabetic? 'id)
   (make-transition 'id char-alphabetic? 'id)
   (make-transition 'id char-numeric? 'id)
   (make-transition 'start one-to-nine? 'int)
   (make-transition 'int char-numeric? 'int)
   (make-transition 'start (chartest #\-) 'minus)
   (make-transition 'minus char-numeric? 'int)
   (make-transition 'start (chartest #\,) 'comma)
   (make-transition 'start (chartest #\() 'lparen)
   (make-transition 'start (chartest #\)) 'rparen)
   (make-transition 'start (chartest #\$) 'dollar)
   (make-transition 'dollar char-numeric? 'register)
   (make-transition 'register char-numeric? 'register)
   (make-transition 'start (chartest #\0) 'zero)
   (make-transition 'zero (chartest #\x) 'zerox)
   (make-transition 'zero char-numeric? 'int)
   (make-transition 'zerox hex-digit? 'hexint)
   (make-transition 'hexint hex-digit? 'hexint)
   (make-transition 'id (chartest #\:) 'label)
   (make-transition 'start (chartest #\;) 'comment)
   (make-transition 'comment (lambda (x) true) 'comment)
   (make-transition 'start (chartest #\.) 'dot)
   (make-transition 'dot (chartest #\w) 'dotw)
   (make-transition 'dotw (chartest #\o) 'dotwo)
   (make-transition 'dotwo (chartest #\r) 'dotwor)
   (make-transition 'dotwor (chartest #\d) 'dotword)
   ))

;; sample list of final states

(define asmfinal
  (list
    'register
    'int
    'id
    'label
    'comma
    'lparen
    'rparen
    'zero
    'hexint
    'comment
    'dotword
    'whitespace
    ))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)

(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'whitespace)
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [(symbol=? state 'comment)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (cond
    [(symbol=? state 'int) (make-token 'int (check-int-range (list->number l)))]
    [(symbol=? state 'zero) (make-token 'int 0)]
    [(symbol=? state 'hexint) (make-token 'hexint (check-hexint-range (list->hexint (rest (rest l)))))]
    [(symbol=? state 'register) (make-token 'register (check-reg-range (list->number (rest l))))]
    [else (make-token state l)]))

;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

(define (list->hexint lst) (string->number (list->string lst) 16))

;; Scheme supports unbounded integers but MIPS doesn't
(define (check-int-range n)
  (cond
    [(<= -2147483648 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-hexint-range n)
  (cond
    [(<= 0 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-int-range-beqbne n)
  (cond
    [(<= -32768 n 32767) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-hexint-range-beqbne n)
  (cond
    [(<= 0 n 65535) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-reg-range n)
  (cond
    [(<= 0 n 31) n]
    [else (error 'ERROR "register out of range: ~a" n)]))

; Test to output tokens
;(scan "01")
;(scan "0xabcd ; should be ignored")
;(scan ".word1")
;(scan ".word aaa")
;(scan "0add")
;(scan "jr $31")
;(scan "foo:     add $1, $2, $3   ; A comment.")
;(scan "jr $31")
;(scan "foo:")
;(scan "alphonse: all:")
;(scan "123al:")
;(scan "al: al:")
;(scan "a12a;")
;(scan "*sdf:")
;(scan "::")
;(scan "@#a:")
;(scan "aaa:")
;(scan ".word me")
;(scan ".word 23")
;(scan "add $4,$4,$3")
;(scan "sub $4, $4, $3")
;(scan "slt $4, $4, $3")
;(scan "beq $4, $4, w")
;(scan "beq $4, $4, 9")
;(scan "beq $4, $4, 0x9")
;(scan "divu $5,$4")
;(scan "div $1,$4")
;(scan "multu $1,$9")
;(scan "lw $4,8($9)")

; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          [else (printf "~a~n" scanned)(scan-input)])]
    [else (scan-input)]))


; label is key: name of label without ':
;	   value: address of label
(define-struct label (key value) #:transparent)

; word is id: label name or '() if i is not an int/hex
;         value: address of label if any, or if i is an int/hex
(define-struct instr (id value))



;****************************************************************************************************************************
; The following helper function checks if line is a valid MIPS instruction

;; (isWord? token) takes in a token
;; Outputs true if it is a .word instruction
(define (isWord? token )
 (and (symbol=? (token-kind token ) 'dotword)
      (equal? (token-lexeme token ) '(#\. #\w #\o #\r #\d))))

;; (isjr-jalr-mfhi-mflo-lis? token) takes in a token
;; Outputs true if it is a jr/jalr/mfhi/mflo/lis instruction
(define (isjr-jalr-mfhi-mflo-lis? token )
  (define instruction (token-lexeme token ))
  (or (equal? instruction '(#\j #\r))
      (equal? instruction '(#\j #\a #\l #\r))
      (equal? instruction '(#\m #\f #\h #\i))
      (equal? instruction '(#\m #\f #\l #\o))
      (equal? instruction '(#\l #\i #\s))))

;; (ismult-multu-div-divu? token) takes in a token 
;; Outputs true if it is a mult/multu/div/divu instruction
(define (ismult-multu-div-divu? token )
  (define instruction (token-lexeme token ))
  (or (equal? instruction '(#\m #\u #\l #\t))
      (equal? instruction '(#\m #\u #\l #\t #\u))
      (equal? instruction '(#\d #\i #\v))
      (equal? instruction '(#\d #\i #\v #\u))))

;; (isadd-sub-slt-sltu? token) takes in a token 
;; Outputs true if it is a add/sub/slt/sltu instruction
(define (is-add-sub-slt-sltu? token)
  (define instruction (token-lexeme token))
  (or (equal? instruction '(#\a #\d #\d))
      (equal? instruction '(#\s #\u #\b))
      (equal? instruction '(#\s #\l #\t))
      (equal? instruction '(#\s #\l #\t #\u))))

;; (isbeq-bne? token) takes in a token
;; outputs true if it is a beq/bne instruction
(define (isbeq-bne? token)
  (define instruction (token-lexeme token))
  (or (equal? instruction '(#\b #\e #\q))
      (equal? instruction '(#\b #\n #\e))))

;; (islw-sw? token) takes in a token
;; outputs true if it is a lw/sw instruction
(define (islw-sw? token)
  (define instruction (token-lexeme token))
  (or (equal? instruction '(#\l #\w))
      (equal? instruction '(#\s #\w))))

;; (isCorrectLength? scanned x) outputs error if scanned does not have length x, else true
(define (isCorrectLength? scanned x)
  (cond [(equal? (length scanned)  x) #t]
                      [else (error 'ERROR)]))

;(IsInstr? scanned) returns true if scanned (line read from input) is a valid instruction and outputs (instr 'token-kind value)
; where value is the operand after the instruction
; else error
(define (IsInstr? scanned n)

  (define t (first scanned)) ;first token in scanned
  (define t-kind (token-kind (first scanned))) ;token-kind of (first scanned)
  (define t-lex (token-lexeme (first scanned))) ;token-lexeme of (first scanned)
  
	(cond
          [(and (isWord? (first scanned))
                (isCorrectLength? scanned 2)
                (isValid-i? (second scanned) 'word))
           (cond [(symbol=? (token-kind (second scanned)) 'id) ; instr is a word with label
                  (instr 'wordLab (token-lexeme (second scanned)))]
                 [else (instr 'wordVal (token-lexeme (second scanned)))]) ; i is an int or a hexint
           
           ]
          [(symbol=? (token-kind (first scanned)) 'id)
                  
                    (cond
                      ; jr,jalr,mfhi,mflo,lis must have a single register operand, $d
                      [(and (isjr-jalr-mfhi-mflo-lis? (first scanned))
                                (isCorrectLength? scanned 2)) 
                           (cond [(not (symbol=? (token-kind (second scanned)) 'register)) (error 'ERROR)]
                                 [else (instr (string->symbol (list->string t-lex))
                                              (token-lexeme (second scanned)) ; register 
                                              )])] 

                      ; mult,multu,div,divu must have 2 registers after it
                      [(and (ismult-multu-div-divu? (first scanned))
                                (isCorrectLength? scanned 4)) 
                           (cond [(not (and (symbol=? (token-kind (second scanned)) 'register)
                                            (symbol=? (token-kind (third scanned)) 'comma)
                                            (symbol=? (token-kind (fourth scanned)) 'register))) (error 'ERROR)]
                                 [else (instr (string->symbol (list->string t-lex))
                                              (list (token-lexeme (second scanned))     ; first register $s
                                                    (token-lexeme (fourth scanned))     ; second register $t
                                                    ))])]

                      ; add,sub,slt,sltu must have 3 registers after it
                      [(and (is-add-sub-slt-sltu? (first scanned))
                            (isCorrectLength? scanned 6))
                       (cond [(not (and (symbol=? (token-kind (second scanned)) 'register)
                                        (symbol=? (token-kind (third scanned)) 'comma)
                                        (symbol=? (token-kind (fourth scanned)) 'register)
                                        (symbol=? (token-kind (fifth scanned)) 'comma)
                                        (symbol=? (token-kind (sixth scanned)) 'register))) (error 'ERROR)]
                             [else (instr (string->symbol t-lex)
                                          (list (token-lexeme (second scanned))   ; first register $d
                                                (token-lexeme (fourth scanned))   ; second register $s
                                                (token-lexeme (sixth scanned)))   ; second register $t
                                          )])]
                          
                      ; bne,beq must have 2 registers and i after it
                      [(and (isbeq-bne? (first scanned))
                            (isCorrectLength? scanned 6))
                       (cond [(not (and (symbol=? (token-kind (second scanned)) 'register)
                                        (symbol=? (token-kind (third scanned)) 'comma)
                                        (symbol=? (token-kind (fourth scanned)) 'register)
                                        (symbol=? (token-kind (fifth scanned)) 'comma)
                                        (isValid-i? (sixth scanned) 'beqbne))) (error 'ERROR)]
                             [else (instr (string->symbol (list->string t-lex))
                                          (list (instr 'register (token-lexeme (second scanned))) ; first register ($s)
                                                (instr 'register (token-lexeme (fourth scanned))) ; second register ($t)
                                                (instr (token-kind (sixth scanned)) ; == 'id if i is a label,  'int if i is an int, 'hexint if i is a hexint
                                                       (token-lexeme (sixth scanned))) ; stores the value of i
                                                (instr 'pc (* n 4)))) ; stores position of the beq/bne
                              ])]

                      ;lw and sw must have 2 registers and a valid i
                      [(islw-sw? (first scanned))
                       (cond [(not (and (symbol=? (token-kind (second scanned)) 'register)
                                        (symbol=? (token-kind (third scanned)) 'comma)
                                        (symbol=? (token-kind (fifth scanned)) 'lparen)
                                        (symbol=? (token-kind (sixth scanned)) 'register)
                                        (symbol=? (token-kind (seventh scanned)) 'rparen)
                                        (isCorrectLength? scanned 7)
                                        (isValid-i? (fourth scanned) 'lwsw))) (error 'ERROR)]
                             [else (instr (string->symbol (list->string t-lex))
                                          (list (token-lexeme (second scanned))     ; first register $t
                                                (token-lexeme (fourth scanned))     ; i
                                                (token-lexeme (sixth scanned))))    ; second register $s
                                   ])]

                      )
                    
                    ]
              
          [else (error 'ERROR)]))


; outputs true if it is a valid immediate (32-bit signed or unsigned number, i.e i is an int or hexint or label):
; for word i:
; if i is a decimal number, must be in the range -2^31 through 2^32-1
; if i is hexadecimal, must not exceed 0xffffffff.
; if i is a label, value must be encoded as a 32-bit integer
; else outputs error
; for beq/bne/lw/sw i:
; if i is a decimal number, must be in the range -32768 through 32767
; if i is hexadecimal, must not exceed 0xffff.
; if i is a label, value must be encoded as a 32-bit integer

(define (isValid-i? i id)
	
	(define i-kind (token-kind i))
        (define i-lexeme (token-lexeme i))

  (cond [(or (symbol=? id 'beq) (symbol=? id 'bne))
         (cond
     		[(and (symbol=? i-kind 'int) (integer? i-lexeme))
      			(check-int-range-beqbne i-lexeme)]
     		[(and (symbol=? i-kind 'hexint) (integer? i-lexeme))
      			(check-hexint-range-beqbne i-lexeme)]

                
		[(symbol=? i-kind 'id) #t] ; label 
     		[else (error 'ERROR)]
     	)]
        [(symbol=? id 'lwsw)
         (cond
     		[(and (symbol=? i-kind 'int) (integer? i-lexeme))
      			(check-int-range-beqbne i-lexeme)]
     		[(and (symbol=? i-kind 'hexint) (integer? i-lexeme))
      			(check-hexint-range-beqbne i-lexeme)]
     		[else (error 'ERROR)]
     	)]
        [else
         (cond
     		[(and (symbol=? i-kind 'int) (integer? i-lexeme))
      			(check-int-range i-lexeme)]
     		[(and (symbol=? i-kind 'hexint) (integer? i-lexeme))
      			(check-hexint-range i-lexeme)]
		[(symbol=? i-kind 'id) #t] ; label 
     		[else (error 'ERROR)]
     	)])

)



;;****************************************************************************************************************************


; (isLabel? token-li) outputs true if line is a label
(define (isLabel? token-li)
    (symbol=? (token-kind (first token-li)) 'label))

; (is-valid-label? token-li label-list n word-list) checks from one read line
; if token-li has valid labels ,returns a list of (list of label names) and the current n  if it does
; label-list : stores all the label names - unique
; word-list: stores all the label ids in word with it's address
(define (is-valid-label? scanned label-list n instr-list)
  (cond [(empty? scanned) (list label-list n instr-list)]
	;if theres a word after a label
        [(not (symbol=? (token-kind (first scanned))  'label))
		(list label-list (+ n 1) (cons (IsInstr? scanned n)  instr-list))
	]
	
	; ouput error if not read a label 
        [(symbol=? (token-kind (first scanned))  'label)
         (define t-lexeme (token-lexeme (first scanned)))

              ; checks if has : at the end of label
              (cond
                [(and (char=? (last t-lexeme) #\:) ; error when label not end with :
                      (not (ormap (lambda (y) (equal? (remove #\: t-lexeme) (label-key y)))  label-list))) ; error when label already exists
                 
                 (define t-key (remove #\: t-lexeme))
                 
                 (define updated-label-list (cons (label t-key (* n 4)) label-list))
                 (is-valid-label? (rest scanned) updated-label-list n instr-list)] ;error when label is not unique
		
                [else (error 'ERROR)])  ;when first input is not a label             
                   
        ])
)


; (getLabelValue id lab-li) gets the value of the id(label) in lab-li if it exists and returns the address,
; else outputs error, label does not exist
(define (getLabelValue id lab-li)
	(cond [(empty? lab-li) (error 'ERROR)]
	      [(equal?  id (label-key (first lab-li))) (label-value (first lab-li))]
	      [else (getLabelValue id (rest lab-li))]))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;The following helper functions outputs binary encoding for each instruction

; (output-word-i i) outputs the binary encoding of word i
; i can be a label/int/hexint
(define (output-word-i i)
	(write-byte (bitwise-and (arithmetic-shift i -24) #xff))
	(write-byte (bitwise-and (arithmetic-shift i -16) #xff))
	(write-byte (bitwise-and (arithmetic-shift i -8) #xff))
	(write-byte (bitwise-and i #xff)))

;(output-beq-bne-i i op) outputss binary encoding of beq/bne $s,$t,i
; i can be a label/int/hexint
(define (output-beq-bne-i s t i op)
  (define b (bitwise-ior (arithmetic-shift op 26)
                         (arithmetic-shift s 21)
                         (arithmetic-shift t 16)
                         (bitwise-and i #xffff)))
  (write-byte (bitwise-and (arithmetic-shift b -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift b -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift b -8) #xff))
  (write-byte (bitwise-and b #xff))
  )

;(output-jr-jalr s op) outputs binary encoding of s in jr s or jalr s
; op = 8 if jr, else 9
(define (output-jr-jalr s op)
  (define j (bitwise-ior (arithmetic-shift 0 26)
                         (arithmetic-shift s 21)
                         (arithmetic-shift 0 4)
                         op))

  (write-byte (bitwise-and (arithmetic-shift j -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift j -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift j -8) #xff))
  (write-byte (bitwise-and j #xff))

  )

;(output-add-sub-slt-sltu  op) outputs binary encoding of sub or add $d, $s, $t
; op: 32 if add, 34 if sub, 42 if slt and 43 if sltu
(define (output-add-sub-slt-sltu reg-list op)
  (define d (first reg-list))
  (define s (second reg-list))
  (define t (third reg-list))
  
  (define word (bitwise-ior (arithmetic-shift 0 26)
                            (arithmetic-shift s 21)
                            (arithmetic-shift t 16)
                            (arithmetic-shift d 11)
                            op))

  (write-byte (bitwise-and (arithmetic-shift word -24) #xff)) 
  (write-byte (bitwise-and (arithmetic-shift word -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift word -8) #xff))
  (write-byte (bitwise-and word #xff))

  )

;(output-lis-mflo-mfhi d op) outputs binary encoding of lis/mflo/mfhi $d
; op: 20 if lis, 18 if mflo, 16 if mfhi
(define (output-lis-mflo-mfhi d op)
  (define lis-mfhi-lo (bitwise-ior (arithmetic-shift 0 26)
                                   (arithmetic-shift d 11)
                                   op))
  (write-byte (bitwise-and (arithmetic-shift lis-mfhi-lo -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift lis-mfhi-lo -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift lis-mfhi-lo -8) #xff))
  (write-byte (bitwise-and lis-mfhi-lo  #xff))
  )

;(output-mult-div-u s t op) outputs binary encoding of div/divu/mult/multu $d
; op: 24 if mult, 25 if multu, 26 if div, 27 if divu
(define (output-mult-div-u reg-list op)
  (define s (first reg-list))
  (define t (second reg-list))
  (define mult-div (bitwise-ior (arithmetic-shift 0 26)
                                (arithmetic-shift s 21)
                                (arithmetic-shift t 16)
                                op))

  (write-byte (bitwise-and (arithmetic-shift mult-div -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift mult-div -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift mult-div -8) #xff))
  (write-byte (bitwise-and mult-div  #xff))

  
  )

;(output-sw-lw reg-list op) outputs binary encoding of sw/lw
; op: 35 if lw, 43 if sw
(define (output-lw-sw reg-list op)
  (define s (third reg-list))
  (define i (second reg-list))
  (define t (first reg-list))
  
  (define lwsw (bitwise-ior (arithmetic-shift op 26)
                            (arithmetic-shift s 21)
                            (arithmetic-shift t 16)
                            (bitwise-and i #xffff)))

  (write-byte (bitwise-and (arithmetic-shift lwsw -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift lwsw -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift lwsw -8) #xff))
  (write-byte (bitwise-and lwsw #xff))
  )

;(output-op id) outputs the opcode of the instruction (id)
(define (output-op id)
  (cond [(symbol=? id 'add) 32]
        [(symbol=? id 'sub) 34]
        [(symbol=? id 'jr) 8]
        [(symbol=? id 'jalr) 9]
        [(symbol=? id 'slt) 42]
        [(symbol=? id 'sltu) 43]
        [(symbol=? id 'beq) 4]
        [(symbol=? id 'bne) 5]
        [(symbol=? id 'lis) 20]
        [(symbol=? id 'mflo) 18]
        [(symbol=? id 'mfhi) 16]
        [(symbol=? id 'mult) 24]
        [(symbol=? id 'multu) 25]
        [(symbol=? id 'div) 26]
        [(symbol=? id 'divu) 27]
        [(symbol=? id 'lw) 35]
        [(symbol=? id 'sw) 43]
        ))

; (output-instr-all instr-list) outputs all the binary encoding in instr-list
; instr-list is a list of instr
(define (output-instr-all instr-list lab-li)
	(cond 
		[(empty? instr-list) (void)]
                ;outputs add,sub,slt,sltu
                [(or (symbol=? (instr-id (first instr-list)) 'add)
                     (symbol=? (instr-id (first instr-list)) 'sub)
                     (symbol=? (instr-id (first instr-list)) 'slt)
                     (symbol=? (instr-id (first instr-list)) 'sltu))
                 (output-add-sub-slt-sltu (instr-value (first instr-list)) (output-op (instr-id (first instr-list))))
                 (output-instr-all (rest instr-list) lab-li)]

                ;outputs jr, jalr
                [(or (symbol=? (instr-id (first instr-list)) 'jr)
                     (symbol=? (instr-id (first instr-list)) 'jalr))
                 (output-jr-jalr (instr-value (first instr-list)) (output-op (instr-id (first instr-list))))
                 (output-instr-all (rest instr-list) lab-li)]

                ;outputs lis,mflo,mfhi
                [(or (symbol=? (instr-id (first instr-list)) 'lis)
                     (symbol=? (instr-id (first instr-list)) 'mflo)
                     (symbol=? (instr-id (first instr-list)) 'mfhi))
                 (output-lis-mflo-mfhi (instr-value (first instr-list)) (output-op (instr-id (first instr-list))))
                 (output-instr-all (rest instr-list) lab-li)]
                
                ;outputs mult,multu,div,divu
                [(or (symbol=? (instr-id (first instr-list)) 'mult)
                     (symbol=? (instr-id (first instr-list)) 'multu)
                     (symbol=? (instr-id (first instr-list)) 'div)
                     (symbol=? (instr-id (first instr-list)) 'divu))
                 (output-mult-div-u (instr-value (first instr-list)) (output-op (instr-id (first instr-list))))
                 (output-instr-all (rest instr-list) lab-li)]

                ;outputs lw,sw
                [(or (symbol=? (instr-id (first instr-list)) 'lw)
                     (symbol=? (instr-id (first instr-list)) 'sw))
                 (output-lw-sw (instr-value (first instr-list)) (output-op (instr-id (first instr-list))))
                 (output-instr-all (rest instr-list) lab-li)]

                
                ;outputs beq,bne
                [(or (symbol=? (instr-id (first instr-list)) 'beq) (symbol=? (instr-id (first instr-list)) 'bne))
                 (define inst (instr-value (first instr-list)))
                 (define s (instr-value (first inst)))
                 (define t (instr-value (second inst)))
                 (define i-id (instr-id (third inst)))
                 (define i-val (instr-value (third inst)))

                 (cond [(symbol=? i-id 'id) ;check if i is a label
                        (define i (/ (- (getLabelValue i-val lab-li) 4 (instr-value (fourth inst))) 4)) ; encodes i
                        (cond [(isValid-i? (token 'int i) 'beq)
                               (output-beq-bne-i s t i (output-op (instr-id (first instr-list))))])
                        
                        ]
                       [(or (symbol=? i-id 'int) (symbol=? i-id 'hexint))
                        (output-beq-bne-i s t i-val (output-op (instr-id (first instr-list))))] ; i is an int or hexint
                       [else (error 'ERROR)])


                 (output-instr-all (rest instr-list) lab-li)]

                ; outputs .word
		[(symbol=? (instr-id (first instr-list)) 'wordLab)
                 (output-word-i (getLabelValue (instr-value (first instr-list)) lab-li))
                 (output-instr-all (rest instr-list) lab-li)]
                
		[(symbol=? (instr-id (first instr-list)) 'wordVal)
                 (output-word-i (instr-value (first instr-list)))
                 (output-instr-all (rest instr-list) lab-li)]

                [else (error 'ERROR)]
                )

  
  
)	

;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;(symbol-table lab-li) outputs the symbol table to error
(define (symbol-table label-list)
	(fprintf (current-error-port)  (symbol-list label-list)))

;symbol-list combines the key and values of label-list into one string
(define (symbol-list labelnames)
  (cond [(empty? labelnames) ""]
	[(equal? (length labelnames) 1) (string-append (list->string (label-key (first labelnames)))
                             " "
                             (number->string (label-value (first labelnames))))]
        [else (string-append (list->string (label-key (first labelnames)))
                             " "
                             (number->string (label-value (first labelnames)))
                             (list->string '(#\newline))
                             (symbol-list (rest labelnames)))
              
              ]))

; (scan-file n) reads the whole file and outputs the binary code
(define (scan-file n lab-li instr-list)
  (define line (read-line))
  (cond
    [(eof-object? line)
	(output-instr-all (reverse instr-list) lab-li)
	(symbol-table lab-li)]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
	
        (cond
          [(empty? scanned) (scan-file n lab-li instr-list)] ; skips empty line
          
          [(not (isLabel? scanned)) 
		; increments n if scanned is an instruction
                (scan-file (+ n 1) lab-li (cons (IsInstr? scanned n)  instr-list))] 

          [(isLabel? scanned)
		(define label-name-n (is-valid-label? scanned lab-li n instr-list))
		(define label-name-li (first label-name-n))
		(define updated-n (second label-name-n))
		(define updated-instr-list (third label-name-n))
		
		(scan-file updated-n label-name-li updated-instr-list)		
		
		]
	  [else (error 'ERROR)] ; not a valid instruction
)]
    [else (scan-file n lab-li instr-list)]))

(scan-file 0 empty empty)



















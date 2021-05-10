; ----- Eval -----

; evaluate evaluates the provided expression expr in the given environment env
(define (evaluate expr env)
  (cond ((self-evaluating? expr) expr)
	((variable? expr)	(lookup-variable-value expr env))
	((quoted? expr)		(text-of-quotation expr))
	((assignment? expr)	(eval-assignment expr env))
	((definition? expr)	(eval-definition expr env))
	((if? expr)		(eval-if expr env))
	((lambda? expr)		(make-procedure (lambda-parameters expr) (lambda-body expr) env))
	((begin? expr)		(eval-sequence (begin-actions expr) env))
	((cond? expr)		(evaluate (cond->if expr) env))
	((or? expr)		(eval-or (or-premises expr) env))
	((and? expr)		(eval-and (and-premises expr) env))
	((let? expr)		(evaluate (let->combination expr) env))
	((let*? expr)		(evaluate (let*->nested-lets expr) env))
	((letrec? expr)		(evaluate (letrec->let expr) env))
	((do? expr)		(evaluate (do->let expr) env))
	((while? expr)		(evaluate (while->do expr) env))
	((until? expr)		(evaluate (until->do expr) env))
	((application? expr)	(do-apply (evaluate (operator expr) env) (list-of-values (operands expr) env)))
	(else
	  (error "Unknown expression type -- EVAL" expr))))

; Evaluates a list of values
; Used to reduce application arguments to evaluatable form
(define (list-of-values exprs env)
  (if (no-operands? exprs)
    '()
    (cons (evaluate (first-operand exprs) env)
	  (list-of-values (rest-operands exprs) env))))


; Evaluates the conditional if statement
; Checks the environment to see if the predicate is true
; If so, then evaluates the consequent
; Otherwise evalues the alternative
(define (eval-if expr env)
  (if (true? (evaluate (if-predicate expr) env))
    (evaluate (if-consequent expr) env)
    (evaluate (if-alternative expr) env)))

; Evaluates a sequence of expressions
(define (eval-sequence exprs env)
  (cond ((last-expr? exprs) (evaluate (first-expr exprs) env))
	(else (evaluate (first-expr exprs) env)
	      (eval-sequence (rest-exprs exprs) env))))

; Sets a variable value in the specified environment
(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
		       (evaluate (assignment-value expr) env)
		       env)
  'ok)

; Defines a new variable in the specified environment
(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
		    (evaluate (definition-value expr) env)
		    env)
  'ok)

; ----- Analysis -----

;; Separate into analysis and execution
;(define (evaluate expr env) ((analyze expr) env))
;
;(define (analyze expr)
;  (cond ((self-evaluating? expr)	(analyze-self-evaluating expr))
;	((quoted? expr)			(analyze-quoted expr))
;	((variable? expr)		(analyze-variable expr))
;	((assignment? expr)		(analyze-assignment expr))
;	((definition? expr)		(analyze-definition expr))
;	((if? expr)			(analyze-if expr))
;	((lambda? expr)			(analyze-lambda expr))
;	((begin? expr)			(analyze-sequence (begin-actions expr)))
;	((cond? expr)			(analyze (cond->if expr)))
;	((let? expr)			(analyze (let->combination expr)))
;	((application? expr)		(analyze-application expr))
;	(else
;	  (error "Unknown expression type -- ANALYZE" expr))))
;
;(define (analyze-self-evaluating expr)
;  (lambda (env) expr))
;
;(define (analyze-quoted expr)
;  (let ((qval (text-of-quotation expr)))
;    (lambda (env) qval)))
;
;(define (analyze-variable expr)
;  (lambda (env) (lookup-variable-value expr env)))
;
;(define (analyze-assignment expr)
;  (let ((var (assignment-variable expr))
;	(vproc (analyze (assignment-value expr))))
;    (lambda (env)
;      (set-variable-value! var (vproc env) env)
;      'ok)))
;
;(define (analyze-definition expr)
;  (let ((var (definition-variable expr))
;	(vproc (analyze (definition-value expr))))
;    (lambda (env)
;      (define-variable! var (vproc env) env)
;      'ok)))
;
;(define (analyze-if expr)
;  (let ((pproc (analyze (if-predicate expr)))
;	(cproc (analyze (if-consequent expr)))
;	(aproc (analyze (if-alternative expr))))
;    (lambda (env)
;      (if (true? (pproc env))
;	(cproc env)
;	(aproc env)))))
;
;(define (analyze-lambda expr)
;  (let ((vars (lambda-parameters expr))
;	(bproc (analyze-sequence (lambda-body expr))))
;    (lambda (env) (make-procedure vars bproc env))))
;
;(define (analyze-sequence exprs)
;  (define (sequentially proc1 proc2)
;    (lambda (env) (proc1 env) (proc2 env)))
;  (define (loop first-proc rest-procs)
;    (if (null? rest-procs)
;      (lambda (env) (first-proc env))
;      (loop (sequentially first-proc (car rest-procs))
;	    (cdr rest-procs))))
;  (let ((procs (map analyze exprs)))
;    (if (null? procs)
;      (error "Empty sequence -- ANALYZE"))
;    (loop (car procs) (cdr procs))))
;
;(define (analyze-application expr)
;  (let ((fproc (analyze (operator expr)))
;	(aprocs (map analyze (operands expr))))
;    (lambda (env)
;      (execute-application (fproc env)
;			   (map (lambda (aproc) (aproc env)) aprocs)))))
;
;(define (execute-application proc args)
;  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
;	((compound-procedure? proc) ((procedure-body proc)
;				     (extend-environment
;				       (procedure-parameters proc)
;				       args
;				       (procedure-environment proc))))
;	(else
;	  (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))


; ----- Apply -----

; do-apply applies a procedure to a list of arguments

(define (do-apply procedure arguments)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure) (eval-sequence
					   (procedure-body procedure)
					   (extend-environment (procedure-parameters procedure)
							       arguments
							       (procedure-environment procedure))))
	(else (error "Unknown procedure type -- APPLY" procedure))))

; A procedure is a compound expression built from various (possibly built-in)
; expressions.
(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; Compound procedures
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Primitive procedures
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	;(list '/ /)
	;(list 'assoc assoc)
	;(list 'cadr cadr)
	(list 'display display)
	;(list '> >)
	(list '< <)
	;(list 'newline newline)
	;(list 'not not)
	(list '= =)
	(list 'list list)
	(list 'append append)
	))

(define primitive-procedure-names (map car primitive-procedures))
(define primitive-procedure-objects (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

; The underlying scheme apply is just the keyword apply (which we didn't redefine)
(define apply-in-underlying-scheme apply)

; ----- Environment -----

; - Exercise 4.11
;	- Frame as a list of bindings
; Have to rewrite the whole frame manipulation interface because of scheme's mutability
; issues. 

; Frames are a list of bindings (variables and values)
(define (make-binding var val) (list var val))
(define (make-frame vars vals)
  (if (null? vars)
    '()
    (cons (make-binding (car vars) (car vals)) (make-frame (cdr vars) (cdr vals)))))
(define (add-binding-to-frame! var val frame)
  (if (null? (cdr frame))
    (set-cdr! frame (list (make-binding var val)))
    (add-binding-to-frame! var val (cdr frame))))

(define (scan-loop env target bindings found missing)
  (if (null? bindings)
    (missing env)
    (let ((binding (car bindings)))
      (if (eq? target (car binding))
	(found binding)
	(scan-loop env target (cdr bindings) found missing)))))

(define (scan-frame env target found missing)
  (let ((frame (first-frame env)))
    (scan-loop env target frame found missing)))

(define (env-loop env target found miss)
  (if (eq? env the-empty-environment)
    (miss target)
    (scan-frame env target found (lambda (e) (env-loop (enclosing-environment e) target found miss)))))

(define (lookup-variable-value var env)
  (env-loop
    env
    var
    (lambda (binding) (cadr binding))
    (lambda (t) (error "Unbound variable - LOOKUP" t))))
(define (set-variable-value! var val env)
  (env-loop
    env
    var
    (lambda (binding) (set-cdr! binding (list val)))
    (lambda (t) (error "Unbound variable - SET!" t))))
(define (define-variable! var val env)
  (scan-frame
    env
    var
    (lambda (binding) (set-cdr! binding (list val)))
    (lambda (env) (add-binding-to-frame! var val (first-frame env)))))

;	- Frame as a pair of lists of variables and values

;; Frame are a pair of lists of variables and values
;(define (make-frame vars vals) (cons vars vals))
;(define (frame-variables frame) (car frame))
;(define (frame-values frame) (cdr frame))
;(define (add-binding-to-frame! var val frame)
;  (set-car! frame (cons var (car frame)))
;  (set-cdr! frame (cons val (cdr frame))))
;
;; Helper function which loops over a frame
;(define (scan-loop env target vars vals found missing)
;  (cond ((null? vars) (missing env))
;	((eq? target (car vars)) (found vals))
;	(else (scan-loop env target (cdr vars) (cdr vals) found missing))))
;
;; Scans a frame for a target variable in the given environment
;; If the target was found, then the found function is called with a list of values
;; If the target wasn't found, then the missing function is called
;(define (scan-frame env target found missing)
;  (let ((frame (first-frame env)))
;    (scan-loop env target (frame-variables frame) (frame-values frame) found missing)))
;
;; Scans all the frames in the environment for the target and calls the found function
;; if the target variable is found
;(define (env-loop env target found miss)
;  (if (eq? env the-empty-environment)
;    (miss target)
;    (scan-frame env target found (lambda (e) (env-loop (enclosing-environment e) target found miss)))))
;
;; Get the value of the variable
;(define (lookup-variable-value var env)
;  (env-loop env var (lambda (vals) (car vals)) (lambda (t) (error "Unbound variable - LOOKUP" t))))
;
;; Set the value of the variable
;(define (set-variable-value! var val env)
;  (env-loop env var (lambda (vals) (set-car! vals val)) (lambda (t) (error "Unbound variable - SET!" t))))
;
;; Defines the variable in the local frame
;(define (define-variable! var val env)
;  (scan-frame
;    env
;    var
;    (lambda (vals) (set-car! vals val))
;    (lambda (env) (add-binding-to-frame! var val (first-frame env)))))
;

; ----- Environment ----- 

; An environment is a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Add a frame with the specified variables and values
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))


; Initial setup environment containing primitives
(define (setup-environment)
  (let ((initial-env (extend-environment primitive-procedure-names
					 primitive-procedure-objects
					 the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

; ----- Syntax -----

; Number and strings are self-evaluated already
(define (self-evaluating? expr)
  (cond ((number? expr) true)
	((string? expr) true)
	(else false)))

; Variables are symbols
(define (variable? expr) (symbol? expr))

; Identifies if the expression begins with the specified tag
(define (tagged-list? expr tag)
  (if (pair? expr)
    (eq? (car expr) tag)
    false))

; Quotations are tagged with quote
(define (quoted? expr) (tagged-list? expr 'quote))
(define (text-of-quotation expr) (cadr expr))

; Assignments are tagged with set!
(define (assignment? expr) (tagged-list? expr 'set!))
(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))

; Definitions are tagged with define
(define (definition? expr) (tagged-list? expr 'define))

; Defintion variable is either symbol or lambda name
(define (definition-variable expr)
  (if (symbol? (cadr expr))
    (cadr expr)
    (caadr expr)))

; Definition value is either symbol or lambda function
(define (definition-value expr)
  (if (symbol? (cadr expr))
    (caddr expr)
    (make-lambda (cdadr expr)
		 (cddr expr))))

; Lambda expressions are tagged by lambda
(define (lambda? expr) (tagged-list? expr 'lambda))
(define (lambda-parameters expr) (cadr expr))
(define (lambda-body expr) (cddr expr))

; Explicitly creates a lambda for evaluation
(define (make-lambda params body) (cons 'lambda (cons params body)))

; Conditionals are tagged by if
(define (if? expr) (tagged-list? expr 'if))
(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))
(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
    (cadddr expr)
    'false))

; Convert cond into if-predicate for evaluation
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Sequences are tagged by begin
(define (begin? expr) (tagged-list? expr 'begin))
(define (begin-actions expr) (cdr expr))
(define (last-expr? seq) (null? (cdr seq)))
(define (first-expr seq) (car seq))
(define (rest-exprs seq) (cdr seq))

; Convert a sequence into a single expression
(define (sequence->expr seq)
  (cond ((null? seq) seq)
	((last-expr? seq) (first-expr seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; Conditionals are taggged by cond
; Else clause of a conditional tagged by else
(define (cond? expr) (tagged-list? expr 'cond))
(define (cond-clauses expr) (cdr expr))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if expr) (expand-clauses-test (cond-clauses expr)))

; Expand cond clauses into if statements by chaining
; if-else expressions
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (if (cond-else-clause? first)
	(if (null? rest)
	  (sequence->expr (cond-actions first))
	  (error "ELSE clause isn't last -- COND->IF" clauses))
      	(make-if (cond-predicate first)
		 (sequence->expr (cond-actions first))
		 (expand-clauses rest))))))

; Anything not explicitly false is true
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; ----- REPL Driver -----

; Prompt strings
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

; REPL driver loop
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaluate input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input str) (newline) (newline) (display str) (newline))
(define (announce-output str) (newline) (display str) (newline))
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
		   (procedure-parameters object)
		   (procedure-body object)
		   '<procedure-env>))
    (display object)))



; ----- Exercises ----- 

; - Exercise 4.1
; Left-to-right list-of-values
; Can create right-to-left by swapping the order of appends.
;(define (lor-list-of-values expr env)
;  (if (no-operands? exprs)
;    '()
;    (let* ((first (evaluate (first-operand exprs) env))
;	   (rest (lor-list-of-values (rest-operands exprs) env)))
;      (cons first rest))))



; - Exercise 4.2
; a) The predicate application? acts as the default behavior of an expression
; if it doesn't match an existing built-in behavior. Therefore evaluating
; it before other built-in methods will cause either a loop or failure to
; evaluate the methods properly (which is an issue when built-in methods
; such as define are dependent on order of execution).
; b) explicit application tagged by call
; also make sure that the application is a pair after call (as with normal
; application?)
(define (application-explicit? expr) (and (tagged-list? expr 'call) (pair? (cdr expr))))


; - Exercise 4.3
; Data-dispatch for the evaluator has same form as data-dispatch for
; symbolic differentiation.
;(define (evaluate-data expr env)
;  (cond ((self-evaluating? expr) expr)
;	((variable? expr) (lookup-variable-value expr env))
;	(else ((get 'evaluate-data (operator expr)) (operand expr) env))))

; - Exercise 4.4
; Special form representation
(define (eval-and expr env)
  (cond ((null? expr) 'true)
	((true? (evaluate (and-first expr) env)) (eval-and (and-rest expr) env))
	(else 'false)))

(define (and? expr) (tagged-list? expr 'and))
(define (and-premises expr) (cdr expr))
(define (and-first expr) (car expr))
(define (and-rest expr) (cdr expr))

(define (eval-or expr env)
  (cond ((null? expr) 'false)
	((true? (evaluate (or-first expr) env)) 'true)
	(else (eval-or (or-rest expr) env))))

(define (or? expr) (tagged-list? expr 'or))
(define (or-premises expr) (cdr expr))
(define (or-first expr) (car expr))
(define (or-rest expr) (cdr expr))

; Derived form
;(define (eval-and premises env) (evaluate (and->if premises) env))
;(define (eval-or premises env) (evaluate (or->if premises) env))
;(define (and->if premises)
;  (if (null? premises)
;    'true
;    (let ((first (and-first premises))
;	  (rest (and-rest premises)))
;      (make-if first (and->if rest) 'false))))
;
;(define (or->if premises)
;  (if (null? premises)
;    'false
;    (let ((first (or-first premises))
;	  (rest (or-rest premises)))
;      (make-if first 'true (or->if rest)))))


; - Exercise 4.5
; Syntax: (<test> => <recipient>)
(define (expand-clauses-test clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (cond ((cond-else-clause? first) 
	     (if (null? rest)
	       (sequence->expr (cond-actions first))
	       (error "ELSE clause isn't last -- COND-IF" clauses)))
	    ((cond-test-clause? first)
	     (make-if (cond-test-predicate first)
		      (list (cond-test-recipient first) (cond-test-predicate first))
		      (expand-clauses rest)))
	    (else
	      (make-if (cond-predicate first)
		       (sequence->expr (cond-actions first))
		       (expand-clauses rest)))))))
(define (cond-test-clause? expr) (eq? (cadr expr) '=>))
(define (cond-test-predicate expr) (car expr))
(define (cond-test-recipient expr) (caddr expr))

;; - Exercise 4.6
(define (let? expr) (tagged-list? expr 'let))
(define (let->combination expr) (cons (let-lambda expr) (let-arguments expr)))
(define (let-lambda expr) (make-lambda (let-parameters expr) (let-body expr)))
(define (let-parameters expr) (map car (let-bindings expr)))
(define (let-arguments expr) (map cadr (let-bindings expr)))
(define (let-bindings expr) (cadr expr))
(define (let-body expr) (cddr expr))

;; - Exercise 4.7
(define (let*->nested-lets expr)
  (if (let*-last-binding? expr)
    (make-let (list (let*-binding-first expr)) (let*-body expr))
    (make-let (list (let*-binding-first expr))
	      (list (let*->nested-lets (make-let* (let*-binding-rest expr) (let*-body expr)))))))

(define (let*-last-binding? expr) (null? (let*-binding-rest expr)))
(define (make-let bindings body) (append (list 'let bindings) body))
(define (make-let* bindings body) (append (list 'let* bindings) body))
(define (let*? expr) (tagged-list? expr 'let*))
(define (let*-binding-first expr) (caadr expr))
(define (let*-binding-rest expr) (cdadr expr))
(define (let*-body expr) (cddr expr))

; - Exercise 4.8

(define (let->combination expr)
  (if (let-has-name? expr)
    (make-begin (list (make-define (cons (let-name expr) (let-parameters expr)) (let-body expr))
	  (cons (let-name expr) (let-arguments expr))))
    (cons (let-lambda expr) (let-arguments expr))))

(define (make-define name body) (append (list 'define name) body))
(define (let-has-name? expr) (variable? (cadr expr)))
(define (let-name expr) (cadr expr))
(define (let-bindings expr) (if (let-has-name? expr) (caddr expr) (cadr expr)))
(define (let-body expr) (if (let-has-name? expr) (cdddr expr) (cddr expr)))


; - Exercise 4.9

; (do ((variable init [step]) ...) (test expr ...) body ...)
(define (do->let expr)
  (make-let (do-initials expr)
	    (make-if (do-test expr)
		     (do-return expr)
		     (make-begin (append (do-body expr)
					 (list (make-do (do-step expr)
							(do-test-return expr)
							(do-body expr))))))))

(define (do-binding-var binding) (car binding))
(define (do-binding-init binding) (cadr binding))
(define (do-binding-step binding) (caddr binding))
(define (do-bindings expr) (cadr expr))
(define (do-test-return expr) (caddr expr))
(define (do-body expr) (cdddr expr))

(define (do-initials expr)
  (map (lambda (binding) (list (do-binding-var binding) (do-binding-init binding))) (do-bindings expr)))

(define (do-test expr) (car (do-test-return expr)))
(define (do-return expr) (make-begin (cdr (do-test-return expr)))
  (let ((ret (cdr (do-test-return expr))))
    (if (null? ret)
      '(quote ok)
      (make-begin ret))))

(define (do-step expr)
  (map (lambda (binding) (list (do-binding-var binding) (do-binding-step binding) (do-binding-step binding)))
       (do-bindings expr)))
(define (do? expr) (tagged-list? expr 'do))
(define (make-do bindings test-ret body) (append (list 'do bindings test-ret) body))

; (until <cond> <body>)
(define (until->do expr) (make-do () (list (while-cond expr) 'true) (until-body expr)))
(define (until-cond expr) (cadr expr))
(define (until-body expr) (cddr expr))
(define (until? expr) (tagged-list? expr 'until))

; (while <cond> <body>)
(define (while->do expr) (make-do () (list (make-not (while-cond expr))) (while-body expr)))
(define (while-cond expr) (cadr expr))
(define (while-body expr) (cddr expr))
(define (while? expr) (tagged-list? expr 'while))
(define (make-not expr) (list 'not expr))

; - Exercise 4.10
; Can edit syntax in the syntax section. For example, we may define tagged-list? so that the tag
; must occur at the end of the list. This would change the syntax so list such as
; 	(if (> count 0) count (- count 1))
; to
;	((count 0 >) count (count 1 -) if)

; - Exercise 4.11
; See the environment section

; - Exercise 4.12
; See the environment section

; - Exercise 4.13
; While its straightforward to remove a variable from a frame, it completely disrupts the entire
; structure because of having empty frames / environments. Fixing that issue would require changing
; a lot of other functions which I can't be bothered right now.

; - Exercise 4.14
; Louis' map attempts to apply evaluated functions to lists of data. These evaluated functions are
; tagged as "procedure"s in our evaluator, so the map tries to apply procedure, which it doesn't
; know how to. On the other hand, a map function defined in the evaluator is just a standard
; definition so it will work as expected.

; - Exercise 4.15
; Consider (try try). Suppose try halts. Then (try try) evaluates (run-forever), so try is a non-halting
; function, and this is a contradiction. Now suppose try doesn't halt. Then (try try) evaluates to
; halted, and is thus a halting function. Again this a contradiction. Thus halt? is not well defined.

; - Exercise 4.16
; Frame as list of variables and list of values
;(define (lookup-variable-value var env)
;  (env-loop env var
;	       (lambda (vals)
;		 (let ((val (car vals)))
;		   (if (eq? val '*unassigned*)
;		     (error "Value is unassigned" val)
;		     val)))
;	       (lambda (t) (error "Unbound variable - LOOKUP NEW" t))
;	       ))

; Frame as list of bindings
(define (lookup-variable-value var env)
  (env-loop
    env
    var
    (lambda (binding)
      (let ((val (cadr binding)))
	(if (eq? val '*unassigned*)
	  (error "Value is unassigned" val)
	  val)))
    (lambda (t) (error "Unbound variable - LOOKUP NEW" t))))

;(define (scan-out-defines body)
;  (if (scan-out-has-defines body)
;    (let ((sets (scan-out-sets body)))
;      (list (make-let (scan-out-bindings sets) (append sets (scan-out-reduced-body body)))))
;    body))

(define (scan-out-bindings sets)
  (if (null? sets)
    '()
    (cons (scan-out-binding-from-set (car sets)) (scan-out-bindings (cdr sets)))))

(define (scan-out-binding-from-set set)
  (list (cadr set) '*unassigned*))

(define (scan-out-sets body)
  (cond ((null? body) '())
	((definition? (car body)) (cons (scan-out-make-set (car body)) (scan-out-sets (cdr body))))
	(else (scan-out-sets (cdr body)))))

(define (scan-out-reduced-body body)
  (cond ((null? body) '())
	((definition? (car body)) (scan-out-reduced-body (cdr body)))
	(else (cons (car body) (scan-out-reduced-body (cdr body))))))

(define (scan-out-has-defines body) (not (null? (scan-out-sets body))))
(define (scan-out-make-set def) (list 'set! (definition-variable def) (definition-value def)))

;(define (procedure-body procedure)
;  (let ((body (caddr procedure)))
;    (scan-out-defines body)))

; Add unassigned to the global namespace (undefined behavior)
(define-variable! '*unassigned* 0 the-global-environment)


; - Exercise 4.17
; A new frame is added via the extend-environment function, which is called whenever parameters
; are applied to a compound procedure. This is necessary to define a new local frame for the
; function to execute in, with the parameters as variables in this new local frame. Thus when
; a let function is called, it is transformed into a lambda called with parameters given by
; the bindings. This introduces a new local frame where the defines are local to said frame.
;
; To maintain simultaneity without constructing an extra frame, when parsing out the define
; clauses directly define the variables in the current frame (with define-variable!). Then
; the sequence of sets and final expression will be simultaneous without an extra frame.

;(define (do-apply procedure arguments)
;  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
;	((compound-procedure? procedure)
;	 (let ((local-environment (extend-environment (procedure-parameters procedure)
;						      arguments
;						      (procedure-environment procedure))))
;	   (eval-sequence (procedure-body procedure local-environment) local-environment)))
;	(else (error "Unknown procedure type -- APPLY" procedure))))
;
;(define (procedure-body proc env)
;  (let ((body (caddr proc)))
;    (scan-out-defines body env)))
;
;(define (scan-out-defines body env)
;  (if (scan-out-has-defines body)
;    (begin
;      (scan-out-define-variables body env)
;      (append (scan-out-sets body) (scan-out-reduced-body body)))
;    body))
;
;(define (scan-out-define-variables body env)
;  (cond ((null? body) 'ok)
;	((definition? (car body)) (begin
;				    (define-variable! (definition-variable (car body)) '*unassigned* env)
;				    (scan-out-define-variables (cdr body) env)))
;	(else (scan-out-define-variables (cdr body) env))))

; - Exercise 4.18
; In both strategies, u and v are added to the local frame and may be referenced by the following
; procedures. The original expression may evaluate u first then v, which allows v to be dependent on u.
; However, this breaks simultaneity since it depends on the order of evaluation.
; The alternative implementation allows the desired expression <e3> or a let statement to be evaluated
; simultaneously. Evaluating the let will cause the definitions of u and v (<e1> and <e2> respectively)
; to be evaluated and stored in a and b, resp. If <e1> or <e2> depend on u or v, then they won't
; evaluate to a valid value since u and v are undefined during this evaluation. Thus it enforces the
; restriction that u and v can't depend on each other.
;
; The solve procedure will therefore not work for the alternative implementation (y and dy depend on
; each other), while it will work for the original implementation.

; - Exercise 4.19
; I agree with Alyssa. To preserve the operating principles of the language, simultenaity should be
; maintained. It's a rather ad hoc choice to define an ordering of the definitions.
; Eva's method becomes ambiguous if there are two definitions of the same variable in the same scope.
; Suppose you have (define a 5) (define a 6) on the same level. This is impossible to be done
; simultaneously in the manner Eva describes. Perhaps you could check to make sure a definition is
; only used once, but I think it would introduce many bugs for little gain.

; - Exercise 4.20

(define (letrec->let expr)
  (let ((bindings (letrec-bindings expr)))
    (make-let (letrec-initial-bindings bindings) (append (letrec-sets bindings) (letrec-body expr)))))

(define (letrec? expr) (tagged-list? expr 'letrec))

(define (letrec-bindings expr) (cadr expr))

(define (letrec-initial-bindings bindings)
  (if (null? bindings)
    '()
    (let ((binding (car bindings))
	  (rest (cdr bindings)))
      (cons (list (car binding) '*unassigned*) (letrec-initial-bindings rest)))))

(define (letrec-sets bindings)
  (if (null? bindings)
    '()
    (let ((binding (car bindings))
	  (rest (cdr bindings)))
      (cons (list 'set! (car binding) (cadr binding)) (letrec-sets (cdr bindings))))))

(define (letrec-body expr) (cddr expr))

; b) When let attempts to bind even? it will try to lookup odd? and be unable to find it. Similarly if
; it tries to bind odd? it will try to lookup even? and also fail. Neither are entered into the
; local frame at the time of evaluation because their bodies need to be evaluated in order to bind
; them to the frame. Thus either using defines or letrec will define even? and odd? in the local frame,
; which may then be referenced in evaluating their bodies and thus successfully bound. 
;
; Turns out you can in fact do recursion via the Y-operator in lambda calculus! (Although using just
; let in this case still won't give the correct behavior). 

; - Exercise 4.21

; a) Fibonacci implementation
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (f k)
      (if (< k 2)
	1
	(+ (f f (- k 1)) (f f (- k 2)))))))
 10)

; b) 

((lambda (even? odd? x)
   (even? even? odd? x))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 10)

; - Exercise 4.22

; See analysis method.
; Implemented as derived function.

; - Exercise 4.23

; The version of the text performs a greater amount of analysis of the expression. It fully creates a
; lambda function by chaining together the procedure sequence via the sequentially method. The result
; is a single lambda function fully independent of the analysis machinery.
; Alyssa's version gives a lambda which wraps the execute-sequence function. This function loops
; through and does further analyze calls on the method. Thus it returns a lambda that is still
; dependent on analysis machinery, and not a fully analyzed function.

; - Exercise 4.24

; Using a recursive function to force many evaluation calls with simple primitive procedures:
;
; (define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
; 
; Real time
; n	With Analysis	Without Analysis
; 1	0.156s		0.153s
; 5	0.131s		0.132s
; 10	0.172s		0.183s
; 15	0.338s		0.497s
; 20	2.229s		3.390s
; 25	22.273s		41.398s
; 30	4m8.62s		7m38.2s
;
; We can see that analysis speeds up the program by reducing syntactic evaluation for repeated calls.
; For only a few number of calls, there's little difference (< 15). At ~15 calls we see the speed
; improvement start to become significant.

; -----------------------------------------------------------------------------------------------------

(display "All done!")

#lang racket
(require compatibility/defmacro)

(define version "0.2")

(define arg-compile-only (make-parameter #f))
(define arg-verbose (make-parameter #f))
(define arg-out-filename (make-parameter "a-out"))
(define arg-filenames
  (command-line
   #:once-each
   (("-c" "--compile") "Compile only; do not run"
                       (arg-compile-only #t))
   (("-v" "--verbose") "Display verbose messages"
                       (arg-verbose #t))
   (("-o") file
           "Place the output into <file>"
           (arg-out-filename file))
   #:args filenames
   filenames))

(define code null)
(cond [(empty? arg-filenames)
       (displayln (format "LFGo Compiler ~a" version))
       (displayln "Enter code (EOF when done):")
       (set! code (port->string))]
      [else
       (set! code (string-join (for/list ([f arg-filenames]) (file->string f))))])

(set! code (string-append "(" code ")"))

;(displayln "Code:")
;(displayln code)
(define parsed (read (open-input-string code)))
;(displayln "Parsed:")
;(displayln parsed)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define (macroexpand-1 lst)
  (syntax->datum (expand-to-top-form (eval `(syntax ,lst) ns))))

(define macros (mutable-set))

(define (str-join lst sep)
  (string-join (map ~a lst) sep))

;; (X Y int) => X, Y int
(define (compile-type-arg arg)
  (format "~a ~a" (str-join (drop-right arg 1) ",") (last arg)))

(define (compile-expr e)
  (cond [(and (list? e) (not (empty? e)))
         (if (set-member? macros (first e))
             (compile-expr (macroexpand-1 e))
             (let ([f (first e)])
               (case f
                 ; (define-syntax ...) ; defines a macro
                 [(define-syntax) (let ([id (second e)]) (when (list? id) (set! id (first id))) (set-add! macros id)) (eval e ns) ""]
                 ; (define-syntax-rule (id . pattern) template) ; defines a macro
                 [(define-syntax-rule) (let ([id (first (second e))]) (set-add! macros id)) (eval e ns) ""]
                 ; (defmacro id formals body ...+) ; defines a (non-hygienic) macro id through a procedure that manipulates S-expressions, as opposed to syntax objects.
                 [(defmacro) (set-add! macros (second e)) (eval e ns) ""]
                 ; (import "package" ...) => import "package"; ...
                 [(import) (string-join (for/list ([x (rest e)]) (format "import ~s\n" x)) "")]
                 ; (defn foo ((a b int)) int ...) => func foo(a, b int) int {...}
                 [(defn)
                  (format "func ~a (~a) ~a {\n~a;}"
                          (second e)
                          (let [(args (third e))] (string-join (map compile-type-arg args) ","))
                          (fourth e)
                          (string-join (map compile-expr (drop e 4)) ";\n"))]
                 ; (def a 3 b 4.0 ...) => a := 3; b := 4.0; ...
                 [(def) (string-join (for/list ([i (in-range 1 (length e) 2)]) (format "~a:=~a" (compile-expr (list-ref e i)) (compile-expr (list-ref e (add1 i))))) ";\n")]
                 ; (var VAR TYPE [VAL]) => var VAR TYPE[=VAL] ; declares a variable
                 [(var) (format "var ~a ~a ~a" (compile-expr (second e)) (compile-expr (third e)) (if (<= (length e) 3) "" (~a "=" (compile-expr (fourth e)))))]
                 ; (+ A B C ...) => (A + B + C + ...)
                 [(+ - * / << >>) (string-join (map compile-expr (rest e)) (~a f) #:before-first "(" #:after-last ")")]
                 ; (<- CHAN) => (<- CHAN)
                 ; (<- CHAN VAL) => (CHAN <- VAL)
                 [(<-) (if (= (length e) 2) (format "(<- ~a)" (compile-expr (second e))) (format "(~a <- ~a)" (compile-expr (second e)) (compile-expr (third e))))]
                 ; (++ A) => (++ A) ; unary statements
                 [(++ --) (format "~a~a" (compile-expr (second e)) f)]
                 ; (< A B) => (A < B) ; binary operators
                 [(< <= > >= == != %) (format "(~a ~a ~a)" (compile-expr (second e)) f (compile-expr (third e)))]
                 ; (= A B) => A = B ; binary statements
                 [(= += -= *= /=) (format "~a ~a ~a" (compile-expr (second e)) f (compile-expr (third e)))]
                 ; (and A B) => (A && B) ; binary operators
                 [(and) (format "(~a && ~a)" (compile-expr (second e))  (compile-expr (third e)))]
                 ; (or A B) => (A && B) ; binary operators
                 [(or) (format "(~a || ~a)" (compile-expr (second e))  (compile-expr (third e)))]
                 ; (not A) => (!A)
                 [(not) (format "(!~a)" (compile-expr (second e)))]
                 ; (return A) => return A
                 [(return) (format "return ~a" (compile-expr (second e)))]
                 ; (if TEST THEN [ELSE]) => if (TEST) THEN; [else ELSE]
                 [(if) (if (= (length e) 4) (format "if ~a {~a} else {~a}" (compile-expr (list-ref e 1)) (compile-expr (list-ref e 2)) (compile-expr (list-ref e 3)))
                           (format "if ~a {~a}" (compile-expr (list-ref e 1)) (compile-expr (list-ref e 2))))]
                 ; (when TEST THEN ...) => if TEST {THEN; ...;}
                 [(when) (format "if ~a {\n~a;}" (compile-expr (list-ref e 1)) (string-join (map compile-expr (drop e 2)) ";\n"))]
                 ; (while TEST BODY ...) => for TEST {BODY; ...;}
                 [(while) (format "for ~a {\n~a;}" (compile-expr (list-ref e 1)) (string-join (map compile-expr (drop e 2)) ";\n"))]
                 ; (for INIT TEST STEP BODY ...) => for INIT; TEST; STEP {BODY; ...;}
                 [(for) (format "for ~a; ~a; ~a {\n~a;}" (compile-expr (list-ref e 1)) (compile-expr (list-ref e 2)) (compile-expr (list-ref e 3)) (string-join (map compile-expr (drop e 4)) ";\n"))]
                 ; (do BODY ...) => {BODY; ...;}
                 [(do) (format "{~a;}" (string-join (map compile-expr (rest e)) ";\n"))]
                 ; (dot A FIELD) => A.FIELD
                 [(dot) (format "~a.~a" (compile-expr (second e)) (compile-expr (third e)))]
                 ; (comma A B ...) => A, B, ...
                 [(comma) (string-join (map compile-expr (rest e)) ",")]
                 ; (at ARRAY [POSITION]) => ARRAY[[POSITION]]
                 [(at) (format "~a[~a]" (compile-expr (second e))
                               (if (= (length e) 3) (compile-expr (third e))
                                   ""))]
                 ; (main BODY ...) => func main() {BODY; ...; return 0;}
                 [(main) (format "func main() {\n~a;}" (string-join (map compile-expr (rest e)) ";\n"))]
                 ; (label ID) => ID:
                 [(label) (format "~a:" (second e))]
                 ; (goto ID) => goto ID
                 [(goto) (format "goto ~a" (second e))]
                 ; (switch EXPR BODY ...) => switch EXPR {BODY; ...;}
                 [(switch) (format "switch ~a {\n~a;}" (compile-expr (second e)) (string-join (map compile-expr (drop e 2)) ";\n"))]
                 ; (case EXPR ...) => case EXPR: case ...:
                 [(case) (string-join (for/list ([x (rest e)]) (format "case ~a:" x)))]
                 ; (default) => default:
                 [(default) "default:"]
                 ; (fn ((a b int)) int (return (+ a b))) => func (a, b int) int {return a + b;}
                 [(fn)
                  (format "func (~a) ~a {\n~a;}"
                          (let [(args (second e))] (string-join (map compile-type-arg args) ","))
                          (third e)
                          (string-join (map compile-expr (drop e 3)) ";\n"))]
                 ; (type TYPE TYPE-LITERAL) => type TYPE TYPE-LITERAL
                 [(type) (format "type ~a ~a" (compile-expr (second e)) (compile-expr (third e)))]
                 ; (struct ((X Y int) ...) => struct { X, Y int; ... }
                 [(struct) (format "struct {~a}" (string-join (map compile-type-arg (second e)) ";"))]
                 ; (code "CODE") => CODE as-is
                 [(code) (~a (second e))]
                 ; (format form ...) ; compile-time formatting
                 [(format) (apply format (second e) (map compile-expr (drop e 2)))]
                 ; (F ARG ...) => F(ARG, ...)
                 [else (format "~a(~a)" (compile-expr f) (string-join (map compile-expr (drop e 1)) ","))])))]
        ; #\A => 'A'
        [(char? e) (format "'~a'" e)]
        ; |CODE| => CODE as-is
        [(string? e) (~s e)]
        [else (~a e)]))

(define prolog "package main\n")
(define compiled (~a prolog (string-join (map compile-expr parsed) "\n" #:after-last "\n")))
(when (arg-verbose)
  (displayln "Compiled:")
  (displayln compiled))
(define outsrc (~a (arg-out-filename) ".go"))
(define outbin (arg-out-filename))
(define out (open-output-file outsrc #:exists 'replace))
(displayln compiled out)
(close-output-port out)
(define dir (current-directory))
(when (arg-verbose)
  (displayln (format "Current directory: ~a" dir))
  (displayln (format "Output written to: ~a" outsrc)))
(when
    (system (format "go build ~a" outsrc))
  (when (arg-verbose)
    (displayln (format "Binary written to: ~a" outbin)))
  (unless (arg-compile-only)
    (void (system (format "~a~a" dir outbin)))))

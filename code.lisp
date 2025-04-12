; Recognizer for the expression language I defined
(defun exprp (x)
  (if (consp x)
      (cond ((eq (car x) 'const)
             (integerp (cadr x)))
            ((eq (car x) 'var)
             (symbolp (cadr x)))
            ((eq (car x) 'add)
             (and (exprp (cadr x))
                  (exprp (caddr x))))
            ((eq (car x) 'mul)
             (and (exprp (cadr x))
                  (exprp (caddr x))))
            ((eq (car x) 'let)
             (and (symbolp (cadr x))
                  (exprp (caddr x))
                  (exprp (cadddr x))))
            (t nil))
      nil))

; Eval function
(defun eval-expr (e env)
  (cond ((eq (car e) 'const) (cadr e))
        ((eq (car e) 'var)
         (cdr (assoc (cadr e) env)))
        ((eq (car e) 'add)
         (+ (eval-expr (cadr e) env)
            (eval-expr (caddr e) env)))
        ((eq (car e) 'mul)
         (* (eval-expr (cadr e) env)
            (eval-expr (caddr e) env)))
        ((eq (car e) 'let)
         (eval-expr (cadddr e)
                                        ; add (cons var val) to env
                    (cons (cons (cadr e) (eval-expr (caddr e) env))
                          env)))
        (t nil)))

(defun evl (e)
  (eval-expr e nil))

; Constant Folding Optimizer
(defun const-fold (e)
  (if (or (eq (car e) 'const) (eq (car e) 'var))
      e
      (if (eq (car e) 'add)
          (let ((l (const-fold (cadr e)))
                (r (const-fold (caddr e))))
            (if (and (eq (car l) 'const) (eq (car r) 'const))
                (list 'const (+ (cadr l) (cadr r)))
                e))
          (if (eq (car e) 'mul)
              (let ((l (const-fold (cadr e)))
                    (r (const-fold (caddr e))))
                (if (and (eq (car l) 'const) (eq (car r) 'const))
                    (list 'const (* (cadr l) (cadr r)))
                    e))
              (if (eq (car e) 'let)
                  (list 'let (cadr e)
                        (const-fold (caddr e))
                        (const-fold (cadddr e)))
                  e)))))

; Lemma 1
(defthm const-fold-preserves-eval
    (implies (and (exprp e) (alistp env))
             (equal (eval-expr (const-fold e) env)
                    (eval-expr e env))))

; Main Theorem
(defthm const-fold-correct
    (implies (exprp e)
             (equal (evl (const-fold e))
                    (evl e))))

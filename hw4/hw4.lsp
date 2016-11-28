; check if this single literal is satisfiable given the variable assignment
(defun check-literal (literal assignment)
  (cond ((null assignment) T)
        ((= literal (car assignment)) T)
        ((= literal (- (car assignment))) nil)
        (T (check-literal literal (cdr assignment)))
    )
  )

; check if the clause is satisfiable given the variable assignment (seq)
(defun check-clause (clause seq)
  (cond ((null clause) nil)
        ((check-literal (car clause) seq) T)
        (T (check-clause (cdr clause) seq))
    )
  )

; check if the whole cnf is satisfiable given the variable assignment (seq)
(defun check-cnf (delta seq)
  (cond ((null delta) T)
        ((check-clause (car delta) seq) (check-cnf (cdr delta) seq))
        (T nil)
    )
  )

; maintain current literal. If the literal is true, return 0 indicating that
; the clause is true. If the literal is false, return nil to make itself be
; deleted in the clause. If the literal is not found in the assigment, return
; itself to make it remain in the clause.
(defun maintain-literal (literal assignment)
  (cond ((null assignment) (list literal))
        ((= literal (car assignment)) 0)
        ((= literal (- (car assignment))) nil)
        (T (maintain-literal literal (cdr assignment)))
    )
  )

; maintain current clause. If the clause is evaluated to true, return null to
; make itself be deleted in the cnf. Otherwise, return the clause with elements 
; already in seq being deleted.
(defun maintain-clause (clause seq result-clause)
  (if (null clause)
    (list result-clause)
    (let ((maintain-result (maintain-literal (car clause) seq)))
      (if (equal maintain-result 0)
        nil
        (maintain-clause (cdr clause) seq (append result-clause maintain-result))
      )
    )
  )
)

; maintain current cnf, deleting all clauses that are satisfied and all literals
; that are in the seq.
(defun maintain-cnf (delta seq)
  (if (null delta)
    nil
    (append (maintain-clause (car delta) seq '()) (maintain-cnf (cdr delta) seq))
  )
)

; using backtrack search to generate seq, check if the seq is satisfiable, and
; maintain the cnf according to the current assignment.
(defun generate-seq (n delta seq)
  (if (null delta)
    seq
    (let* ((nextvar (caar delta))
           (newseq1 (append seq (list nextvar)))
           (newseq2 (append seq (list (- nextvar))))
           (check-result1 (check-cnf delta newseq1))
           (check-result2 (check-cnf delta newseq2))
           (newdelta1 (if check-result1 (maintain-cnf delta newseq1)))
           (newdelta2 (if check-result2 (maintain-cnf delta newseq2)))
           (sorted1 (sort newdelta1 #'< :key #'length))
           (sorted2 (sort newdelta2 #'< :key #'length)))
      (cond 
        ((and (not check-result1) (not check-result2)) nil)
        ((not check-result1) (generate-seq n sorted2 newseq2))
        ((not check-result2) (generate-seq n sorted1 newseq1))
        (T
          (if (< (length newdelta1) (length newdelta2))
            (let* ((result (generate-seq n sorted1 newseq1)))
              (if result
                result
                (generate-seq n sorted2 newseq2)
              )
            )
            (let* ((result (generate-seq n sorted2 newseq2)))
              (if result
                result
                (generate-seq n sorted1 newseq1)
              )
            )
          )
        )
      )
    )
  )
)

; generate-seq returns assignments for literals appearing in the cnf. For those
; that did not appear in the cnf, this function complements them to return the
; complete answer.
(defun complete-ans (n sol)
  (cond
    ((null sol) nil)
    ((= n 0) sol)
    ((find n sol) (complete-ans (- n 1) sol))
    ((find (- n) sol) (complete-ans (- n 1) sol))
    (T (complete-ans (- n 1) (cons n sol)))
  )
)

; top-level function to solve the cnf.
(defun sat? (n delta)
  (sort (complete-ans n (generate-seq n (sort delta #'< :key #'length) '())) #'< :key #'abs)
)

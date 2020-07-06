#lang racket/base

(require types)
(provide (all-defined-out))

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

;; (define empty-tree-type (make-type 'empty-tree))
;; (define make-empty-tree (let ((pack (typed-value-packer empty-tree-type)))
;;                           (pack 'dont-care)))
;; (define empty-tree? (typed-value-predicate empty-tree-type))
;; (define empty-tree (make-empty-tree))


;; (define node-type (make-type 'node))

;; (define make-node
;;   (let ((pack (typed-value-packer node-type)))
;;     (lambda (label left-branch right-branch)
;;       (pack (cons label (cons left-branch right-branch))))))

;; (define node? (typed-value-predicate node-type))

;; (define node-data (typed-value-unpacker node-type))
;; (define node-label
;;   (lambda (node) (car (node-data node))))
;; (define node-left-branch
;;   (lambda (node) (cadr (node-data node))))
;; (define node-right-branch
;;   (lambda (node) (caddr (node-data node))))

;; (define (depth tree)
;;   (if (empty-tree? tree)
;;       0
;;       (+ 1
;;          (max (depth (node-left-branch tree))
;;               (depth (node-right-branch tree))))))

;; (define (node-count tree)
;;   (if (empty-tree? tree)
;;       0
;;       (+ 1
;;          (node-count (node-left-branch tree))
;;          (node-count (node-right-branch tree)))))

(define node-type (make-type 'node))

(define make-node
  (let ((pack (typed-value-packer node-type)))
    (lambda (label left-branch right-branch)
      (pack (list label left-branch right-branch)))))

(define node? (typed-value-predicate node-type))

(define node-data (typed-value-unpacker node-type))

(define node-label
  (lambda (node)
    (let ((data (node-data node)))
      (car data))))

(define node-left-branch
  (lambda (node)
    (let ((data (node-data node)))
      (cadr data))))

(define node-right-branch
  (lambda (node)
    (let ((data (node-data node)))
      (caddr data))))

;; (define empty-node-type (make-type 'empty-node))
;; (define make-empty-node (typed-value-packer empty-node-type))
;; (define empty-node (make-empty-node 'empty))
;; (define empty-node? (typed-value-predicate empty-node-type))

(define empty-tree-type (make-type 'empty-tree))

(define make-empty-tree
  (let ((pack (typed-value-packer empty-tree-type)))
    (lambda ()
      (pack 'dont-care))))

(define empty-tree? (typed-value-predicate empty-tree-type))

(define empty-tree (make-empty-tree))


(define depth
  (lambda (tree)
    (if (empty-node? tree)
        0
        (+ 1
           (max (depth (node-left-branch tree))
                (depth (node-right-branch tree)))))))

(define node-count
  (lambda (tree)
    (if (empty-node? tree)
        0
        (+ 1
           (depth (node-left-branch tree))
           (depth (node-right-branch tree))))))

(define empty-tree? empty-node?)
(define empty-tree empty-node)
(define tree?
  (lambda (x) (or (empty-node? x)
                  (node? x))))


(module+ test
  ;; Tests to be run with raco test
  (require rackunit)
  (define my-tree (make-node 'A
                             (make-node 'B
                                        (make-node 'C
                                                   empty-node
                                                   (make-node 'D
                                                              empty-node
                                                              empty-node))
                                        empty-node)
                             (make-node 'E
                                        (make-node 'F
                                                   empty-node
                                                   empty-node)
                                        empty-node)))
  (check-true (tree? my-tree))
  (check-false (empty-tree? my-tree))
  (check-true (node? my-tree))
  (check-false (empty-node? my-tree))
  (check-= (depth my-tree) 4)
  (check-= (node-count my-tree) 6)

  (define my-tree-1 (make-node 'A
                               (make-node 'B
                                          (make-node 'C
                                                     empty-tree
                                                     (make-node 'D
                                                                empty-tree
                                                                empty-tree))
                                          empty-tree)
                               (make-node 'E
                                          (make-node 'F
                                                     empty-tree
                                                     empty-tree)
                                          empty-tree)))
  (check-true (tree? my-tree-1))
  (check-false (empty-tree? my-tree-1))
  (check-true (node? my-tree-1))
  (check-false (empty-node? my-tree-1))
  (check-= (depth my-tree-1) 4)
  (check-= (node-count my-tree-1) 6)

  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )

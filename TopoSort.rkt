
;=========================================================================
;KHAI NGUYEN
;CHRISTIAN MOREANO
;MATTHEW WILLIYANSON
;==========================================================================

#lang scheme
(define (topoSort L)

;The make-hash procedure creates a table where keys are compared with equal?
  (define in_graph (make-hash))
  (define in_degree_graph (make-hash))
  (define temporary_Hash (make-hash))
  (define result (make-hash))
 
;==========================================================================
  ;Read graph and obtain indegree of nodes
  ;This method constructs the in_graph and indegree in_graph
  ;
;==========================================================================
;hash-set! -> Maps key to v in hash, overwriting any existing mapping for key.
  (define (in_graph_Builder L)
    (hash-set! in_graph (car L) (cdr L))
    (hash-set! in_degree_graph (car L) 0)
  )
;==========================================================================
  ;Make ingraph and calculate indegree
  ;Input L is a list of lists (original list)
;==========================================================================  

  (define (build_Graph L function code)
    (if (null? L) null
      (cond [(= code 0) 
             (cons (function (car L)) 
                              (build_Graph (cdr L) function code))]
            [else (
                   cons (function (cdar L)) 
                        (build_Graph (cdr L) function code)
                  )
            ])
      )
  )
  
;==========================================================================
  ;Obtain  indegree of a node
  ;L: a list of adjency nodes
;==========================================================================  

  (define (obtain_in_degree L)
    (if (null? L) null
      (cons (add_in_degree (car L)) 
            (obtain_in_degree (cdr L)))
      )
  )

;==========================================================================
  ;find zero indegress nodes
  ;nodeList: a list of all nodes
;==========================================================================
;Returns the value for key in hash. If no value is found for key, then failure-result determines the result:
;If failure-result is a procedure, it is called (through a tail call) with no arguments to produce the result.
;Otherwise, failure-result is returned as the result.
  
  (define (find_zero_in_degree nodeList)
    (if (null? nodeList) null
      (cond [(= (hash-ref in_degree_graph (car nodeList)) 0) 
             (cons (car nodeList) 
                   (find_zero_in_degree (cdr nodeList)))]
            [else 
             (find_zero_in_degree (cdr nodeList))
            ])
      )
  )
;==========================================================================
  ;Add nodes + adjacents nodes → in graph
;==========================================================================

  (define (add_Graphs L)
    (if (null? L) null
      (in_graph_Builder L)
    )
  )

;==========================================================================
  ; increase the indegree of a node by 1
;==========================================================================  

  (define (add_in_degree node)
    (cond [(hash-has-key? in_degree_graph node) 
           (hash-set! in_degree_graph node 
                      (+ (hash-ref in_degree_graph node) 1))]
          [else 
           "Graph is not valid"
          ])
  )

;==========================================================================
    ;decrease ingree of zero indegree nodes
;==========================================================================

  (define (decrement_Adjency adjency)
    (hash-set! in_degree_graph adjency
               (- (hash-ref in_degree_graph adjency) 1))
    ;if the indegree of that node is 0, put it in the temporary_Hash
    (cond [(= (hash-ref in_degree_graph adjency) 0) 
           (hash-set! temporary_Hash 
                      (hash-count temporary_Hash) adjency)])
  )
;==========================================================================
  ;hash zero degreen nodes
;==========================================================================

  (define (add_zero_in_degree_node node)
    (define adjacent_List (hash-ref in_graph node))
    (hash-set! result (hash-count result) node)
    (hash-clear! temporary_Hash)
    (map (lambda(adjency) (decrement_Adjency adjency)) adjacent_List)
  )

;==========================================================================
    ;Return a new list of zero degree nodes
;==========================================================================  

  (define (new_zero_deg hashTable valueList)
    (if (null? valueList) null
        (cons (hash-ref hashTable (car valueList)) 
              (new_zero_deg hashTable (cdr valueList))))
  )
;==========================================================================
   ;This method performs sort operation
  ;zero_degree_list: a list of zero indegree nodes
;==========================================================================

  (define (start_sorting zero_degree_list)
    (if (null? zero_degree_list) null
        (cons (add_zero_in_degree_node 
               (car zero_degree_list)) 
              (start_sorting (cdr (append zero_degree_list 
                         (new_zero_deg temporary_Hash 
                              (hash-keys temporary_Hash)))))))
  )
;==========================================================================
   ; Various function calls
;==========================================================================  
  ;Call function to build a in_graph
  (build_Graph L add_Graphs 0)
  
  ;Compute the indegree of each node
  (build_Graph L obtain_in_degree 1)
 
  ;Sort 
  (start_sorting (find_zero_in_degree (hash-keys in_graph)))
  (new_zero_deg result 
                (sort (hash-keys result) <))
  )
; To use this functions just type the following in your terminal.
;(topoSort '((1 2 3 4) (2 4 5) (3 6) (4 3 6 7) (5 4 7) (6) (7 6)))

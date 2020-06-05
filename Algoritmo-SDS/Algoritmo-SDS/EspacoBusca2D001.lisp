;;;--------------------------------------------------------------------------------------------
;;; ficheiro : AlgoritmoMergulhadores.lisp
;;;; autor: Othon Oliveira
;;; projeto:
;;; descrição: Algoritmos para tratar do dominio da aplição
;;;--------------------------------------------------------------------------------------------

;;------ um tabuleiro ----

(defun tab1()
  (list
   '( x x x x x x )
   '( x m x x O x )
   '( x x x x x x )
   '( x x x m x x )
   '( x x x x x x )
   '( x x x x x x ))
  )
;;;----------------------------------------------------------------------------------------
;;; Formata o tabuleiro para o ecrã
;;;---------------------------------------------------------------------

(defun imprime-tab (tabuleiro &optional (i 0))
  (format t "~%tabuleiro: ~a"(incf i))
  (format t "~%--+---+---+---+---+---+---+")
  (dotimes (x (length tabuleiro));x até (length (car (tabuleiro) = elementos numa linha)
    (format t "~%~A "  (- (length tabuleiro) x))
    (dotimes (y (length (car tabuleiro)));y até nº de linhas de um tabuleiro
      (format t "| ~A " (if (null (nth y (nth x tabuleiro))) " " (nth y (nth x tabuleiro))) )) 
    (format t "| ~%--+---+---+---+---+---+---+" x))
;;---->>>>>  inserir mais um loop com inc i no format abaixo
  (format t "~%  | 1 | 2 | 3 | 4 | 5 | 6 |~%--+---+---+---+---+---+---+")
  (format t "~%")
  )



;;;««««««««««««««««««««««  funcoes para operar no tabuleiro  »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

;;;----------------------------------------------------------------------------------------
;;; esta funcao inverte a coordenada da funcao «nth» conforme os parametro do projecto que 
;;; expecificam a coordenada «1:1» como o canto inferior esquerdo do tabuleiro.
;;; retorna como primeira linha a do canto inferior esquerdo
;;;-----------------------------------------------------------------------------------------
(defun devol-lin (coord tab)
" devolve uma linha de um tabuleiro conforme um indice invertido"
  (cond
    ((null tab) (car tab))
    (t (nth (- (length tab) coord) tab))
    )
  )


;;;-----------------------------------------------------------------------------------------
;;; esta funcao utliza a funcao acima para retornar, conforme os parametro do projecto que 
;;; expecificam a coordenada «1:1» como o canto inferior esquerdo do tabuleiro
;;; retorna como primeiro elemento, a coordenada «1:1» (e nao o 0 como a funcao nth)
;;;-----------------------------------------------------------------------------------------

(defun devol-ele (n lst)
"devolve um elemento de uma lista, a primeira posicao xy da lista = 1"
  (cond
    ((= 1 n) (car lst))
    (t (devol-ele (1- n) (cdr lst))))
  )



;;;----------------------------------------------------------------------------------------
;;; verifica as validade das coordenadas «x»:«y» (parametros da 
;;; função abaixo; retorna t 'TRUE' ou nil 'FALSE', para um tabuleiro
;;;----------------------------------------------------------------------------------------

(defun coord-valida?(x y tab)
  "verifica as validade dos parametros «coord-v»:«coord-h» "
  (and (>= x 1) (<= x (length (car tab)))
       (>= y 1) (<= y (length tab))
       )
  )



;;;---------------------------------------------------------------------------------------
;;; esta função recebe um numero, uma lista e um elemento e substitui o n-énesimo
;;; elemento com posição «n» pelo elemento «elem» da lista «lst», a posição n=1
;;; será o primeiro elemento da lista «lst» conforme os critérios do proejecto 
;;;---------------------------------------------------------------------------------------

(defun subs-n-esimo(n lst elem)
  (cond
    ((= 1 n) (cons elem (cdr lst)))
    (t (cons (car lst) (subs-n-esimo (1- n) (cdr lst) elem)))
    )
  )



;;;---------------------------------------------------------------------------------------
;;; verifica a peca da coordenada «x:y» :: m (lugar com mergulhador),
;;; retorna t (TRUE) ou nil (falso).
;;;---------------------------------------------------------------------------------------

(defun posso-mover? (x y tab)
"verifica a função: «coord-valida? (x:y)» esta valida"
  (cond
    ;((null tab) nil)
    ((and (numberp x) (numberp y);; verifica se as coordenadas (x:y) são  numeros inteiros
	  (coord-valida? x y tab);; coordenadas não saem do tabuleiro
	  (eql (devol-ele x (devol-lin y tab)) 'm)))
    (t nil)
    )
  )



;;;---------------------------------------------------------------------------------------
;;; funcao para substituir peça da coordenada «x:y» por «M» lugar ocupado pelo mergulhador
;;; retorna um tabuleiro actualizado
;;;---------------------------------------------------------------------------------------

(defun poe-x(x y tab)
"esta função mete um 'X'  numa coordenada (x y) do tabuleiro"
  (if (null tab) nil
      (reverse (subs-n-esimo y (reverse tab) (subs-n-esimo x (devol-lin y tab) 'X))))
  )

(defun poe-mergulhador(x y tab)
"esta função mete um 'M'  numa coordenada (x y) do tabuleiro"
  (if (null tab) nil
      (reverse (subs-n-esimo y (reverse tab) (subs-n-esimo x (devol-lin y tab) 'M))))
  )

;;---------- FIBONACCI EM DIVERSOS MODOS ------------------------

(defun fibonacci (n)
"abordagem tradicional, tempo computacional O($^n)"
  (cond
    ((< n 2) 1)
    (t (+ (fibonacci (1- n)) (fibonacci (- n 2))))
    )
  )

(defun fibo (n) 
"abordagem iterativa tempo computacional O($n)"
  (labels ((calc-fib (n atual prox)
	     (if (= n 0) 
		 atual
		 (calc-fib (1- n) prox (+ atual prox)))))
    (calc-fib n 0 1)))

(defun fib (n) 
  "método SICP dos sucessivos quadrados, tempo computacional O(log(n))" 
  (check-type n (integer 0 *)) 
  (labels ((fib-aux (a b p q count) 
	     (cond ((= count 0) b) 
		   ((evenp count) 
		    (fib-aux a 
			     b 
			     (+ (* p p) (* q q)) 
			     (+ (* q q) (* 2 p q)) 
			     (/ count 2))) 
		   (t (fib-aux (+ (* b q) (* a q) (* a p)) 
			       (+ (* b p) (* a q)) 
			       p 
			       q 
			       (- count 1)))))) 
    (fib-aux 1 0 0 1 n)))


;; para testar: (time (print (fibo 7000)))

;;;------------   movimentos simples sobre o espaço de busca  ---------------------------

(defun direita (x y tab)
"esta função mete um 'M' na coordenada (x y) a direita "
  (cond
    ((null tab) nil)
    ((and (coord-valida? (1+ x) y tab) (posso-mover? x y tab))
       (poe-x x y (poe-mergulhador (1+ x) y tab)))
    (t nil))
  )



(defun esquerda (x y tab)
"esta função mete um 'M' na coordenada (x y) a direita "
  (cond
    ((null tab) nil)
    ((and (coord-valida? (1- x) y tab) (posso-mover? x y tab))
       (poe-x x y (poe-mergulhador (1- x) y tab)))
    (t nil))
  )



(defun acima (x y tab)
"esta função mete um 'M' na coordenada (x y) a direita "
  (cond
    ((null tab) nil)
    ((and (coord-valida? x (1+ y) tab) (posso-mover? x y tab))
       (poe-x x y (poe-mergulhador x (1+ y) tab)))
    (t nil))
  )


(defun abaixo (x y tab)
"esta função mete um 'M' na coordenada (x y) a direita "
  (cond
    ((null tab) nil)
    ((and (coord-valida? x (1- y) tab) (posso-mover? x y tab))
       (poe-x x y (poe-mergulhador x (1- y) tab)))
    (t nil))
  )


;;;-------  fim dos movimentos sobre o espaço de busca --------------

;;;----------------------------------------------------------------|
;;;  -------    Definição da estrutura MERGULHADOR   --------------|
;;;  ------------       uma lista dináminca       -----------------|
;;;  espaco     : Um Esbaço de busca                               |
;;;  id         : Um agente no espaço de busca                     |
;;;  pos-atual  : A posição atual do agente no espaço de busca     |
;;;  pos-melhor : Lista de Movimento - um apontador                |
;;;  f-aval     : Função de avaliação                              |
;;;  ;>>> definir uma lista de posições como vairável local (let)  | 
;;;       dentro de uma função                                     |
;-------------------------------------------------------------------
;;; O primeiro nó recebe um tabuleiro e devolve um nó
(defun mergulhador (espaco  &optional
		   (id 1)
		   (pos-atual  nil)
		   (pos-nelhor nil) 
		   (f-aval 0)
		    )
  (list 
   tabuleiro       ;; espaço de busca para o mergulhador
   mergulhador-id  ;; um mergulhador ou um identificador
   mergulhador-pos ;; a posição dum mergulhador
   movimentos      ;; lista de movimentos executados no espaço
   f-aval          ;; função de avaliação do mergulhador
   )
  )


;;;----------------------------------------------------------------------------------------
;;;----------         Funções para verificar o fim do jogo                -----------------
;;;----------------------------------------------------------------------------------------
(defun alisa (lst)
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    (t (append (alisa (car lst)) (alisa (cdr lst))))
    )
  )

(defun procura-c-linha (lin &optional (val 0))
  "verifica se existe 'C' numa linha"
  (cond
    ((null lin) val)
    ((eql (car lin) 'C) (procura-c-linha (cdr lin) (1+ val)))
    (t (procura-c-linha (cdr lin) val))
    )
  )

;; função objetivo
(defun f-obj (no)
  (let ((tab (car no)))
    (if  (= (procura-c-linha (alisa tab)) 0) t nil)
    )
  )


(defun f-aval (no)
  ;; avalia a posição do mergulhador em relação ao espaço de busca
  )

(defun movs-legais (tab &optional (lst (car tab)) (col (length (car tab))) (lin 1))
  "Recebe um tabuleiro e devolve uma lista com os movimentos aplicados ao tabuleiro"
  (let ((nlin (length tab)) (ncol (length (car tab))))
  (cond
    ((> lin nlin) nil)
    ((= col 0) (movs-legais tab lst (+ col ncol) (1+ lin)))
    ((eql (devol-ele col (devol-lin lin tab)) 'C)
     (cons (list col lin) (movs-legais tab (cdr lst) (1- col) lin)))
    (t (movs-legais tab (cdr lst) (1- col) lin))
    )
  )
 )


;;; recebe um tabuleiro e devolve uma lista de tabuleiros com os movimentos aplicados
(defun lista-tabuleiros (tab &optional (lst-jogadas (movs-legais tab)))
  (cond
    ((null lst-jogadas) nil)
    (t
     (cons (poe-zero-rect (caar lst-jogadas) (cadar lst-jogadas) tab)
	   (lista-tabuleiros tab (cdr lst-jogadas))))
    )
  )

;;; lista de sucessores, recebe um nó, expande esse nó criando uma lista de sucessores
(defun sucessores (no)
  "Recebe um nó, expande esse nó conforme a lista de movimentos"
  (let ((tab (car no)))
  (mapcar #'(lambda (tab &optional (i 0))
	      (list
	       tab
	       (cadr no)
	       (append (car no) (list (incf i));; junta listas no pai e separa com i
		       tab)
	       (car (fourth no))
	       (funcall #'f-aval no) ;;  funcao avaliacao
	       1
	       ))
	  (lista-tabuleiros tab)))
  )

;;;; -----------------   fim do arquivo  -------------------

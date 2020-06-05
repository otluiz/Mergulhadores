;;;--------------------------------------------------------------------------------------------
;;; ficheiro : AlgoritmoMergulhadores.lisp
;;; autor: Othon Oliveira
;;; projeto:
;;; descrição: Algoritmos para tratar do dominio da aplição
;;;--------------------------------------------------------------------------------------------

;;;--------------------------------------------------------------------------------------------
(defun tab1()
  (list
   '( * * * * * * )
   '( * * * * * * )
   '( * * * m * * )
   '( * 0 m - * * )
   '( * 0 * * * * )
   '( m * * * * *))
  )

;;;----------------------------------------------------------------|
;;;  -------    Definição da estrutura MERGULHADOR   --------------|
;;;  ------------       uma lista dináminca       -----------------|
;------------------------------------------------------------------|
;;; O primeiro nó recebe um tabuleiro e devolve um nó
(defun mergulhador ( &optional
		    (espaco (tab1)) 
		    (mergul-id 1)
		    (origem nil)
		    (pos-atual  nil)
		    (pos-antes nil) 
		    (f-aval 0)
		    )
  (list 
   espaco          ;; espaço de busca para o mergulhador
   mergul-id       ;; um mergulhador ou um agente
   origem          ;; a posição inicial do mergulhador ex:(x y)
   pos-atual       ;; a posição dum mergulhador atualmente
   pos-antes      ;; lista ordenada ds movimentos já executados
   f-aval          ;; função de avaliação do mergulhador
   )
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
  (and (>= x 1) (<= x (length (car tab))) ;; retorna verdadeiro
       (>= y 1) (<= y (length tab))       ;; retorna verdadeiro
       (or                                ;; retorna verdadeiro ou um ou outro
        (eql (devol-ele x (devol-lin y tab)) '-);; * um lugar(coordenada) não ocupado
        (eql (devol-ele x (devol-lin y tab)) '*))
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
   ((eql (devol-ele x (devol-lin y tab)) 'm))
   (t nil)
   )
  )



;;;---------------------------------------------------------------------------------------
;;; funcões para substituir uma peça  «x:y» por outra, ex: lugar ocupado pelo mergulhador
;;; retorna um tabuleiro actualizado com o mergulhador marcado
;;;---------------------------------------------------------------------------------------

(defun poe-x(x y tab) ;; serve para marcar as evidências no tabuleiro
"esta função mete um 'X'  numa coordenada (x y) do tabuleiro"
  (if (null tab) nil
      (reverse (subs-n-esimo y (reverse tab) (subs-n-esimo x (devol-lin y tab) 'X))))
  )

(defun poe-a(x y tab) ;; serve para marcar os lugares já visitados pelo mergulhador
"esta função mete um '-'  numa coordenada (x y) do tabuleiro"
  (if (null tab) nil
      (reverse (subs-n-esimo y (reverse tab) (subs-n-esimo x (devol-lin y tab) '-))))
  )

(defun poe-mergulhador(x y tab) ;; serve para marcar o lugar onde está o mergulhador
"esta função mete um 'M'  numa coordenada (x y) do tabuleiro"
  (if (null tab) nil
      (reverse (subs-n-esimo y (reverse tab) (subs-n-esimo x (devol-lin y tab) 'M))))
  )

;; para testar: (time (print (fibo 7000)))

;;;------------   movimentos simples sobre o espaço de busca  ---------------------------
;;; para espirais quadradas: 
;;; destra -> gira para direita: acima->direita->abaixo->esquerda...
;;;           gira para direita: abaixo->esquerda->acima->direita
;;;           gira para direita: esquerda->acima->direita->abaixo...
;;;           gira para direita: direita->abaixo->esquerda->acima...
;;; sestra -> gira para esquerda: acima->esquerda->abaixo->direita...
;;;           gira para esquerda: abaixo->direita->acima->esquerda...
;(defun poe-zero-rect (x y tab &optional (col (length (car tab))) (lin 1))
;  "ex: 12 = « 4 colunas X 3 linhas »  ou « 3 colunas X 4 linhas »"
;  (let ((ncol (length (car tab))))
;  (cond
;    ((> lin y) tab); sair do ciclo e devolver o tabuleiro actualizado
;    ((< col x) (poe-zero-rect x y tab (if (/=  col ncol) ncol ncol) (1+ lin)));;forçar a barra
;    ((<= (dif-rect x y tab) 144); tamanho maximo 12 linhas X 12 colunas = 144
;     (poe-zero-rect x y (poe-zero col lin tab) (1- col) lin))
;    (t nil))
;  )
; )


(defun direita (x y tab &optional (val 1) (col x))
  "esta função mete um 'M' na coordenada (+ x y) e "-" na (x y) a direita "
  (let ((ncol (length (car tab))))
  (cond
   ((= col (+ val x)) tab)
   ((and (< col ncol) (coord-valida? (+ x val) y tab))
     (direita x y (poe-a col y (poe-mergulhador (+ val x) y tab)) val (1+ col)));; anda + um passo
    (t nil))
    )
  )


(defun esquerda (x y tab &optional (val 1) (col x))
"esta função mete um 'M' na coordenada (x y) à esquerda "
  (let ((ncol (length (car tab))))
  (cond
   ((= col (- x val)) tab)
    ((and (coord-valida? (- x val) y tab)) ;;(posso-mover? x y tab))
       (esquerda x y (poe-a col y (poe-mergulhador (if (> x 3) (- x val) (- ncol x val)) y tab)) val (1- col)))
    (t nil))
    )
  )

(defun abaixo (x y tab&key (val 1))
"esta função mete um 'M' na coordenada (x y) abaixo "
  (cond
    ((null tab) nil)
    ((and (coord-valida? x (1- y) tab) (posso-mover? x y tab))
       (poe-a x y (poe-mergulhador x (1- y) tab)))
    (t nil))
  )





(defun acima (x y tab)
"esta função mete um 'M' na coordenada (x y) acima "
  (cond
    ((null tab) nil)
    ((and (coord-valida? x (1+ y) tab) (posso-mover? x y tab))
       (poe-a x y (poe-mergulhador x (1+ y) tab)))
    (t nil))
  )




;; funcao cria uma lista com as coordenadas (x,y) dos mergulhadores
(defun movs-legais (tab &optional (lst (car tab)) (col (length (car tab))) (lin 1))
  "Recebe um espaço de busca e devolve uma lista com as coordenadas dos mergulhadores "
  (let ((nlin (length tab)) (ncol (length (car tab))))
  (cond
    ((> lin nlin) nil)
    ((= col 0) (movs-legais tab lst (+ col ncol) (1+ lin)))
    ((eql (devol-ele col (devol-lin lin tab)) 'M)
     (cons (list col lin) (movs-legais tab (cdr lst) (1- col) lin)))
    (t (movs-legais tab (cdr lst) (1- col) lin))
    )
  )
 )


;;função para criar o padrão de busca quadrado-espandido
(defun espiral-direita (no &optional (tab (car no)))
  "aplica os quatro movimentos sobre o espaco de busca"
  (let ((movs (movs-legais tab))
        (coord (coord-espiral)))
  (cond 
    ((null movs) nil)
    ((posso-mover? (caar movs) (cadar movs) tab)
      (acima (1+ (caar movs)) (cadar movs) (direita (caar movs) (cadar movs) tab)))
    (t nil))
   )
  )
    

(defun coord-espiral (n)
  "cria uma lista com valores duplicados para as coordenadas das espirais quadradas"
  (labels ((espiral-aux (n lst)
	     (if (= 0 n) lst
		 (progn
		   (push n lst)
		   (espiral-aux (1- n) (cons n lst)))))
	   )
    (espiral-aux n '())))
		 


;;função para criar o padrão de busca quadrado-espandido
;(defun espiral-direita (no &optional (tab (car no)))
;  "aplica os quatro movimentos sobre o espaco de busca"
;  (let ((movs (movs-legais tab))
;        (coord (coord-espiral)))
;  (cond 
;    ((null movs) nil)
;    ((posso-mover? (caar movs) (cadar movs) tab)
;      (acima (1+ (caar movs)) (cadar movs) (direita (caar movs) (cadar movs) tab)))
;    (t nil))
;   )
;  )


;;---------- FIBONACCI EM DIVERSOS MODOS ------------------------

(defun fibonacci (n)
"abordagem tradicional, tempo computacional O($^n)"
  (cond
    ((< n 2) 1)
    (t (+ (fibonacci (1- n)) (fibonacci (- n 2))))
    )
  )

(defun fibo (n) ;; fibonacci
"abordagem iterativa tempo computacional O($n)"
  (labels ((calc-fib (n atual prox)
	     (if (= n 0) 
		 atual
		 (calc-fib (1- n) prox (+ atual prox)))))
    (calc-fib n 0 1)))

(defun fibo-iter (n) ;; fibonacci
"segunda abordagem iterativa f1 e f2 se atualizam"
  (do ((i n (- i 1))
       (f1 1 (+ f1 f2))
       (f2 1 f1))
      ((<= i 1) f1)))
       
(defun fib (n) ;; fibonacci
  "método SICP dos sucessivos quadrados, tempo computacional O(log(n))" 
  (check-type n (integer 0 *)) 
  (labels ((fib-aux (a b p q count) 
	     (cond ((= count 0) b) 
		   ((evenp count) 
		    (fib-aux a b (+ (* p p) (* q q)) (+ (* q q) (* 2 p q)) (/ count 2))) 
		   (t (fib-aux (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))))) 
    (fib-aux 1 0 0 1 n)))

;;;-------  fim dos movimentos sobre o espaço de busca --------------



(defun procura-mergul (lin &optional (val 0))
  "verifica se existe 'M' numa linha"
  (cond
    ((null lin) val)
    ((eql (car lin) 'M) (procura-mergul (cdr lin) (1+ val)))
    (t (procura-mergul (cdr lin) val))
    )
  )


;; função objetivo
;(defun f-obj (no)
;  (let ((tab (car no)))
;    (if  (= (procura-mergul (alisa tab)) 0) t nil)
;    )
;  )




;;; recebe um tabuleiro e devolve uma lista de tabuleiros com os movimentos aplicados
;(defun lista-tabuleiros (tab &optional (lst-jogadas (movs-legais tab)))
;  (cond
;    ((null lst-jogadas) nil)
;    (t
;     (cons (poe-zero-rect (caar lst-jogadas) (cadar lst-jogadas) tab)
;	   (lista-tabuleiros tab (cdr lst-jogadas))))
;    )
;  )

;;; lista de sucessores, recebe um nó, expande esse nó criando uma lista de sucessores
;(defun sucessores (no)
;  "Recebe um nó, expande esse nó conforme a lista de movimentos"
;  (let ((tab (car no)))
;  (mapcar #'(lambda (tab &optional (i 0))
;	      (list
;	       tab
;	       (cadr no)
;	       (append (car no) (list (incf i));; junta listas no pai e separa com i
;		       tab)
;	       (car (fourth no))
;	       (funcall #'f-aval no) ;;  funcao avaliacao
;	       1
;	       ))
;	  (lista-tabuleiros tab)))
; )

;;;; -----------------   fim do arquivo  -------------------

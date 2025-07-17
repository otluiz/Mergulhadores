;;;--------------------------------------------------------------------------------------------
;;; ficheiro : AlgoritmoMergulhadores.lisp
;;; autor: Othon Oliveira
;;; projeto: 2013- Maio
;;; descrição: Algoritmos para tratar do dominio da aplição
;;;--------------------------------------------------------------------------------------------


  
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
;;; verifica a peca da coordenada «x:y» :: m (lugar com mergulhador),
;;; retorna t (TRUE) ou nil (falso).
;;;---------------------------------------------------------------------------------------

(defun posso-mover? (x y tab)
"verifica a função: «coord-valida? (x:y)» esta valida"
  (cond
    ;((null tab) nil)
   ((eql (devol-ele x (devol-lin y tab)) 1))
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
"esta função mete um '1' id do mergulhador, numa coordenada (x y) do tabuleiro"
  (if (null tab) nil
      (reverse (subs-n-esimo y (reverse tab) (subs-n-esimo x (devol-lin y tab) 1))))
  )


;;;------------   movimentos simples sobre o espaço de busca  ---------------------------

;;; «« a direita »»
;;; mete um mergulhador, a um número determinado pela variavel VAL, de posicoes à direita
(defun direita (x y tab &optional (val 1) (col x) (pos-col (+ val x)))
  "esta função mete um 'M' na coordenada (+ x y) e "-" na (x y) a direita "
  (let ((ncol (length (car tab))))
    (cond
     ((< val 1) tab)
     ;((= col (+ val x)) tab)
     ((and (< col ncol) (coord-valida? (+ x val) y tab))
      (direita x y (poe-a col y (poe-mergulhador pos-col y tab)) (1- val) (1+ col) pos-col)) ;; anda + um passo
     (t nil))
    )
  )

;;; «« a esquerda »»
;;; mete um mergulhador, a um número determinado pela variavel VAL, de posicoes à esquerda
(defun esquerda (x y tab &optional (val 1) (col x) (pos-col (- x val)))
"esta função mete um 'M' na coordenada (x y) à esquerda "
    (cond
     ((< val 1) tab)
     ;((= col (- x val)) tab)
     ((and (coord-valida? (- x val) y tab)) ;;(posso-mover? x y tab))
      (esquerda x y (poe-a col y (poe-mergulhador pos-col y tab)) (1- val) (1- col) pos-col))
     (t nil))
    )


;;; «« abaixo »»
;;; mete um mergulhador, a um número determinado pela variavel VAL, de posicoes abaixo
(defun abaixo (x y tab &optional (val 1) (lin y) (pos-lin (- y val)))
  "esta função mete um 'M' na coordenada (x y) abaixo "
  (let ((xx x) (yy y))
  (cond
   ((< val 1) tab)
   ((and (coord-valida? xx (- yy val) tab)); (posso-mover? xx yy tab))
    (abaixo x y (poe-a xx lin (poe-mergulhador xx pos-lin tab)) (1- val) (1- lin) pos-lin))
   (t nil))
    )
  )

;;; «« acima »»
;;; mete um mergulhador, a um número determinado pela variavel VAL, de posicoes acima
(defun acima (x y tab &optional (val 1) (lin y) (pos-lin (+ y val)))
"esta função mete um 'M' na coordenada (x y) acima "
  (cond
    ((< val 1) tab)
    ((and (coord-valida? x (+ y val) tab)) ;;(posso-mover? x y tab))
       (acima x y (poe-a x lin (poe-mergulhador x pos-lin tab)) (1- val) (1+ lin) pos-lin))
    (t nil))
  )


;;;----------------------«««« para espirais quadradas »»»»------------------------------------------- 

;;; sestra -> gira para esquerda: acima->direita->abaixo->esquerda...
;;; roda uma lista de argumentos, para a esquerda
;;; ---------------------------» espiral esquerda => sestra
(defun left-rotate(lista)
  "move o 1º elemento para o fim da lista"
  (append (rest lista) (list (first lista)))
  )

;;; destra -> gira para direita: acima->esquerda->abaixo->direita...  
;;; roda uma lista de argumentos, para a direita
;;; ---------------------------» espiral direita => destra
(defun right-rotate(lista)
  "move o ultimo elemento para a frente da lista"
  (append (last lista) (butlast lista))
  )


    
;;; cria uma lista de coordenadas para aplicar aos movimentos acima e criar espirais
(defun coord-espiral (n)
  "cria uma lista com valores duplicados para as coordenadas das espirais quadradas"
  (labels ((espiral-aux (n lst)
	     (if (= 0 n) lst
		 (progn
		   (push n lst)
		   (espiral-aux (1- n) (cons n lst)))))
	   )
    (espiral-aux n '())))
		 

;;; recebe um tabuleiro e devolve uma lista de tabuleiros com os movimentos aplicados
;;; mergulhador : um mergulhador => um nó
;;; nvoltas : numero de voltas a dar
;;; cvoltas : um contador de voltas
(defun destra (no &optional (nvoltas 1) (cvoltas 0))
  (let ((lst-jog '(direita abaixo esquerda acima))
        (lst-coo (coord-espiral nvoltas))
        (tabu (car no))
        (xx (car lst-coo))
        (yy (car lst-coo))
        )
  (cond
    ((= nvolta cvoltas) nil)
   (t
    (destra (mergulhador
             (mapcar #'(lambda (x y tabu)
                         (apply #'right-rotate lst-jog)) xx yy tabu)
             (cadr no)
             (caddr no)
             (cadddr no)
             (fifth no)
             (sixth no)
             ) nvoltas (1+ cvoltas))
    )
   )
    )
  )
    
    

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


;; funcao cria uma lista com as coordenadas (x,y) dos mergulhadores
(defun coord-merg (tab &optional (lst (car tab)) (col (length (car tab))) (lin 1))
  "Recebe um espaço de busca e devolve uma lista com as coordenadas dos mergulhadores "
  (let ((nlin (length tab)) (ncol (length (car tab))))
  (cond
    ((> lin nlin) nil)
    ((= col 0) (coord-merg tab lst (+ col ncol) (1+ lin)))
    ((eql (devol-ele col (devol-lin lin tab)) 1)
     (cons (list col lin) (coord-merg tab (cdr lst) (1- col) lin)))
    (t (coord-merg tab (cdr lst) (1- col) lin))
    )
  )
 )

;;;--------------------------------------------------------------------------------------------
(defun tab1()
  (list
   '( * * * * * * )
   '( * * * * * * )
   '( * * * 0 * * )
   '( * 0 1 * * * )
   '( * 0 * * * * )
   '( 1 * * * * *))
  )

;;;----------------------------------------------------------------|
;;;  -------    Definição da estrutura MERGULHADOR   --------------|
;;;  ------------       uma lista dináminca       -----------------|
;------------------------------------------------------------------|
;;; O primeiro nó recebe um tabuleiro e devolve um nó
(defun mergulhador (&optional
                    (espaco (tab1))
		    (mergul-id 1)
		    (origem (coord-merg (tab1)))
		    (pos-atual  nil)
		    (pos-antes nil) 
		    (f-aval 0)
		    )
  (list 
   espaco         ;; espaço de busca para o mergulhador
   mergul-id       ;; um mergulhador ou um agente
   origem          ;; a posição inicial do mergulhador ex:(x y)
   pos-atual       ;; a posição dum mergulhador atualmente
   pos-antes      ;; lista ordenada ds movimentos já executados
   f-aval          ;; função de avaliação do mergulhador
   )
  )

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

;; para testar: (time (print (fibo 7000)))

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

;;;-------------------------------------------------------------------------
;;; arquivo: Interface Humano
;;; Othon Oliveira
;;; projeto: Algoritmo Mergulhadores
;;; descrição: Interface para interagir com o utilizador humano


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Carrega os espaços de busca(tabuleiro) de um arquivo para a memoria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun carrega-tabuleiro (caminho)
  (with-open-file (fich caminho :direction :input :if-does-not-exist :error)
    (read fich)
    )
  )

(defun ler-fich ()
  (carrega-tabuleiro "~/Lisp/AlgoMergulhadores/tabuleiro.txt" )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Captura o tabuleiro escolhido pelo utilizador
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enesimo-tabuleiro (a lst)
  (cond ((= 1 a) (car lst))
        ((null lst) nil)
	(t (enesimo-tabuleiro (1- a) (cdr lst))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tabuleiros para teste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tab1()
  (list
   '( * * * * * * )
   '( * * * * * * )
   '( * * * * * * )
   '( * 0 * * * * )
   '( * 0 * * * * )
   '( * * * * * *))
  )
(defun tab2()
  (list
   '( * * * * * 0 )
   '( * * * * 0 0 )
   '( * * * * * * )
   '( * * * * * * )
   '( * * * * * * )
   '( * * * * * *))
  )
;;;----------------------------------------------------------------------------------------
;;; Formata o espaço de busca para o ecrã
;;;---------------------------------------------------------------------

(defun imprime-tab (tabuleiro &optional (i 0))
  (format t "~%tabuleiro: ~d"(incf i))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Menu Tabuleiro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun escolha-do-tabuleiro ()
  (terpri)
  (format t "    Escolha um ESPAÇO DE BUSCA :~% ")
  (format t "    ~%~a" "1 - tabuleiro 1")
  (format t "    ~%~a" "2 - tabuleiro 2")
  (terpri)
  (let ((opcao (read)))
    (cond
      ((= opcao 1) (tab1));;(enesimo-tabuleiro 1 (ler-fich)))
      ((= opcao 2) (tab2));;(enesimo-tabuleiro 2 (ler-fich)))
      (t (princ "Escolha invalida !!!") (terpri) (escolha-do-tabuleiro)))
    )
  )


;;*****************************************************************************
;;; funções que interagem com o humano
;;*****************************************************************************

;;; Executa a interface com o utilizador
;;; Devolve um nó com os movimentos aplicados ao nó

(defun interface-Human (no)
  (let ((pos-x (read (format t "~% =>(coluna): Posição X da origem : ")))
        (pos-y (read (format t "   =>(linha): Posição Y da origem: ")))
	(tabu (escolha-do-tabuleiro)))
    (if (and (numberp pos-x) (numberp pos-y) ;; valida as coordenadas
	     (coord-valida? pos-x pos-y tabu))
	   (mergulhador
	    (poe-mergulhador pos-x pos-y tabu)
	    (cadr no) ;1 id padrão	
	    (list pos-x pos-y) ;; origem dos movimentos
	    (push (list pos-x pos-y) (fourth no)) ;; posição atual
	    (fifth no) ;;posições anteriores
	    (sixth no)    ;;(funcall #'f-aval no)))
	    )
	   (format t "~A ~% algo correu mal" (interface-Human no))
	   )
    )
  )


;;; Inicia o jogo
(defun iniciar ()
  (terpri) (terpri) (terpri)

  (format t "~%   **************************************************************")
  (format t "~%   ************                                       ***********")
  (format t "~%   **********             B E M  V I N D O              *********")
  (format t "~%   *********                    A O                      ********")
  (format t "~%   *********             A L G O R I T M O               ********")
  (format t "~%   *********                   D O S                     ********")
  (format t "~%   *********          M E R G U L H A D O R E S          ********")
  (format t "~%   **********                                           *********")
  (format t "~%   ************                                       ***********")
  (format t "~%   ************************************************************** ~%")
  (terpri) (terpri);; (terpri) (terpri) (terpri) (terpri)
    (progn
     ; (imprime-tab tabu)
      (format t "~% inicializando o espaço ... ")
      (sleep 5) 
      (terpri) (terpri) (terpri) (terpri) (terpri) (terpri)     
      (format t"~% estes são os espaços dispiníveis...")      
      ;;(terpri) (terpri) ;;(terpri) (terpri) (terpri) (terpri)      
      (imprime-tab (tab1))
      (sleep 2)
      (imprime-tab (tab2))
      (sleep 1)
      (format t "escolha uma coordenada e um tabuleiro, para o mergulhador iniciar ~%")
      (terpri) (terpri);; (terpri) (terpri) (terpri) (terpri)
      (imprime-tab (car (interface-Human (mergulhador))))
      )
    )

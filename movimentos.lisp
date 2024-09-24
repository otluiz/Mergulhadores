;;;-------------------------------------------------------------------------
;;; Arquivo: movimentos.lisp
;;; Autor: Othon Oliveira
;;; Projeto: Algoritmo Mergulhadores
;;; Descrição: Funções de movimento para os mergulhadores
;;;-------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Funções Auxiliares para Movimentação
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pos-valida? (tabuleiro x y)
  "Verifica se a posição (X, Y) é válida no TABULEIRO."
  (and (>= x 0) (< x (length tabuleiro))
       (>= y 0) (< y (length (car tabuleiro)))
       (not (eq (aref tabuleiro x y) '0)))) ;; 0 representa um obstáculo

(defun marcar-posicao (tabuleiro x y)
  "Marca a posição (X, Y) com 'M' para indicar a posição do mergulhador no TABULEIRO."
  (setf (aref tabuleiro x y) 'M))

(defun limpar-posicao (tabuleiro x y)
  "Limpa a posição (X, Y) no TABULEIRO."
  (setf (aref tabuleiro x y) '*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função de Movimento em Espiral
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mover-espiral (tabuleiro pos &optional (raio 1) (angulo 0))
  "Move o mergulhador em movimento espiral a partir da posição POS no TABULEIRO.
RAIO define o tamanho da espiral e ANGULO é a direção inicial."
  (let ((x (car pos))
        (y (cadr pos))
        (direcao 'direita) ;; Direção inicial
        (passos 0)
        (passos-para-mudar 1)
        (mudancas-de-direcao 0))
    (loop
     ;; Verifica se a posição é válida
     (unless (pos-valida? tabuleiro x y)
       (return (list x y)))
     ;; Limpa a posição anterior
     (limpar-posicao tabuleiro x y)
     ;; Move o mergulhador com base na direção
     (case direcao
       (direita (incf y))
       (baixo (incf x))
       (esquerda (decf y))
       (cima (decf x)))
     ;; Marca a nova posição
     (marcar-posicao tabuleiro x y)
     (incf passos)
     ;; Mudança de direção
     (when (= passos passos-para-mudar)
       (setf passos 0)
       (incf mudancas-de-direcao)
       (when (evenp mudancas-de-direcao)
         (incf passos-para-mudar))
       ;; Atualiza a direção
       (setf direcao (case direcao
                       (direita 'baixo)
                       (baixo 'esquerda)
                       (esquerda 'cima)
                       (cima 'direita)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função de Movimento em Linha-guia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mover-linha-guia (tabuleiro pos passos)
  "Move o mergulhador em linha reta, por PASSOS, a partir da posição POS no TABULEIRO."
  (let ((x (car pos))
        (y (cadr pos))
        (direcao 'direita) ;; Direção inicial
        (passos-restantes passos))
    (loop
     ;; Verifica se a posição é válida e se ainda há passos para percorrer
     (when (or (not (pos-valida? tabuleiro x y)) (= passos-restantes 0))
       (return (list x y)))
     ;; Limpa a posição anterior
     (limpar-posicao tabuleiro x y)
     ;; Move o mergulhador com base na direção
     (case direcao
       (direita (incf y))
       (baixo (incf x))
       (esquerda (decf y))
       (cima (decf x)))
     ;; Marca a nova posição
     (marcar-posicao tabuleiro x y)
     (decf passos-restantes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Funções de Movimento Auxiliares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mover-randomico (tabuleiro pos)
  "Move o mergulhador para uma posição aleatória válida no TABULEIRO."
  (let ((x (car pos))
        (y (cadr pos)))
    (loop
       (let ((nova-x (+ x (random 3) -1)) ;; Movimento aleatório entre -1 e 1
             (nova-y (+ y (random 3) -1)))
         (when (pos-valida? tabuleiro nova-x nova-y)
           (return (list nova-x nova-y)))))))

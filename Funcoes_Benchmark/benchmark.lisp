;;;-------------------------------------------------------------------------
;;; Arquivo: benchmark.lisp
;;; Autor: Othon Oliveira
;;; Projeto: Algoritmo Mergulhadores
;;; Descrição: Funções de benchmark e representação visual do espaço de busca
;;;-------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função de Benchmark: Rastrigin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rastrigin (posicao)
  "Calcula a função Rastrigin na posição POSICAO.
POSICAO é uma lista com as coordenadas (x y)."
  (let* ((x (car posicao))
         (y (cadr posicao))
         (n 2) ;; Dimensão do problema
         (a 10))
    (+ (* a n)
       (- (+ (expt x 2) (- (* a (cos (* 2 pi x)))))
          (+ (expt y 2) (- (* a (cos (* 2 pi y)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função para Criar um Tabuleiro de Benchmark Rastrigin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun criar-tabuleiro-rastrigin (dimensao &optional (alvo '(0 0)))
  "Cria um tabuleiro de benchmark Rastrigin com dimensão DIMENSAO.
O parâmetro ALVO representa a posição do mínimo global."
  (let ((tabuleiro (make-array (list dimensao dimensao) :initial-element '*)))
    ;; Preencher o tabuleiro com valores da função Rastrigin
    (dotimes (i dimensao)
      (dotimes (j dimensao)
        (setf (aref tabuleiro i j) (rastrigin (list i j)))))
    ;; Marcar o alvo no tabuleiro
    (setf (aref tabuleiro (car alvo) (cadr alvo)) 'O) ;; 'O' indica o alvo
    tabuleiro))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função para Imprimir Tabuleiro de Benchmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun imprime-tabuleiro-rastrigin (tabuleiro)
  "Imprime o tabuleiro TABULEIRO de forma visual."
  (dotimes (i (array-dimension tabuleiro 0))
    (dotimes (j (array-dimension tabuleiro 1))
      (format t "|~4A " (aref tabuleiro i j)))
    (format t "~%"))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função para Atualizar a Posição do Mergulhador no Tabuleiro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atualizar-posicao (tabuleiro posicao)
  "Atualiza a posição do mergulhador no TABULEIRO.
POSICAO é uma lista com as coordenadas (x y)."
  (setf (aref tabuleiro (car posicao) (cadr posicao)) 'M)) ;; 'M' indica o mergulhador

;;;-------------------------------------------------------------------------
;;; Arquivo: main.lisp
;;; Autor: Othon Oliveira
;;; Projeto: Algoritmo Mergulhadores
;;; Descrição: Interface principal para interagir com o utilizador humano
;;;-------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Carregar Módulos Necessários
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Carregar o módulo de movimentos
(load "movimentos.lisp")

;; Carregar o módulo de benchmark (antes rastrigin)
(load "benchmark.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Funções para Carregar Tabuleiros e Benchmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun carrega-tabuleiros (arquivo)
  "Carrega os tabuleiros do arquivo ARQUIVO e retorna uma lista de tabuleiros."
  (with-open-file (stream arquivo)
    (loop with tabuleiros = nil
          for linha = (read stream nil 'eof)
          until (eq linha 'eof)
          do (push linha tabuleiros)
          finally (return (nreverse tabuleiros)))))

(defun carrega-benchmark (arquivo)
  "Carrega o arquivo de benchmark contendo a função Rastrigin e retorna o resultado."
  (if (probe-file arquivo)
      (progn
        (load arquivo)
        (format t "~%Arquivo ~a carregado com sucesso!~%" arquivo)
        (criar-tabuleiro-rastrigin 10 '(0 0))) ;; Cria e retorna um tabuleiro de benchmark
      (format t "~%Erro: Arquivo ~a não encontrado!~%" arquivo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função para Inicializar Mergulhadores no Espaço de Busca
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inicializar-mergulhadores (tabuleiro)
  "Inicializa 20 mergulhadores em posições aleatórias válidas no TABULEIRO.
TABULEIRO deve ser uma matriz 2D ou um array multidimensional."
  (let (mergulhadores)
    (dotimes (i 20) ;; 20 mergulhadores
      (let ((posicao (loop
                       with x = (random (array-dimension tabuleiro 0))
                       with y = (random (array-dimension tabuleiro 1))
                       until (pos-valida? tabuleiro x y)
                       finally (return (list x y)))))
        (push (list posicao (rastrigin posicao)) mergulhadores)))
    mergulhadores))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função de Busca Utilizando Mergulhadores e Funções de Benchmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buscar-minimo-global (tabuleiro)
  "Executa a busca pelo mínimo global utilizando 20 mergulhadores no TABULEIRO."
  (let ((tentativas 0)
        (melhor-valor most-positive-fixnum)
        (melhor-posicao nil)
        (mergulhadores (inicializar-mergulhadores tabuleiro)))
    ;; Loop de busca até atingir o número máximo de tentativas
    (loop while (< tentativas 1000) ;; 1000 tentativas
          do (progn
               (incf tentativas)
               (dolist (mergulhador mergulhadores)
                 (let* ((posicao (car mergulhador))
                        (valor (cadr mergulhador))
                        (nova-posicao (loop
                                        with x = (car posicao)
                                        with y = (cadr posicao)
                                        until (pos-valida? tabuleiro x y)
                                        finally (return (list x y))))
                        (novo-valor (rastrigin nova-posicao)))
                   ;; Se encontrar um valor melhor, atualizar
                   (when (< novo-valor melhor-valor)
                     (setf melhor-valor novo-valor)
                     (setf melhor-posicao nova-posicao))
                   ;; Atualizar posição e valor do mergulhador
                   (setf (car mergulhador) nova-posicao)
                   (setf (cadr mergulhador) novo-valor)))
               ;; Verificar se o valor é próximo o suficiente do mínimo global
               (when (< melhor-valor 1e-2)
                 (format t "Mínimo global encontrado na posição ~a com valor ~f após ~d tentativas.~%"
                         melhor-posicao melhor-valor tentativas)
                 (return))))
    (if (>= tentativas 1000)
        (format t "Número máximo de tentativas atingido. Melhor valor encontrado: ~f na posição ~a.~%"
                melhor-valor melhor-posicao)
        (format t "Busca encerrada. Melhor valor encontrado: ~f na posição ~a.~%"
                melhor-valor melhor-posicao))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função para Escolher o Tipo de Busca e Espaço de Busca
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun escolher-espaco-de-busca ()
  "Permite ao usuário escolher entre carregar um tabuleiro ou um benchmark."
  (format t "Escolha o tipo de espaço de busca:~%")
  (format t "1 - Carregar Tabuleiros do Arquivo tabuleiros.txt~%")
  (format t "2 - Carregar Arquivo benchmark.lisp~%")
  (terpri)
  (let ((opcao (read)))
    (cond
     ((= opcao 1)
      (carrega-tabuleiros "Tabuleiros/tabuleiros.txt"))
     ((= opcao 2)
      (carrega-benchmark "benchmark.lisp"))
     (t (format t "Escolha inválida! Tente novamente.~%")
        (escolher-espaco-de-busca)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função Principal para Iniciar o Programa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iniciar ()
  "Função principal para iniciar o programa."
  (let ((tabuleiro (escolher-espaco-de-busca)))
    (if tabuleiro
        (buscar-minimo-global tabuleiro)
        (format t "Erro ao carregar o espaço de busca. Tente novamente."))))

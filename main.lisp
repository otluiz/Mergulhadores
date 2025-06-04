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

;; Carregar o módulo de interface gráfica opcional
(load "gui.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Estrutura para representar um mergulhador
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct mergulhador
  posicao    ;; Coordenadas atuais
  valor      ;; Valor da função na posição
  (oxigenio 5) ;; Quantidade de oxigênio disponível
  (vivo t))  ;; Indica se o mergulhador ainda pode participar

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
(defun inicializar-mergulhadores (tabuleiro &optional (quantidade 20))
  "Inicializa QUANTIDADE mergulhadores em posições aleatórias válidas no
TABULEIRO. TABULEIRO deve ser uma matriz 2D ou um array multidimensional."
  (let (mergulhadores)
    (dotimes (i quantidade)
      (let ((posicao (loop
                       with x = (random (array-dimension tabuleiro 0))
                       with y = (random (array-dimension tabuleiro 1))
                       until (pos-valida? tabuleiro x y)
                       finally (return (list x y)))))
        (push (make-mergulhador
               :posicao posicao
               :valor (rastrigin posicao))
              mergulhadores)))
    mergulhadores))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função de Busca Utilizando Mergulhadores e Funções de Benchmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buscar-minimo-global (tabuleiro quantidade &optional (visualizar nil))
  "Executa a busca pelo mínimo global utilizando QUANTIDADE de mergulhadores no TABULEIRO.
Se VISUALIZAR for verdadeiro, uma janela com Common Graphics exibirá as posições."
  (let ((tentativas 0)
        (melhor-valor most-positive-fixnum)
        (melhor-posicao nil)
        (mergulhadores (inicializar-mergulhadores tabuleiro quantidade)))
    (when visualizar
      (mergulhadores-gui:abrir-janela (array-dimension tabuleiro 0))
      (mergulhadores-gui:atualizar-gui tabuleiro mergulhadores))
    ;; Loop de busca até atingir o número máximo de tentativas
    (loop while (and (< tentativas 1000) mergulhadores)
          do (progn
               (incf tentativas)
               (setf mergulhadores
                     (remove-if-not #'mergulhador-vivo mergulhadores))
               (dolist (m mergulhadores)
                 (let* ((posicao (mergulhador-posicao m))
                        (nova-posicao (mover-randomico tabuleiro posicao))
                        (novo-valor (rastrigin nova-posicao)))
                   ;; Se encontrar valor melhor, atualizar global e oxigênio
                   (when (< novo-valor melhor-valor)
                     (setf melhor-valor novo-valor
                           melhor-posicao nova-posicao)
                     (incf (mergulhador-oxigenio m)))
                   ;; Caso contrário, consumir oxigênio
                   (decf (mergulhador-oxigenio m))
                   ;; Atualizar posição/valor
                   (setf (mergulhador-posicao m) nova-posicao
                         (mergulhador-valor m) novo-valor)
                   ;; Verifica se mergulhador morreu
                   (when (<= (mergulhador-oxigenio m) 0)
                     (setf (mergulhador-vivo m) nil))))
               (when visualizar
                 (mergulhadores-gui:atualizar-gui tabuleiro mergulhadores))
               ;; Checagem de convergência
               (when (< melhor-valor 1e-2)
                 (format t "Mínimo global encontrado na posição ~a com valor ~f após ~d tentativas.~%"
                         melhor-posicao melhor-valor tentativas)
                 (return))))
    (if (>= tentativas 1000)
        (format t "Número máximo de tentativas atingido. Melhor valor encontrado: ~f na posição ~a.~%"
                melhor-valor melhor-posicao)
        (format t "Busca encerrada. Melhor valor encontrado: ~f na posição ~a.~%"
                melhor-valor melhor-posicao))
    (when visualizar
      (mergulhadores-gui:fechar-janela)))

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
;;;; Função para Solicitar Quantidade de Mergulhadores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solicitar-quantidade-mergulhadores ()
  "Pergunta ao usuário quantos mergulhadores deseja utilizar."
  (format t "Digite a quantidade de mergulhadores (padrão 20): ")
  (let ((q (read)))
    (if (and (integerp q) (> q 0))
        q
        20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perguntar se deseja visualizar com Common Graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solicitar-visualizacao ()
  "Pergunta ao usuário se deseja abrir a janela gráfica."
  (format t "Deseja visualizar graficamente? (1 = sim, 0 = nao): ")
  (= (read) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Função Principal para Iniciar o Programa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iniciar ()
  "Função principal para iniciar o programa."
  (let ((tabuleiro (escolher-espaco-de-busca)))
    (if tabuleiro
        (let ((quantidade (solicitar-quantidade-mergulhadores))
              (vis (solicitar-visualizacao)))
          (buscar-minimo-global tabuleiro quantidade vis))
        (format t "Erro ao carregar o espaço de busca. Tente novamente."))))

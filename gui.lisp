(in-package :cl-user)
(defpackage :mergulhadores-gui
  (:use :cl :common-graphics))
(in-package :mergulhadores-gui)

(defvar *janela* nil)
(defparameter *celula-tamanho* 20)

(defun abrir-janela (dimensao)
  "Cria a janela para exibir o tabuleiro."
  (setf *janela*
        (make-window "Mergulhadores"
                     :width (* *celula-tamanho* dimensao)
                     :height (* *celula-tamanho* dimensao)
                     :class 'dialog))
  (display *janela*))

(defun fechar-janela ()
  (when *janela*
    (destroy-window *janela*)
    (setf *janela* nil)))

(defun desenhar-celula (x y cor)
  (let ((stream (graphics-port *janela*)))
    (set-ink stream cor)
    (draw-rectangle* stream
                     (* x *celula-tamanho*)
                     (* y *celula-tamanho*)
                     (+ (* x *celula-tamanho*) *celula-tamanho*)
                     (+ (* y *celula-tamanho*) *celula-tamanho*))))

(defun desenhar-tabuleiro (tabuleiro mergulhadores)
  (let ((dim (array-dimension tabuleiro 0)))
    (dotimes (i dim)
      (dotimes (j dim)
        (desenhar-celula j i +white+)))
    (dolist (m mergulhadores)
      (let* ((pos (mergulhador-posicao m))
             (x (second pos))
             (y (first pos)))
        (desenhar-celula x y +red+)))))

(defun atualizar-gui (tabuleiro mergulhadores)
  (when *janela*
    (clear-graphics *janela*)
    (desenhar-tabuleiro tabuleiro mergulhadores)
    (invalidate *janela*)))

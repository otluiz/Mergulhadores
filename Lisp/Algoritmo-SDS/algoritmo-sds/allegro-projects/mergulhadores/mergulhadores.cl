;;;;;;;;;;
;;;;;;;;;; mergulhadores orientado a objetos
;;;;;;;;;;

(defclass mergulhadores ()
  ((rows
    :initarg :rows
    :initform (error ":linha tem que ser especificada")
    :reader mergulhadores-rows)
    (cols
    :initarg :cols
    :initform (error ":coluna tem que ser especificada")
     :reader mergulhadores-cols)
   (data
    :initarg :data
    :accessor mergulhador-data)))
 
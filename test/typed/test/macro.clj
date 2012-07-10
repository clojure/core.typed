(ns typed.test.macro)

(defmacro anything [& a]
  `(if 1 ~a 1))

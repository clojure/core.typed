;https://gist.github.com/3191865
;
;(defn cpf [f priority]
;  (fn [k & args]
;    (js/setTimeout #(k (apply f args)) priority)))
;
;(def sqrt #(Math/sqrt %))
;
;(defn pyth
;  "Calculate the hypotenuse of a right triangle."
;  [x y]
;  (sqrt (+ (* x x) (* y y))))
;
;(defn pyth&
;  "Calculate the hypotenuse of a right triangle: continuation passing style (CPS) version.
;  
;  Represents a green thread. k is the function to invoke with the
;  final result; priority is the number of milliseconds to wait between
;  continuation invocations.
;
;  One can imagine a macro for converting pyth to pyth&.
;
;  One can also imagine a mechanism, involving the cpf function above,
;  for maintaining outstanding green threads and their associated
;  timeouts and continuations.  One might use .clearInterval to pause a
;  thread, restarting it by invoking the last reified continuation in
;  that thread.
;
;  One may also involve a scheduler that prioritizes or deprioritizes
;  outstanding threads by some heuristic.
;
;  This system could allow us to use futures, promises, and blocking
;  derefs on green threads without blocking the entire browser in
;  ClojureScript programs."
;  [x y k priority]
;  ((cpf * priority)
;   (fn [x2]
;     ((cpf * priority)
;      (fn [y2]
;        ((cpf + priority)
;         (fn [x2py2]
;           ((cpf sqrt priority) k x2py2)) x2 y2)) y y)) x x))
;
;(do
;  (pyth& 1 2 #(.log js/console (str "thread 0: " %)) 500)
;  (pyth& 1 2 #(.log js/console (str "thread 1: " %)) 100)
;  (pyth& 1 2 #(.log js/console (str "thread 2: " %)) 0)
;  ;; thread 2: 2.23606797749979
;  ;; thread 1: 2.23606797749979
;  ;; thread 0: 2.23606797749979
;  )

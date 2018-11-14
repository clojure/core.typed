(ns clojure.core.typed.annotator.debug-macros
  (:require [clojure.core.typed.annotator.util :refer [*debug*
                                                       *debug-depth*
                                                       current-time]])
  )

(defmacro debug-flat
  ([msg]
   `(when (= :all *debug*)
      (print (str (apply str (repeat *debug-depth* "  ")) *debug-depth* ": "))
      ~msg)))

(defmacro debug 
  ([msg body]
   `(do
      (debug-flat ~msg)
      (binding [*debug-depth* (when (= :all *debug*)
                                (inc *debug-depth*))]
        ~body))))

(defmacro debug-when [state msg]
  `(when (and (set? *debug*)
              (contains? *debug* ~state))
     (let [msg# ~msg]
       (println)
       (println (str "SQUASH ITERATION:\n" msg#)))))

(defmacro debug-squash [msg]
  `(debug-when :squash 
               (str "\nSQUASH ITERATION:\n" ~msg "\n")))

(defmacro time-if-slow
  "Evaluates expr and prints the time it took.  Returns the value of expr."
  [msg expr]
  `(let [start# (current-time)
         ret# ~expr
         msduration# (/ (double (- (current-time) start#)) 1000000.0)]
     (when (< 1000 msduration#)
       (prn (str "Elapsed time: " msduration# " msecs"))
       (prn ~msg))
     ret#))


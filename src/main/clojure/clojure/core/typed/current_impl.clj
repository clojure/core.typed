(ns clojure.core.typed.current-impl)

(def default ::default)
(def clojure ::clojure)
(def clojurescript ::clojurescript)

(derive clojurescript default)
(derive clojure default)

(def TYPED-IMPL (atom clojure))
(set-validator! TYPED-IMPL #(isa? % default))

(defn ensure-clojure []
  (reset! TYPED-IMPL clojure))

(defn ensure-clojurescript []
  (reset! TYPED-IMPL clojurescript))

(defn checking-clojure? []
  (= clojure @TYPED-IMPL))

(defn checking-clojurescript? []
  (= clojurescript @TYPED-IMPL))

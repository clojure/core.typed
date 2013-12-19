(ns clojure.core.typed.load-cljs)

; We need to be very strict about loading Clojurescript. It
; breaks easily if loaded while compiling files.

(def ^:private tried-loading-cljs? (atom false))

(defn load-cljs []
  (when-not *compile-files*
    (assert (not *compile-files*))
    (when-not @tried-loading-cljs?
      (do
        (println (str "Loading Clojurescript..."))
        (flush)
        (try (require '[cljs.analyzer]
                      '[cljs.compiler]
                      '[cljs.tools.analyzer]
                      '[cljs.tools.analyzer.hygienic]
                      '[cljs.tools.analyzer.emit-form]
                      '[cljs.core])
             (println (str "Clojurescript found and loaded."))
             (flush)
             (catch Throwable e
               (println (str "Clojurescript not found"))
               (flush)))
        (reset! tried-loading-cljs? true)))))

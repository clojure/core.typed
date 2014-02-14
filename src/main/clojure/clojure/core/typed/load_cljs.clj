(ns clojure.core.typed.load-cljs)

; We need to be very strict about loading Clojurescript. It
; breaks easily if loaded while compiling files.

(def ^:private tried-loading-cljs? (atom false))

(defn load-cljs []
  ;completely disable CLJS until we can support later versions
  #_(when-not *compile-files*
    (assert (not *compile-files*))
    (when-not @tried-loading-cljs?
      (do
        (println (str "Loading Clojurescript..."))
        (flush)
        (try (require '[cljs.analyzer]
                      '[cljs.compiler]
                      '[cljs.jvm.tools.analyzer]
                      '[cljs.jvm.tools.analyzer.hygienic]
                      '[cljs.jvm.tools.analyzer.emit-form]
                      '[cljs.core])
             (println (str "Clojurescript found and loaded."))
             (flush)
             (catch Throwable e
               (println (str "Clojurescript not found"))
               (flush)))
        (reset! tried-loading-cljs? true)))))

(ns clojure.core.typed.test.CTYP-37
  (:require [clojure.core.typed :as t]))

; this used to fail with jvm.tools.analyzer, now seems
; to type check successfully with tools.analyzer.jvm

(t/defprotocol PProcess
  (stop [system] :- PProcess
        "Runs side effects to stop the process. Returns the process."))

(t/ann-record Logger [in :- t/Any
                      out-listener :- t/Any
                      stop :- t/Any])
(defrecord Logger [in out-listener stop]
  PProcess
  (stop [{:keys [stop] :as logger}]
    logger))


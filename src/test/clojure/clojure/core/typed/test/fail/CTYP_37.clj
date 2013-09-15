(ns clojure.core.typed.test.fail.CTYP-37
  (:require [clojure.core.typed :as t]))

(t/ann-protocol PProcess
                stop [PProcess -> PProcess])
(t/defprotocol> PProcess
  (stop [system] "Runs side effects to stop the process. Returns the process."))

(t/ann-record Logger [in :- Any
                      out-listener :- Any
                      stop :- Any])
(defrecord Logger [in out-listener stop]
  PProcess
  (stop [{:keys [stop] :as logger}]
    logger))


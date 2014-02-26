(ns clojure.core.typed.test.ctyp105
  (:require [clojure.string :as str]
            [clojure.core.typed :refer 
             [check-ns cf ann ann-many ann-form 
              def-alias ann-many Seqable Option 
              AnyInteger 
              fn> doseq>
              print-env Seq tc-ignore]
             :as t])
  (:import [clojure.lang IPersistentList IPersistentVector IPersistentSet IPersistentMap Keyword Symbol]))

(def-alias PatInfo (HMap :mandatory {:ptnt_nr String}
                         :optional {:ptnt_vertrekdatum String
                                     :ptnt_voorv2denaam String 
                                     :ptnt_tweedenaam   String
                                     :ptnt_fullname     String
                                     :ptnt_roepnaam  String
                                     :roepnaam       String
                                     :ptnt_geboren   String
                                     :geboortedatum String
                                     :ptnt_voorletters String
                                     :voorletters String
                                     :ptnt_voorveigen   String
                                     :voorvoegsel String
                                     :actief Boolean
                                     :achternaam String
                                     :ptnt_ruiter String
                                     :sms_status String
                                     }))

(ann ptnt-check-ruiters [PatInfo -> PatInfo])
(defn ptnt-check-ruiters
  "Check the SMS-status of a patient.
    :sms_satus Status will be set to 'ON', 'OFF' or ''"
  [rec]
  (let [smsStat (if-let [ruiters (:ptnt_ruiter rec)]
                  (let [ruiters (str/upper-case ruiters)]
                    (if (re-find #"\*SN" ruiters)
                      "Off"
                      (if (re-find #"\*SO" ruiters)
                        "On"
                        "?")))
                  "?")]
    (ann-form rec PatInfo)
    (assoc (ann-form (dissoc rec :ptnt_ruiter) PatInfo)
      :sms_status (str smsStat))))

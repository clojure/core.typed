#!/bin/sh
#_(

   #_DEPS is same format as deps.edn. Multiline is okay.
   DEPS='
   {:deps {}}
   '

   #_You can put other options here

exec clojure $OPTS -Sdeps "$DEPS" "$0" "$@"

)

(def changelog
  (-> (slurp "changelog/changelog.edn")
      read-string))

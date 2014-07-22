(ns ^:skip-wiki clojure.core.typed.unsafe)

(alter-meta! *ns* assoc :skip-wiki true)

;(ann ignore-with-unchecked-cast* [Any Any -> Any])
(defn ^:skip-wiki ignore-with-unchecked-cast* [form ty]
  form)

(defmacro ignore-with-unchecked-cast
  "Assumes the form is well typed and annotates it with the provided
  type without verifying."
  [form ty]
  `(ignore-with-unchecked-cast* ~form '~ty))

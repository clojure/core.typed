(ns cljs.core.typed
  "Internal functions for CLJS")

(defn ^:skip-wiki
  ann-form* 
  "Internal use only. Use ann-form."
  [form ty]
  form)

(defn ^:skip-wiki
  ann-protocol* 
  "Internal use only. Use ann-protocol."
  [vbnd varsym mth]
  nil)

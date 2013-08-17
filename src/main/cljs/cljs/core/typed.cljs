(ns cljs.core.typed
  "Internal functions for CLJS"
  (:require-macros 
    [clojure.core.typed.bootstrap-cljs :as boot]))

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

(defn ^:skip-wiki
  ann-datatype*
  "Internal use only. Use ann-datatype."
  [vbnd dname fields opts]
  nil)

(defn ^:skip-wiki
  def-alias* 
  "Internal use only. Use def-alias."
  [sym type]
  nil)

(defn ^:skip-wiki
  inst-poly 
  "Internal use only. Use inst."
  [inst-of types-syn]
  inst-of)

(defn ^:skip-wiki
  loop>-ann 
  "Internal use only. Use loop>"
  [loop-of bnding-types]
  loop-of)

(defn ^:skip-wiki
  typed-deps* 
  "Internal use only. Use typed-deps."
  [args]
  nil)

(defn ^:skip-wiki
  tc-ignore-forms* 
  "Internal use only. Use tc-ignore"
  [r]
  r)

; populate this namespace with core aliases
(boot/base-aliases)

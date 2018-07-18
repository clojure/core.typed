(ns cljs.core.typed
  "Internal functions for CLJS"
  (:refer-clojure :exclude [IFn])
  (:require-macros 
    [clojure.core.typed.bootstrap-cljs :as boot]))

(defn ^:skip-wiki
  ann* 
  "Internal use only. Use ann."
  [qsym typesyn check? form]
  nil)

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
  "Internal use only. Use defalias."
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

; populate this namespace with core aliases
(boot/base-aliases)

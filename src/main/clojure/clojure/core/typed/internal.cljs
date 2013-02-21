(ns typed.internal)

(defn ann-form-cljs* [form typ]
  form)

(defn print-env
  "Print the current type environment, and debug-string"
  [debug-string] nil)

(defn print-filterset
  "Print the filter set attached to form, and debug-string"
  [debug-string frm] 
  frm)

(defn inst-poly 
  [inst-of types-syn]
  inst-of)

(defn inst-poly-ctor [inst-of types-syn]
  inst-of)

(defn fn>-ann [fn-of param-types-syn]
  fn-of)

(defn pfn>-ann [fn-of polys param-types-syn]
  fn-of)

(defn loop>-ann [loop-of bnding-types]
  loop-of)


;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.unsafe)

;(ann ignore-with-unchecked-cast* [Any Any -> Any])
(defn ^:skip-wiki ignore-with-unchecked-cast* [form ty]
  form)

(defmacro ignore-with-unchecked-cast
  "Assumes the form is well typed and annotates it with the provided
  type without verifying."
  [form ty]
  `(ignore-with-unchecked-cast* ~form '~ty))

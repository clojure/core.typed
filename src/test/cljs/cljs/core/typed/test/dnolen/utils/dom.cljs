;From David Nolen's blog
(ns cljs.core.typed.test.dnolen.utils.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [cljs.core :refer [ISeq]])
  (:require-macros [cljs.core.typed :as t]))

(defn ^{:ann '[string -> (U nil js/HTMLElement)]}
  by-id [id]
  (.getElementById js/document id))

(defn ^{:ann '[js/HTMLElement string -> string]}
  set-html! [el s]
  (set! (.-innerHTML el) s))

(defn ^{:ann '[js/Element (U string number) -> js/Window]}
  set-text! [el s]
  (dom/setTextContent el s))

(defn ^{:ann '[(U js/Node nil) string -> Any]}
  set-class! [el name]
  (classes/set el name))

(defn ^{:ann '[js/Node (U nil string) -> boolean]}
  add-class! [el name]
  (classes/add el name))

(defn ^{:ann '[(U js/Node nil) (U nil string) -> boolean]}
  remove-class! [el name]
  (classes/remove el name))

(defn ^{:ann '[string -> [js/HTMLElement -> Any]]}
  tag-match [tag]
  (fn [el]
    (when-let [tag-name (.-tagName el)]
      (t/ann-form tag-name string)
      (= tag (.toLowerCase tag-name)))))

(defn ^{:ann '[Any -> [Any -> Any]]}
  el-matcher [el]
  (fn [other] (identical? other el)))

(defn ^{:ann '[(U nil js/Document js/Element) (U nil string) -> (U nil (ISeq js/Element))]}
  by-tag-name [el tag]
  (prim-seq (dom/getElementsByTagNameAndClass tag nil el)))

(defn ^{:ann '[(U nil js/Element) -> '[number number]]}
  offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

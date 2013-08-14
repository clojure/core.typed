;From David Nolen's blog
(ns cljs.core.typed.test.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes])
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

(defn el-matcher [el]
  (fn [other] (identical? other el)))

(defn by-tag-name [el tag]
  (prim-seq (dom/getElementsByTagNameAndClass tag nil el)))

(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

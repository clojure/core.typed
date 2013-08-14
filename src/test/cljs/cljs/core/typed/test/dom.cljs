;From David Nolen's blog
(ns cljs.core.typed.test.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]))

(defn by-id [id]
  (.getElementById js/document id))

(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(defn set-text! [el s]
  (dom/setTextContent el s))

(defn set-class! [el name]
  (classes/set el name))

(defn add-class! [el name]
  (classes/add el name))

(defn remove-class! [el name]
  (classes/remove el name))

(defn tag-match [tag]
  (fn [el]
    (when-let [tag-name (.-tagName el)]
      (= tag (.toLowerCase tag-name)))))

(defn el-matcher [el]
  (fn [other] (identical? other el)))

(defn by-tag-name [el tag]
  (prim-seq (dom/getElementsByTagNameAndClass tag nil el)))

(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

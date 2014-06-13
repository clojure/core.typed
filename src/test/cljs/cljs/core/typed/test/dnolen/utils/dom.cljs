;From David Nolen's blog
(ns cljs.core.typed.test.dnolen.utils.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes])
  (:require [cljs.core.typed :as t :refer-macros [ann]]))

(ann by-id [string -> (U nil js/HTMLElement)])
(defn by-id [id]
  (.getElementById js/document id))

(ann set-html! [js/HTMLElement string -> string])
(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(ann set-text! [js/Element (U string number) -> js/Window])
(defn set-text! [el s]
  (dom/setTextContent el s))

(ann set-class! [(U js/Node nil) string -> Any])
(defn set-class! [el name]
  (classes/set el name))

(ann add-class! [js/Node (U nil string) -> boolean])
(defn add-class! [el name]
  (classes/add el name))

(ann remove-class! [(U js/Node nil) (U nil string) -> boolean])
(defn remove-class! [el name]
  (classes/remove el name))

(ann tag-match [string -> [js/HTMLElement -> Any]])
(defn tag-match [tag]
  (fn [el]
    (when-let [tag-name (.-tagName el)]
      (t/ann-form tag-name string)
      (= tag (.toLowerCase tag-name)))))

(ann el-matcher [Any -> [Any -> Any]])
(defn el-matcher [el]
  (fn [other] (identical? other el)))

(ann by-tag-name [(U nil js/Document js/Element) (U nil string) 
                  -> (U nil (ISeq js/Element))])
(defn by-tag-name [el tag]
  (prim-seq (dom/getElementsByTagNameAndClass tag nil el)))

(ann offset [(U nil js/Element) -> '[number number]])
(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

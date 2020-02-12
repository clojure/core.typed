;From David Nolen's blog
(ns cljs.core.typed.test.dnolen.utils.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes])
  (:require [cljs.core.typed :as t :refer-macros [ann]]))

(ann by-id [t/JSString -> (t/U nil js/HTMLElement)])
(defn by-id [id]
  (.getElementById js/document id))

(ann set-html! [js/HTMLElement t/JSString -> t/JSString])
(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(ann set-text! [js/Element (t/U t/JSString t/JSNumber) -> js/Window])
(defn set-text! [el s]
  (dom/setTextContent el s))

(ann set-class! [(t/U js/Node nil) t/JSString -> t/Any])
(defn set-class! [el name]
  (classes/set el name))

(ann add-class! [(t/U nil js/Node) t/JSString -> t/JSBoolean])
(defn add-class! [el name]
  (classes/add el name))

(ann remove-class! [(t/U js/Node nil) (t/U nil t/JSString) -> t/JSBoolean])
(defn remove-class! [el name]
  (classes/remove el name))

(ann tag-match [t/JSString -> [js/HTMLElement -> t/Any]])
(defn tag-match [tag]
  (fn [el]
    (when-let [tag-name (.-tagName el)]
      (t/ann-form tag-name t/JSString)
      (= tag (.toLowerCase tag-name)))))

(ann el-matcher [t/Any -> [t/Any -> t/Any]])
(defn el-matcher [el]
  (fn [other] (identical? other el)))

(ann by-tag-name [(t/U nil js/Document js/Element) (t/U nil t/JSString) 
                  -> (t/U nil (ISeq js/Element))])
(defn by-tag-name [el tag]
  (prim-seq (dom/getElementsByTagNameAndClass tag nil el)))

(ann offset [(t/U nil js/Element) -> '[t/JSNumber t/JSNumber]])
(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

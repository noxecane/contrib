(ns contrib.ui.base
  (:require-macros [cljs.core.async.macros :refer [go-loop]]
                   [reagent.ratom :refer [reaction]])
  (:require [contrib.ui.core :as ui :refer [dosub]]
            [contrib.ui.utils :as utils]))


(defn header
  ([{:keys [brand href] :or {href "/"}}]
   [:header.nav.has-shadow.header
    [:div.container
     [:div.nav-left
      [:a.nav-item {:href href}
       [:h1.title.is-brand brand]]]]])

  ([{:keys [brand href] :or {href "/"}} & right]
   [:header.nav.has-shadow.header
    [:div.container
     [:div.nav-left
      [:a.nav-item {:href href}
       [:h1.title.is-brand brand]]]
     (into [:div.nav-toggle] (repeat 3 [:span]))
     (into [:div.nav-right.nav-menu] right)]]))


(defn button [channel {:keys [name] :as props} & children]
  (into [:button.button (merge props
                               {:type       "button"
                                :on-click   (utils/click-fn channel name)})]
        children))


(defn icon [{:keys [name size]}]
  [(utils/str->node "div" "icon" (if size (str "is-" size) ""))
   [(utils/str->node "i" "fa" (str "fa-" name))]])


(defn async-button [state node-in props & children]
  (let [loading? (reaction (:loading? @state))]
    (fn []
      (into [button node-in (utils/toggle-class props @loading? "is-loading")]
            children))))


(defn loading! [state v]
  (swap! state assoc :loading? v))

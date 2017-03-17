(ns contrib.ui.base
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [contrib.ui.core :as ui]))


(defn header
  ([{:keys [data-brand href] :or {href "/"}}]
   [:header.nav.has-shadow.header
    [:div.container
     [:div.nav-left
      [:a.nav-item {:href href}
       [:h1.title.is-brand data-brand]]]]])

  ([{:keys [data-brand href] :or {href "/"}} right-menu]
   [:header.nav.has-shadow.header
    [:div.container
     [:div.nav-left
      [:a.nav-item {:href href}
       [:h1.title.is-brand data-brand]]]
     [:div.nav-toggle
      (repeat 3 [:span])]
     [:div.nav-right.nav-menu
      right-menu]]]))


(defn button [channel {:keys [name] :as props} & children]\
  (into [:button.button (merge props
                               {:type       "button"
                                :on-click   (ui/click-fn channel name)})]
        children))


(defn async-button [state channel props & children]
  (let [loading? (reaction (:loading? @state))]
    (fn []
       [button channel (ui/toggle-class props @loading? "is-loading") children])))


(defn icon [{:keys [name size]}]
  [(ui/str->node "div" "icon" (str "is-" size))
   [(ui/str->node "i" "fa" (str "fa-" name))]])

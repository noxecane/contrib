(ns contrib.ui.router
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [accountant.core :as accountant]
            [contrib.ui.core :as ui :refer [dosub]]
            [reagent.core :as r]
            [secretary.core :as secretary]))


(defn use-secretary! []
  (accountant/configure-navigation! {:nav-handler  #(secretary/dispatch! %)
                                     :path-exists? #(secretary/locate-route %)}))


(defn route! [url]
  (accountant/navigate! url))


(defonce state (r/atom {:current [:div]}))
(def node-in (ui/node))

(dosub [[_ view] (ui/key-sub node-in :switch)]
  (swap! state assoc :current view))


(defn view []
  (let [current (reaction (:current @state))]
    (fn []
      [:div.route
       @current])))

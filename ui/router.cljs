(ns contrib.ui.router
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [accountant.core :as accountant]
            [contrib.ui.signal :as signal]
            [reagent.core :as r]
            [secretary.core :as secretary]))


(defn use-secretary! []
  (accountant/configure-navigation! {:nav-handler  #(secretary/dispatch! %)
                                     :path-exists? #(secretary/locate-route %)}))


(defonce state (r/atom {:current [:div]}))
(defonce channel (signal/channel))


(signal/listen
 (signal/on channel :switch)
 (fn [[_ view]]
   (swap! state assoc :current view)))


(defn view []
  (let [current (reaction (:current @state))]
    (fn []
      [:div.route
       @current])))

(ns contrib.ui.router
  (:require [accountant.core :as accountant]
            [contrib.ext.async :refer-macros [loop-chan-js]]
            [contrib.talk.emitter :as emitter]
            [reagent.core :as r]
            [reagent.ratom :refer-macros [reaction]]
            [secretary.core :as secretary]))


(defn use-secretary! []
  (accountant/configure-navigation! {:nav-handler  #(secretary/dispatch! %)
                                     :path-exists? #(secretary/locate-route %)}))


(defn route! [url]
  (accountant/navigate! url))


(defonce state (r/atom {:current [:div]}))
(def emitter (emitter/emitter))

(loop-chan-js [[_ view] (emitter/on-event emitter :switch)]
  (swap! state assoc :current view))


(defn view []
  (let [current (reaction (:current @state))]
    (fn []
      [:div.route
       @current])))

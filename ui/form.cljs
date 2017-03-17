(ns contrib.ui.form
  (:require-macros [cljs.core.async.macros :refer [go-loop]]
                   [reagent.ratom :refer [reaction]])
  (:require [cljs.core.async :as a]
            [contrib.ui.base :as base]
            [contrib.ui.core :as ui]
            [contrib.ui.signal :as signal]))


(defn form [& controls]
  (into [:form {:no-validate true}] controls))


(defn use-form-update! [state channel]
  (signal/listen
   (signal/on channel :update)
   (fn [[_ n v]]
     (swap! state assoc n v))))


(defn use-form-submit! [state channel]
  (signal/listen
   (signal/on channel :submit)
   (fn [[_ n v]]
     (swap! state assoc :loading? true))))


(defn text [state channel {:keys [name type] :or {type "text"} :as props}]
  (let [value (reaction (name @state))]
    (fn []
      [:input.input (merge props
                           {:type      type
                            :name      name
                            :on-change (ui/input-fn channel name)
                            :value     @value})])))


(defn submit [state channel {:keys [disabled]} & children]
  [base/async-button state channel (ui/add-class {:name     :submit
                                                  :disabled disabled}
                                                 "is-primary")
   children])

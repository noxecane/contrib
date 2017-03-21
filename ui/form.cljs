(ns contrib.ui.form
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [contrib.ui.base :as base]
            [contrib.ui.core :as ui :refer-macros [dosub]]
            [contrib.ui.utils :as utils]))



(defn- invalid? [node]
  (-> node .checkValidity not))


(defn- get-message [node]
  (.-validationMessage node))


(defn valid-form? [{errors :errors}]
  (every? not (vals errors)))


(defn setup-form! [state node-in]
  ;; auto-update model when values change
  (dosub [[_ n v] (ui/key-sub node-in :update)]
         (swap! state assoc-in [:values n] v))

  ;; setup form validation
  ;; TODO validation should be optional
  (dosub [[_ n v] (as-> :update i
                    (ui/key-filter i)
                    (ui/basket i)
                    (ui/on node-in i)
                    (ui/wait i (ui/box) 100))]
         (let [node (get-in @state [:nodes n])]
           (if (invalid? node)
             (swap! state assoc-in [:errors n] (get-message node))
             (swap! state assoc-in [:errors n] false)))))


(defn text [state channel {:keys [name type] :or {type "text"} :as props}]
  (let [value    (reaction (get-in @state [:values name]))
        invalid? (reaction (get-in @state [:errors name]))]
    (utils/attach-node state (fn []
                               [:input.input (-> props
                                                 (utils/toggle-class @invalid? "is-danger")
                                                 (merge {:type      type
                                                         :name      name
                                                         :on-change (utils/input-fn channel name)
                                                         :value     @value}))])
                       [:nodes name])))


(defn submit [state channel {:keys [disabled]} & children]
  [base/async-button state channel (utils/add-class {:name     :submit
                                                  :disabled disabled}
                                                 "is-primary")
   children])


(defn error [state {:keys [name]}]
  (let [valid? (reaction (get-in @state [:errors name]))]
    [:p.help.is-danger (utils/toggle-class {} (not @valid?) "is-hidden") @valid?]))

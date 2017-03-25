(ns contrib.ext.sync
  (:require [cljs.reader :as reader]
            [reagent.core :as r]))


(defn save-to-local [k v]
  (.setItem (aget js/window "localStorage") k (pr-str v)))


(defn load-from-local [k]
  (-> (.getItem (aget js/window "localStorage") k)
      str
      reader/read-string))


(defn autosave [state name]
  (letfn [(save [k a o n]
            (save-to-local name n))]
    (add-watch state :auto-save save)))


(defn autoview
  ([state]
   (autoview state (r/atom nil)))

  ([state ratom]
   (letfn [(review [k a o n]
             (reset! ratom n))]
     (reset! ratom @state)
     (add-watch state :auto-view review)
     ratom)))


(defn load-latest [name not-found]
  (let [state (atom not-found)
        value (load-from-local name)]
    (if value
      (reset! state value))
    (autosave state name)
    state))

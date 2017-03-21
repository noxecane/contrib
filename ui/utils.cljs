(ns contrib.ui.utils
  (:require [clojure.string :as s]
            [contrib.ui.core :as ui]
            [reagent.core :as r]))


(defn str->node
  "Combines a list of strings to form a keyword"
  [& combs]
  (->> combs
       (s/join ".")
       (keyword)))


(defn attach-node
  "Attach the node created by the render function to the state using
   the given list of keys"
  [state render keys]
  (r/create-class {:reagent-render render
                   :component-did-mount #(->> (r/dom-node %)
                                              (swap! state assoc-in keys))}))

(defn add-class
  "Append a new class to the pre-existing classes, if there is one."
  [{:keys [class-name] :as props} new-class]
  (assoc props :class-name (if class-name (str class-name " " new-class) new-class)))


(defn toggle-class [props trigger new-class]
  (if trigger (add-class props new-class) props))


(defn input-fn
  "Create an input function to dispatch update events to
  the stream using the format [$name $value] from input
  elements"
  [channel name]
  (let [notify-stream #(ui/emit channel [:update name %])]
   #(-> %
        (aget "target")
        (aget "value")
        (notify-stream))))


(defn click-fn
  "Create a function to dispatch events from button clicks.
  The events are dispatched without any arguments acting
  as some form of notification."
  [channel action]
  #(ui/emit channel [action]))


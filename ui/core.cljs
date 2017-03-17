(ns contrib.ui.core
  (:require [clojure.string :as s]
            [contrib.ui.signal :as signal]
            [reagent.core :as r]))


(defn str->node
  "Combines a list of strings to form a keyword"
  [& combs]
  (->> combs
       (s/join ".")
       (keyword)))


(defn add-class [{:keys [class-name] :as props} new-class]
  (assoc props :class-name (if class-name (str class-name " " new-class) new-class)))


(defn toggle-class [props trigger new-class]
  (if trigger (add-class props new-class) props))


(defn- by-id
  "Proxy to getElementById"
  [id]
  (js/document.getElementById id))


(defn embed
  "Mount the given react component on the HTML dom element
  with the given node-id"
  [main node-id]
  (r/render-component main (by-id node-id)))


(defn input-fn
  "Create an input function to dispatch update events to
  the stream using the format [$name $value] from input
  elements"
  [channel name]
  (let [notify-stream #(signal/emit channel [:update name %])]
   #(-> %
        (aget "target")
        (aget "value")
        (notify-stream))))


(defn click-fn
  "Create a function to dispatch events from button clicks.
  The events are dispatched without any arguments acting
  as some form of notification."
  [channel action]
  #(signal/emit channel [action]))

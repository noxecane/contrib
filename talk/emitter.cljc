(ns contrib.talk.emitter
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]))
  (:require #?(:cljs [cljs.core.async :as a])
            #?(:clj [clojure.core.async :as a :refer [go go-loop]])))


(defprotocol IEmitter
  (emit [s e]
    "Broadcasts an event to all subscribers")
  (on [s c]
    "Adds a channel to it's subscription list"))


(deftype ChannelEmitter [in out]
  IEmitter
  (emit [_ e]
    (a/put! in e))

  (on [_ c]
    (a/tap out c)
    c))


(defn emitter []
  (let [in  (a/chan 10)
        out (a/mult in)]
    (->ChannelEmitter in out)))


(defn by-event
  "Filter new messages by :event property(clojure) or the first element
   of the message vector(clojurescript)"
  [name]
  (filter
   #?(:clj #(-> % :event (= name))
      :cljs #(-> % first (= name)))))


(defn on-event
  "Create a subscription on the given emitter, that only receives messages
   that share the same event name"
  [emitter event]
  (on emitter (a/chan 10 (by-event event))))

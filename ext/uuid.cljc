(ns contrib.ext.uuid)


(defn random
  "Create a random UUID and convert it to string."
  []
  (str #?(:cljs (random-uuid)
          :clj (java.util.UUID/randomUUID))))

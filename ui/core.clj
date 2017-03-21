(ns contrib.ui.core)


(defmacro dosub {:style/indent 1}
  [binding & body]
  (let [[name expr] binding]
    `(let [chan# ~expr]
       (cljs.core.async.macros/go-loop [~name (cljs.core.async/<! chan#)]
         ~@body
         (recur (cljs.core.async/<! chan#))))))

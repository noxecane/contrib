(ns contrib.ui.notie)


(defn alert [type msg]
  (js/notie.alert (clj->js {:type type :text msg})))

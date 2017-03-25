(ns contrib.ui.core
  (:require [cljs.core.async :as a]
            [clojure.string :as s]
            [contrib.ext.async :as async :refer-macros [loop-chan-js]]
            [contrib.ext.core :as ext]
            [contrib.talk.emitter :as emitter]
            [reagent.core :as r]
            [reagent.ratom :refer-macros [reaction]]))


(defn mount
  "Mount the given react component on the HTML dom element
   with the given node-id"
  [main node-id]
  (->> (js/document.getElementById node-id)
       (r/render-component main)))


(defn str->node
  "Combines a list of strings to form a keyword"
  [& combs]
  (->> combs (s/join ".") keyword))


(defn get-node
  "Pass the node to f, when the component is mounted. Ensure that the
   node is not mutated as that might cause some problems with react"
  [render f]
  (r/create-class {:reagent-render render
                   :component-did-mount #(f (r/dom-node %))}))


(defn valid-form? [{errors :errors}]
  (every? not (vals errors)))


(defn add-class
  "Append a new class to the pre-existing classes, if there is one."
  [props cls]
  (letfn [(new-class [old new]
            (if (seq old)
              (str old " " new)
              new))]
    (update props :class-name new-class cls)))


(defn toggle-class
  "Based on the value of on?, add the class cls to the class list"
  [props on? cls]
  (if on?
    (add-class props cls)
    props))


(defn on-change
  "Emit :update events whenever a change happens"
  [emitter name]
  (letfn [(notify [value]
            (emitter/emit emitter [:update name value]))]
   #(-> % (aget "target") (aget "value") notify)))


(defn on-click
  "Emit the given action when a button is clicked"
  [emitter action]
  #(emitter/emit emitter [action]))


(defn icon [{:keys [name size]}]
  [(str->node "div" "icon" (if size (str "is-" size) ""))
   [(str->node "i" "fa" (str "fa-" name))]])


(defn header
  ([{:keys [brand href] :or {href "/"}}]
   [:header.nav.has-shadow.header
    [:div.container
     [:div.nav-left
      [:a.nav-item {:href href}
       [:h1.title.is-brand brand]]]]])

  ([{:keys [brand href] :or {href "/"}} & right]
   [:header.nav.has-shadow.header
    [:div.container
     [:div.nav-left
      [:a.nav-item {:href href}
       [:h1.title.is-brand brand]]]
     (into [:div.nav-toggle] (repeat 3 [:span]))
     (into [:div.nav-right.nav-menu]
           (map #(-> [:div.nav-item %]))
           right)]]))


(defn button [emitter {:keys [name] :as props} & children]
  (into [:button.button (merge props
                               {:type       "button"
                                :on-click   (on-click emitter name)})]
        children))


(defn async-button [state emitter props & children]
  (let [loading? (reaction (:loading? @state))]
    (fn []
      (into [button emitter (toggle-class props @loading? "is-loading")]
            children))))


(defn update$ [state emitter]
  (loop-chan-js [[_ n v] (emitter/on-event emitter :update)]
    (swap! state assoc-in [:values n] v)))


(defn validate$ [state emitter]
  (let [sub     (async/fixed-chan 10 (emitter/by-event :update))
        _       (emitter/on emitter sub)
        out     (async/pipeline sub (a/chan 10) 200)
        valid?  #(.checkValidity %)
        message #(aget % "validationMessage")]
    (loop-chan-js [[_ n v] out]
      (let [node (get-in @state [:nodes n])]
        (when node
          (if (valid? node)
            (swap! state assoc-in [:errors n] false)
            (swap! state assoc-in [:errors n] (message node))))))))


(defn text [state emitter {:keys [name type] :or {type "text"} :as props}]
  (let [value    (reaction (get-in @state [:values name]))
        invalid? (reaction (get-in @state [:errors name]))]
    (fn []
      [:input.input (-> props
                        (toggle-class @invalid? "is-danger")
                        (merge {:type      type
                                :name      name
                                :on-change (on-change emitter name)
                                :value     @value}))])))


(defn v-text [state emitter {name :name :as props}]
  (get-node #(-> [text state emitter props])
            #(swap! state assoc-in [:nodes name] %)))


(defn auto-options [state emitter {name :name} render]
  (let [suggestions (reaction (get-in state [:suggestions name]))
        selection   (reaction (get-in state [:values name]))
        visible?    (reaction (or (not-empty @suggestions)
                                  (nil? selection)))]
    (fn []
      (into [:ul.options.box {:class-name (if (not visible?) "is-hidden")}]
            (map (fn [[i s]]
                   [:li.option {:key      i
                                :on-click #(emitter/emit emitter [:select name s])}
                    [render s]]))
            (ext/vector-map @suggestions)))))


(defn suggest$ [state emitter search]
  (loop-chan-js [[_ n v] (emitter/on-event emitter :update)]
                (swap! state assoc-in [:suggestions n] (search n v))
                (js/console.log (clj->js (:suggestions @state)))))


(defn select$ [state emitter render]
  (loop-chan-js [[_ n v] (emitter/on-event emitter :select)]
                (swap! state assoc-in [:selections n] v)
                (swap! state assoc-in [:values n] (render n v))))


(defn textarea [state emitter {:keys [name] :as props}]
  (let [value    (reaction (get-in @state [:values name]))
        invalid? (reaction (get-in @state [:errors name]))]
    (fn []
      [:textarea.textarea (-> props
                              (toggle-class @invalid? "is-danger")
                              (merge {:name      name
                                      :on-change (on-change emitter name)
                                      :value     @value}))])))



(defn submit [state emitter {:keys [disabled]} & children]
  [async-button state emitter (add-class {:name     :submit
                                          :disabled disabled}
                                         "is-primary")
   children])


(defn error [state {:keys [name]}]
  (let [valid? (reaction (get-in @state [:errors name]))]
    [:p.help.is-danger (toggle-class {} (not @valid?) "is-hidden") @valid?]))


(defn submit-deck [state emitter props & children]
  [:div.is-clearfix
   [:div.is-pulled-right
    (into [submit state emitter props] children)]])


(defn open-modal$ [state emitter]
  (loop-chan-js [_ (emitter/on-event emitter :open)]
    (swap! state assoc :open? true)))


(defn close-modal$ [state emitter]
  (loop-chan-js [_ (emitter/on-event emitter :close)]
    (swap! state assoc :open? false)))


(defn modal-trigger [emitter props & children]
  (into [button emitter (assoc props :name :open)] children))


(defn modal [state emitter {:keys [size] :or {size "small"}} & children]
  (let [active? (reaction (:open? @state))]
    #(into [(str->node "div" "modal" (str "is-" size)) {:class-name (if @active? "is-active")}
            [:div.modal-background {:on-click (on-click emitter :close)}]
            (into [:div.modal-content.box] children)
            [button emitter {:name       :close
                             :class-name "modal-close"
                             :on-click   (on-click emitter :close)}]])))

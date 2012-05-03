(ns com.gigmesh.main
  (:use at.ac.tuwien.complang.flickr
        clojure.java.browse
        [fs.core :only (temp-file)]
        [hiccup.core :only (html)]
        [clojure.java.io :only (as-url)]
        [clojure.tools.logging :only (info error)]))

(def ^{:private true} user (atom nil))

(defn login []
  (let [api-key "2ce46a452bc05592d3c5124ac324770e"
        secret "deb41df7143847f1"
        info (request-authorization api-key secret)]
    (browse-url (:url info))
    (Thread/sleep 10000)
    (let [new (complete-authorization (:api-info info))]
      (swap! user (fn [old] new)))))

(defn photo-html [photo]
  [:a {:href (photopage-url photo)} [:img {:src (source-url photo :small)}]])

(defn photos-html-page [photos]
  [:html [:body (pmap (fn [p]
                        (list (photo-html p)
                              " "))
                      photos)]])

(defn browse-photos [photos]
  (let [file (temp-file "photos" ".html")]
    (spit file (html (photos-html-page photos)))
    (browse-url (str (as-url file)))))

;;; flickr.clj

;; flickr-clojure --- Flickr API bindings for Clojure

;; Copyright (C) 2009 Mark Probst

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns at.ac.tuwien.complang.flickr
  (:use clojure.contrib.def
	at.ac.tuwien.complang.flickr-api))

(defstruct minimal-instance :api-info :id)

(defn- minimal-instance? [x]
  (if (= (class x) clojure.lang.Ref)
    false
    (do
      (assert (:minimal-instances (:api-info x)))
      true)))

(defn- deref-instance [x]
  (if (minimal-instance? x)
    x
    (deref x)))

(defn id [instance]
  (:id (deref-instance instance)))

(defn api-info [instance]
  (:api-info (deref-instance instance)))

(defn- capitalize-string [s]
  (str (. Character toUpperCase (nth s 0)) (subs s 1)))

(defn- fetch-if-necessary [instance slot fetcher]
  (if (minimal-instance? instance)
    (fetcher instance)
    (let [value (get (deref instance) slot)]
      (if (= value :unfetched)
	(let [fetched-value (fetcher instance)]
	  (dosync
	   (if (= (get (deref instance) slot) :unfetched)
	     (do
	       (ref-set instance (assoc (deref instance) slot fetched-value))
	       fetched-value)
	     (get (deref instance) slot))))
	value))))

(defmacro- def-multi-page-fetcher [entity what converter fetch-page]
  (let [[entity] entity
	fetcher-name (symbol (str "fetch-" entity "-" what))]
    `(defn- ~fetcher-name [~entity]
       (let [~'api-info (api-info ~entity)
	     api-seq# (collect-pages (fn [~'per-page ~'page]
				       ~fetch-page)
				     500 1)]
	 (map (fn [x#] (~converter ~'api-info x#)) api-seq#)))))

(defn- deconstruct-source [src]
  (let [count (count src)]
    (if (and (>= count 3) (= (nth src (- count 3)) :custom))
      [(take (- count 3) src) (nth src (- count 2)) (nth src (- count 1))]
      [src nil nil])))

(defn- source-slots [src]
  (let [[auto-slots custom-slots getter] (deconstruct-source src)]
    (concat auto-slots custom-slots)))

(defn- lookup-or-create-instance [api-info type id creator]
  (let [creator (if (:minimal-instances api-info)
		  (fn [] (with-meta (creator) {:type type}))
		  (fn [] (ref (with-meta (creator) {:type type}))))]
    (if (:instance-maps api-info)
      (dosync
       (let [instance-maps (deref (:instance-maps api-info))]
	 (if-let [instance-map (instance-maps type)]
	   (or (instance-map id)
	       (let [instance (creator)]
		 (ref-set (:instance-maps api-info)
			  (assoc instance-maps type
				 (assoc instance-map id instance)))
		 instance))
	   (let [instance (creator)]
	     (ref-set (:instance-maps api-info)
		      (assoc instance-maps type {id instance}))
	     instance))))
      (creator))))

(defmacro- defapiclass [class-name & keyvals]
  (let [{sources :sources [fetch-struct fetch-func] :fetcher
	 custom-fetchers :custom-fetchers extra-slots :extra-slots} (apply hash-map keyvals)
	make-taker-name (fn [struct-name]
			  (symbol (str class-name "-take-values-from-" struct-name)))
	make-slot-fetcher-name (fn [slot-name] (symbol (str "fetch-" class-name "-" slot-name)))
	have-fetcher (if fetch-struct true false)
	source-slot-names (distinct (mapcat source-slots (vals sources)))
	slot-names (concat custom-fetchers source-slot-names extra-slots)
	slot-keywords (map #(keyword (name %)) slot-names)
	struct-names (keys sources)
	constructor-name (symbol (str "make-" class-name))
	fetcher-name (symbol (str "fetch-" class-name))
	defstruct-name (symbol (str class-name "-struct"))
	type-keyword (keyword (str (ns-name *ns*)) (capitalize-string (str class-name)))
	slot-constructors (mapcat #(list % :unfetched) slot-keywords)
	print-prefix (str "#<" (capitalize-string (str class-name)) ": ")]
    (concat
     '(do)
     (map (fn [slot-name]
	    `(defmulti ~slot-name (fn [i#] (type (deref-instance i#)))))
	  (filter #(not (contains? (ns-interns *ns*) %)) slot-names))
     (map (fn [fetcher-name]
	    `(defvar- ~fetcher-name))
	  (filter #(and (not (nil? %)) (not (contains? (ns-interns *ns*) %)))
		  (concat (map make-slot-fetcher-name custom-fetchers)
			  (map #(nth (deconstruct-source %) 2) (vals sources)))))
     `((defstruct ~defstruct-name :api-info :id ~@slot-keywords)
       (defn ~constructor-name [api-info# id#]
	 (let [creator# (if (and ~have-fetcher (:minimal-instances api-info#))
			  (fn [] (struct-map minimal-instance :api-info api-info# :id id#))
			  (fn [] (struct-map ~defstruct-name :api-info api-info# :id id# ~@slot-constructors)))]
	   (lookup-or-create-instance api-info# ~type-keyword id# creator#)))
       (defmethod print-method ~type-keyword [instance# w#]
	 (. w# write ~print-prefix)
	 (. w# write (:id instance#))
	 (. w# write ">")))
     (mapcat (fn [struct-name]
	       (let [taker-name (make-taker-name struct-name)
		     maker-name (symbol (str "make-" class-name "-from-" struct-name))
		     [auto-slots custom-slots getter] (deconstruct-source (sources struct-name))
		     getter-call (if getter
				   `(~getter ~'instance ~'struct)
				   'nil)
		     auto-keyvals (mapcat (fn [slot]
					    (let [kw (keyword (name slot))]
					      `(~kw (~kw ~'struct))))
					  auto-slots)
		     custom-keyvals (mapcat (fn [slot]
					      (let [kw (keyword (name slot))]
						`(~kw ~slot)))
					    custom-slots)]
		 `((defn- ~taker-name [~'instance ~'struct]
		     (let [[~@custom-slots] ~getter-call]
		       (if (minimal-instance? ~'instance)
			 (assoc ~'instance ~@auto-keyvals ~@custom-keyvals)
			 (dosync
			  (let [new-instance# (assoc (deref ~'instance) ~@auto-keyvals ~@custom-keyvals)]
			    (ref-set ~'instance new-instance#)
			    ~'instance)))))
		   (defn- ~maker-name [api-info# struct#]
		     (let [instance# (~constructor-name api-info# (:id struct#))]
		       (if (and ~have-fetcher (:minimal-instances api-info#))
			 instance#
			 (~taker-name instance# struct#)))))))
	     struct-names)
     (when fetch-struct
       `((defn- ~fetcher-name [instance#]
	   (let [deref-instance# (deref-instance instance#)
		 struct# (~fetch-func (:api-info deref-instance#) (:id deref-instance#))]
	     (~(make-taker-name fetch-struct) instance# struct#)))))
     (map (fn [slot-name]
	    (let [slot-keyword (keyword (name slot-name))]
	      (if fetch-struct
		`(defmethod ~slot-name ~type-keyword [instance#]
		   (if (minimal-instance? instance#)
		     (let [instance# (~fetcher-name instance#)]
		       (~slot-keyword instance#))
		     (dosync
		      (let [deref-instance# (deref instance#)
			    value# (~slot-keyword deref-instance#)]
			(if (= value# :unfetched)
			  (do
			    (~fetcher-name instance#)
			    (~slot-keyword (deref instance#)))
			  value#)))))
		`(defmethod ~slot-name ~type-keyword [instance#]
		   (~slot-keyword (deref-instance instance#))))))
	  source-slot-names)
     (map (fn [slot-name]
	    (let [slot-keyword (keyword (name slot-name))
		  slot-fetcher-name (make-slot-fetcher-name slot-name)]
	      `(defmethod ~slot-name ~type-keyword [instance#]
		 (fetch-if-necessary instance# ~slot-keyword ~slot-fetcher-name))))
	  custom-fetchers))))

(defapiclass user
  :sources {flickr-person
	    (username realname location ispro)
	    flickr-user
	    (username)
	    flickr-contact
	    (username realname)
	    flickr-public-contact
	    (username)}
  :fetcher [flickr-person people-get-info]
  :custom-fetchers [photos photosets groups contacts favorites])

;;MISSING: num-views, num-favs
(defapiclass photo
  :sources {flickr-full-photo
	    (secret server isfavorite license rotation
		    title description
		    ispublic isfriend isfamily
		    posted taken takengranularity lastupdate
		    permcomment permaddmeta
		    cancomment canaddmeta
		    urls
		    :custom (owner notes tags) get-photo-owner-notes-tags-from-flickr-full-photo)
	    flickr-search-photo
	    (secret server title
		    ispublic isfriend isfamily
		    :custom (owner) get-owner)
	    flickr-photoset-photo
	    (secret server title)
	    flickr-favorite
	    (secret server title
		    ispublic isfriend isfamily
		    :custom (owner) get-owner)}
  :fetcher [flickr-full-photo photos-get-info]
  :custom-fetchers [sizes comments sets groups]
  :extra-slots [contexts])

(defapiclass photoset
  :sources {flickr-photoset
	    (primary secret server title description)
	    flickr-photoset-info
	    (primary title description
		     :custom (owner) get-owner)
	    flickr-context-set
	    (title)}
  :fetcher [flickr-photoset-info photosets-get-info]
  :custom-fetchers [photos])

(defapiclass group
  :sources {flickr-group
	    (title description privacy)
	    flickr-list-group
	    (title)
	    flickr-context-pool
	    (title)}
  :fetcher [flickr-group groups-get-info]
  :custom-fetchers [photos])

(defapiclass comment
  :sources {flickr-comment
	    (date-create permalink text
			 :custom (author) get-author)})

(defapiclass note
  :sources {flickr-note
	    (x y w h text
	       :custom (author) get-author)})

(defapiclass tag
  :sources {flickr-tag
	    (raw text
		 :custom (author) get-author)})

;;; generic photos search

(defn- substitute-keyvals [m & keyvals]
  (if (seq keyvals)
    (let [[k func & keyvals-rest] keyvals]
      (if (contains? m k)
	(let [[nk nv] (func (get m k))]
	  (apply substitute-keyvals (assoc (dissoc m k) nk nv) keyvals-rest))
	(apply substitute-keyvals m keyvals-rest)))
    m))

(defn search-photos [api-info & keyvals]
  (let [keyvals-map (apply hash-map keyvals)
	new-keyvals-map (substitute-keyvals keyvals-map :user #(vector :user-id (id %)))
	new-keyvals (mapcat #(apply list %) new-keyvals-map)]
    (map #(make-photo-from-flickr-search-photo api-info %)
	 (collect-pages (fn [per-page page] (apply photos-search api-info per-page page new-keyvals)) 500 1))))

;;; find user

(defn find-user-by-username [api-info username]
  (make-user-from-flickr-user api-info (people-find-by-username api-info username)))

;;; generic getters

(defn- get-owner [instance struct]
  [(make-user (api-info instance) (:owner struct))])

(defn- get-author [instance struct]
  [(make-user (api-info instance) (:author struct))])

;;; user fetchers

(def-multi-page-fetcher [user]
  photos make-photo-from-flickr-search-photo
  (photos-search api-info per-page page :user-id (id user)))

(defn- fetch-user-photosets [user]
  (map #(make-photoset-from-flickr-photoset (api-info user) %)
       (photosets-get-list (api-info user) (id user))))

(defn- fetch-user-groups [user]
  (map #(make-group-from-flickr-list-group (api-info user) %)
       (people-get-public-groups (api-info user) (id user))))

(def-multi-page-fetcher [user]
  contacts make-user-from-flickr-public-contact
  (contacts-get-public-list api-info (id user) per-page page))

(def-multi-page-fetcher [user]
  favorites make-photo-from-flickr-favorite
  (favorites-get-public-list api-info (id user) per-page page))

;;; photoset fetchers and getters

(defn- fetch-photoset-photos [photoset]
  (map #(make-photo-from-flickr-photoset-photo (api-info photoset) %)
       (photosets-get-photos (api-info photoset) (id photoset))))

(defn- get-photo-owner-notes-tags-from-flickr-full-photo [instance struct]
  (let [api-info (api-info instance)]
    [(make-user api-info (:owner struct))
     (map #(make-note-from-flickr-note api-info %) (:notes struct))
     (map #(make-tag-from-flickr-tag api-info %) (:tags struct))]))

;;; group fetchers

(def-multi-page-fetcher [group]
  photos make-photo-from-flickr-search-photo
  (groups-pools-get-photos api-info (id group) per-page page nil))

;;; photo fetchers

(defn- fetch-photo-sizes [photo]
  (photos-get-sizes (api-info photo) (id photo)))

(defn- fetch-photo-comments [photo]
  (map #(make-comment-from-flickr-comment (api-info photo) %)
       (photos-comments-get-list (api-info photo) (id photo))))

(defn- photo-contexts [photo]
  (fetch-if-necessary (:contexts photo) (photos-get-all-contexts (api-info photo) (id photo))))

(defn- fetch-photo-sets [photo]
  (let [api-info (api-info photo)
	contexts (photo-contexts photo)]
    (map #(make-photoset-from-flickr-context-set api-info %)
	 (filter #(= (:type %) :set) contexts))))

(defn- fetch-photo-groups [photo]
  (let [api-info (api-info photo)
	contexts (photo-contexts photo)]
    (map #(make-group-from-flickr-context-pool api-info %)
	 (filter #(= (:type %) :pool) contexts))))

;;MISSING: contact-info

(def request-authorization api-request-authorization)

(defn complete-authorization [api-info & rest]
  (let [{persistence :persistence minimal-instances :minimal-instances do-cache :do-cache
	 :or {persistence nil minimal-instances false do-cache true}} (apply hash-map rest)
	api-info (assoc (api-complete-authorization api-info persistence) :minimal-instances minimal-instances)
	api-info (if do-cache
		   (assoc api-info :instance-maps (ref (hash-map)))
		   api-info)]
    (make-user api-info (:nsid (:user api-info)))))

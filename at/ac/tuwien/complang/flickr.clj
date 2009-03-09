(ns at.ac.tuwien.complang.flickr
  (:use clojure.contrib.def
	at.ac.tuwien.complang.flickr-api))

(def id :id)

(defn- capitalize-string [s]
  (str (. Character toUpperCase (nth s 0)) (subs s 1)))

(defmacro- fetch-if-necessary [ref fetch]
  `(let [ref-name# ~ref]
     (if-let [value# (deref ref-name#)]
       value#
       (let [new-value# ~fetch]
	 (dosync
	  (if-let [value# (deref ref-name#)]
	    value#
	    (do
	      (ref-set ref-name# new-value#)
	      new-value#)))))))

(defmacro- def-multi-page-fetcher [entity what converter fetch-page]
  (let [[entity] entity
	fetcher-name (symbol (str "fetch-" entity "-" what))
	api-info 'api-info
	per-page 'per-page
	page 'page]
    `(defn- ~fetcher-name [~entity]
       (let [~api-info (:api-info ~entity)
	     api-seq# (collect-pages (fn [~per-page ~page]
				       ~fetch-page)
				     500 1)]
	 (map (fn [x#] (~converter ~api-info x#)) api-seq#)))))

(defn- deconstruct-source [src]
  (let [count (count src)]
    (if (and (>= count 3) (= (nth src (- count 3)) :custom))
      [(take (- count 3) src) (nth src (- count 2)) (nth src (- count 1))]
      [src nil nil])))

(defn- source-slots [src]
  (let [[auto-slots custom-slots getter] (deconstruct-source src)]
    (concat auto-slots custom-slots)))

(defn- lookup-or-create-instance [api-info type id creator]
  (dosync
   (let [creator (fn [] (with-meta (merge {:api-info api-info :id id} (creator)) {:type type}))
	 instance-maps (deref (:instance-maps api-info))]
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
	 instance)))))

(defmacro- defapiclass [class-name & keyvals]
  (let [{sources :sources [fetch-struct fetch-func] :fetcher
	 custom-fetchers :custom-fetchers extra-slots :extra-slots} (apply hash-map keyvals)
	make-taker-name (fn [struct-name]
			  (symbol (str class-name "-take-values-from-" struct-name)))
	make-slot-fetcher-name (fn [slot-name] (symbol (str "fetch-" class-name "-" slot-name)))
	source-slot-names (apply concat (map source-slots (vals sources)))
	slot-names (concat custom-fetchers source-slot-names extra-slots)
	struct-names (keys sources)
	constructor-name (symbol (str "make-" class-name))
	fetcher-name (symbol (str "fetch-" class-name))
	type-keyword (keyword (str (ns-name *ns*)) (capitalize-string (str class-name)))
	slot-constructors (mapcat (fn [element-name]
				    (list (keyword (name element-name)) '(ref nil)))
				  slot-names)
	print-prefix (str "#<" (capitalize-string (str class-name)) ": ")]
    (concat
     '(do)
     (map (fn [slot-name]
	    `(defmulti ~slot-name type))
	  (filter #(not (contains? (ns-interns *ns*) %)) slot-names))
     (map (fn [fetcher-name]
	    `(defvar- ~fetcher-name))
	  (filter #(and (not (nil? %)) (not (contains? (ns-interns *ns*) %)))
		  (concat (map make-slot-fetcher-name custom-fetchers)
			  (map #(nth (deconstruct-source %) 2) (vals sources)))))
     `((defn ~constructor-name [api-info# id#]
	 (lookup-or-create-instance api-info# ~type-keyword id# (fn [] (hash-map ~@slot-constructors))))
       (defmethod print-method ~type-keyword [instance# w#]
	 (. w# write ~print-prefix)
	 (. w# write (id instance#))
	 (. w# write ">")))
     (mapcat (fn [struct-name]
	       (let [taker-name (make-taker-name struct-name)
		     maker-name (symbol (str "make-" class-name "-from-" struct-name))
		     [auto-slots custom-slots getter] (deconstruct-source (sources struct-name))
		     instance 'instance
		     struct 'struct
		     getter-call (if getter
				   `(~getter ~instance ~struct)
				   'nil)
		     auto-elements (map #(keyword (name %)) auto-slots)
		     auto-element-sets (map (fn [kw]
					      `(ref-set (~kw ~instance) (~kw ~struct)))
					    auto-elements)
		     custom-elements (map #(keyword (name %)) custom-slots)
		     custom-element-sets (map (fn [var kw]
						`(ref-set (~kw ~instance) ~var))
					      custom-slots custom-elements)]
		 `((defn- ~taker-name [~instance ~struct]
		     (let [[~@custom-slots] ~getter-call]
		       (dosync
			~@auto-element-sets
			~@custom-element-sets)
		       ~instance))
		   (defn- ~maker-name [api-info# struct#]
		     (~taker-name (~constructor-name api-info# (:id struct#)) struct#)))))
	     struct-names)
     (when fetch-struct
       `((defn- ~fetcher-name [instance#]
	   (let [struct# (~fetch-func (:api-info instance#) (:id instance#))]
	     (~(make-taker-name fetch-struct) instance# struct#)
	     instance#))))
     (map (fn [slot-name]
	    (let [slot-keyword (keyword (name slot-name))]
	      (if fetch-struct
		`(defmethod ~slot-name ~type-keyword [instance#]
		   (dosync
		    (or (deref (~slot-keyword instance#))
			(do
			  (~fetcher-name instance#)
			  (deref (~slot-keyword instance#))))))
		`(defmethod ~slot-name ~type-keyword [instance#]
		   (deref (~slot-keyword instance#))))))
	  source-slot-names)
     (map (fn [slot-name]
	    (let [slot-keyword (keyword (name slot-name))
		  slot-fetcher-name (make-slot-fetcher-name slot-name)]
	      `(defmethod ~slot-name ~type-keyword [instance#]
		 (fetch-if-necessary (~slot-keyword instance#) (~slot-fetcher-name instance#)))))
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

;;; generic getters

(defn- get-owner [instance struct]
  [(make-user (:api-info instance) (:owner struct))])

(defn- get-author [instance struct]
  [(make-user (:api-info instance) (:author struct))])

;;; user fetchers

(def-multi-page-fetcher [user]
  photos make-photo-from-flickr-search-photo
  (photos-search api-info per-page page :user-id (id user)))

(defn- fetch-user-photosets [user]
  (map #(make-photoset-from-flickr-photoset (:api-info user) %)
       (photosets-get-list (:api-info user) (id user))))

(defn- fetch-user-groups [user]
  (map #(make-group-from-flickr-list-group (:api-info user) %)
       (people-get-public-groups (:api-info user) (id user))))

(def-multi-page-fetcher [user]
  contacts make-user-from-flickr-public-contact
  (contacts-get-public-list api-info (id user) per-page page))

(def-multi-page-fetcher [user]
  favorites make-photo-from-flickr-favorite
  (favorites-get-public-list api-info (id user) per-page page))

;;; photoset fetchers and getters

(defn- fetch-photoset-photos [photoset]
  (map #(make-photo-from-flickr-photoset-photo (:api-info photoset) %)
       (photosets-get-photos (:api-info photoset) (id photoset))))

(defn- get-photo-owner-notes-tags-from-flickr-full-photo [instance struct]
  (let [api-info (:api-info instance)]
    [(make-user api-info (:owner struct))
     (map #(make-note-from-flickr-note api-info %) (:notes struct))
     (map #(make-tag-from-flickr-tag api-info %) (:tags struct))]))

;;; group fetchers

(def-multi-page-fetcher [group]
  photos make-photo-from-flickr-search-photo
  (groups-pools-get-photos api-info (id group) per-page page nil))

;;; photo fetchers

(defn- fetch-photo-sizes [photo]
  (photos-get-sizes (:api-info photo) (id photo)))

(defn- fetch-photo-comments [photo]
  (map #(make-comment-from-flickr-comment (:api-info photo) %)
       (photos-comments-get-list (:api-info photo) (id photo))))

(defn- photo-contexts [photo]
  (fetch-if-necessary (:contexts photo) (photos-get-all-contexts (:api-info photo) (id photo))))

(defn- fetch-photo-sets [photo]
  (let [api-info (:api-info photo)
	contexts (photo-contexts photo)]
    (map #(make-photoset-from-flickr-context-set api-info %)
	 (filter #(= (:type %) :set) contexts))))

(defn- fetch-photo-groups [photo]
  (let [api-info (:api-info photo)
	contexts (photo-contexts photo)]
    (map #(make-group-from-flickr-context-pool api-info %)
	 (filter #(= (:type %) :pool) contexts))))

;;MISSING: contact-info

(ns-unmap *ns* 'request-authorization)
(def request-authorization at.ac.tuwien.complang.flickr-api/request-authorization)

(ns-unmap *ns* 'complete-authorization)
(defn complete-authorization [api-info]
  (let [api-info (at.ac.tuwien.complang.flickr-api/complete-authorization api-info)
	api-info (into api-info {:instance-maps (ref (hash-map))})]
    (make-user api-info (:nsid (:user api-info)))))

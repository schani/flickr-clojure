(def id :id)

(defn capitalize-string [s]
  (str (. Character toUpperCase (nth s 0)) (subs s 1)))

(defmacro fetch-if-necessary [ref fetch]
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

(defmacro def-multi-page-fetcher [entity what converter fetch-page]
  (let [[entity] entity
	fetcher-name (symbol (str "fetch-" entity "-" what))
	api-info 'api-info
	per-page 'per-page
	page 'page]
    `(defn ~fetcher-name [~entity]
       (let [~api-info (:api-info ~entity)
	     api-seq# (collect-pages (fn [~per-page ~page]
				       ~fetch-page)
				     50 1)]
	 (map (fn [x#] (~converter ~api-info x#)) api-seq#)))))

(defmacro defapiclass [class-name & keyvals]
  (let [{sources :sources [fetch-struct fetch-func] :fetcher custom-fetchers :custom-fetchers} (apply hash-map keyvals)
	make-taker-name (fn [struct-name]
			  (symbol (str class-name "-take-values-from-" struct-name)))
	make-slot-fetcher-name (fn [slot-name] (symbol (str "fetch-" class-name "-" slot-name)))
	source-slot-names (apply concat (vals sources))
	slot-names (concat custom-fetchers source-slot-names)
	struct-names (keys sources)
	constructor-name (symbol (str "make-" class-name))
	fetcher-name (symbol (str "fetch-" class-name))
	type-keyword (keyword (str (ns-name *ns*)) (capitalize-string (str class-name)))
	slot-constructors (mapcat (fn [element-name]
				    (list (keyword (name element-name)) '(ref nil)))
				  slot-names)]
    (concat
     '(do)
     (map (fn [slot-name]
	    `(defmulti ~slot-name :entity-type))
	  (filter #(not (contains? (ns-interns *ns*) %)) slot-names))
     `((defn ~constructor-name [api-info# id#]
	 (merge {:entity-type ~type-keyword :api-info api-info# :id id#} (hash-map ~@slot-constructors))))
     (mapcat (fn [struct-name]
	       (let [taker-name (make-taker-name struct-name)
		     maker-name (symbol (str "make-" class-name "-from-" struct-name))
		     elements (map #(keyword (name %)) (sources struct-name))
		     instance 'instance
		     struct 'struct
		     element-sets (map (fn [kw]
					 `(ref-set (~kw ~instance) (~kw ~struct)))
				       elements)]
		 `((defn ~taker-name [~instance ~struct]
		     (dosync
		      ~@element-sets)
		     ~instance)
		   (defn ~maker-name [api-info# struct#]
		     (~taker-name (~constructor-name api-info# (:id struct#)) struct#)))))
	     struct-names)
     `((defn ~fetcher-name [instance#]
	 (let [struct# (~fetch-func (:api-info instance#) (:id instance#))]
	   (~(make-taker-name fetch-struct) instance# struct#)
	   instance#)))
     (map (fn [slot-name]
	    (let [slot-keyword (keyword (name slot-name))]
	      `(defmethod ~slot-name ~type-keyword [instance#]
		 (dosync
		  (or (deref (~slot-keyword instance#))
		      (do
			(~fetcher-name instance#)
			(deref (~slot-keyword instance#))))))))
	  source-slot-names)
     (map (fn [fetcher-name]
	    `(def ~fetcher-name))
	  (filter #(not (contains? (ns-interns *ns*) %))
		  (map make-slot-fetcher-name custom-fetchers)))
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

;;MISSING: owner, notes, tags, sets, groups, sizes, comments, num-views, num-favs
(defapiclass photo
  :sources {flickr-full-photo
	    (secret server isfavorite license rotation
		    title description
		    ispublic isfriend isfamily
		    posted taken takengranularity lastupdate
		    permcomment permaddmeta
		    cancomment canaddmeta
		    urls)
	    flickr-search-photo
	    (secret server title
		    ispublic isfriend isfamily)
	    flickr-photoset-photo
	    (secret server title)
	    flickr-favorite
	    (secret server title
		    ispublic isfriend isfamily)}
  :fetcher [flickr-full-photo photos-get-info])

;;MISSING: owner, photos
(defapiclass photoset
  :sources {flickr-photoset
	    (primary secret server title description)
	    flickr-photoset-info
	    (primary title description)
	    flickr-context-set
	    (title)}
  :fetcher [flickr-photoset-info photosets-get-info])

(defapiclass group
  :sources {flickr-group
	    (title description privacy)
	    flickr-list-group
	    (title)
	    flickr-context-pool
	    (title)}
  :fetcher [flickr-group groups-get-info]
  :custom-fetchers [photos])

;;; photo fetchers

(def-multi-page-fetcher [user]
  photos make-photo-from-flickr-search-photo
  (photos-search api-info per-page page :user-id (id user)))

(defn fetch-user-photosets [user]
  (map #(make-photoset-from-flickr-photoset (:api-info user) %)
       (photosets-get-list (:api-info user) (id user))))

(defn fetch-user-groups [user]
  (map #(make-group-from-flickr-list-group (:api-info user) %)
       (people-get-public-groups (:api-info user) (id user))))

(def-multi-page-fetcher [user]
  contacts make-user-from-flickr-public-contact
  (contacts-get-public-list api-info (id user) per-page page))

(def-multi-page-fetcher [user]
  favorites make-photo-from-flickr-favorite
  (favorites-get-public-list api-info (id user) per-page page))

;;; group fetchers

(def-multi-page-fetcher [group]
  photos make-photo-from-flickr-search-photo
  (groups-pools-get-photos api-info (id group) per-page page nil))

;;MISSING: contact-info

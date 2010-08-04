;;; flickr_api.clj

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

(ns at.ac.tuwien.complang.flickr-api
  (:import [org.apache.xmlrpc.client XmlRpcClient XmlRpcClientConfigImpl]
	   [java.io ByteArrayInputStream])
  (:use at.ac.tuwien.complang.utils
	clojure.xml
	clojure.contrib.seq-utils
	clojure.contrib.fcase
	clojure.contrib.def))

(defmulti persistence-get (fn [p key] (type p)))
(defmulti persistence-put (fn [p key val] (type p)))

(defvar- xml-rpc-client
     (let [config (XmlRpcClientConfigImpl.)
	   client (XmlRpcClient.)
	   url (java.net.URL. "http://www.flickr.com/services/xmlrpc/")]
       (. config setServerURL url)
       (. config setConnectionTimeout 2000)
       (. config setReplyTimeout 15000)
       (. client setConfig config)
       client))

(defn- xml-tag [xml]
  (:tag xml))

(defn- xml-body [xml]
  (apply str (:content xml)))

(defn- xml-children [xml]
  (:content xml))

(defn- xml-child [tag xml]
  (first (filter #(= (:tag %) tag) (xml-children xml))))

(defn- xml-attrib [attrib xml]
  (attrib (:attrs xml)))

(defn- xml-follow-path [xml path]
  (cond (keyword? path) (xml-attrib path xml)
	(empty? path) xml
	true
	 (let [f (first path)]
	   (cond (= f :body) (xml-body xml)
		 (= f :attrib) (xml-attrib (second path) xml)
		 (= f :child) (xml-follow-path (xml-child (second path) xml) (rest (rest path)))
		 true (throw (Exception. (str "Illegal XML Path " path)))))))

(defn- convert-type [value type]
  (cond (string? value) (case type
			  :string value
			  :integer (if (empty? value) :unknown (BigInteger. value))
			  :boolean (if (empty? value) :unknown (not (zero? (BigInteger. value))))
			  (throw (Exception. (str "Illegal type " type))))
	(nil? value) :unknown
	true (throw (Exception. (str "Value to be converted (" value ") must be string or nil")))))

(defmacro- defapistruct [name & members]
  (let [parser-name (symbol (str "make-" name))
	xml-arg (gensym)
	member-inits (mapcat (fn [member]
			       (let [[name path & type-list] member
				     type (or (first type-list) :string)
				     member-keyword (keyword (str name))]
				 (if (and (list? path) (includes? '(fn fn*) (first path)))
				   `(~member-keyword (~path ~xml-arg))
				   `(~member-keyword (convert-type (xml-follow-path ~xml-arg '~path) ~type)))))
			     members)]
    `(defn- ~parser-name [~xml-arg]
       (sorted-map ~@member-inits))))

(defapistruct flickr-comment
  (id :id)
  (author :author)
  (authorname :authorname)
  (date-create :date_create)
  (permalink :permalink)
  (text (:body)))

(defapistruct flickr-contact
  (id :nsid)
  (username :username)
  (realname :realname)
  (isfriend :friend :boolean)
  (isfamily :family :boolean)
  (ignored :ignored :boolean))

(defapistruct flickr-context-set
  (id :id)
  (title :title))

(defapistruct flickr-context-pool
  (id :id)
  (title :title))

(defapistruct flickr-favorite
  (id :id)
  (owner :owner)
  (secret :secret)
  (server :server)
  (farm :farm)
  (title :title)
  (ispublic :ispublic :boolean)
  (isfriend :isfriend :boolean)
  (isfamily :isfamily :boolean))

(defapistruct flickr-group
  (id :id)
  (title (:child :name :body))
  (description (:child :description :body))
  (members (:child :members :body) :integer)
  (privacy (:child :privacy :body)))

(defapistruct flickr-url-group
  (id :id)
  (title (:child :groupname :body)))

(defapistruct flickr-list-group
  (id :nsid)
  (title :name)
  (admin :admin)
  (eighteenplus :eighteenplus :boolean))

(defapistruct flickr-note
  (id :id)
  (author :author)
  (authorname :authorname)
  (x :x :integer)
  (y :y :integer)
  (w :w :integer)
  (h :h :integer)
  (text (:body)))

(defapistruct flickr-person
  (id :nsid)
  (isadmin :isadmin :boolean)
  (ispro :ispro :boolean)
  (iconserver :iconserver)
  (username (:child :username :body))
  (realname (:child :realname :body))
  (location (:child :location :body))
  (photosurl (:child :photosurl :body))
  (profileurl (:child :profileurl :body))
  (mobileurl (:child :mobileurl :body))
  (firstdate (:child :photos :child :firstdate :body))
  (firstdatetaken (:child :photos :child :firstdatetaken :body))
  (count (:child :photos :child :count :body) :integer))

(defapistruct flickr-photoset
  (id :id)
  (primary :primary)
  (photos :photos)
  (secret :secret)
  (server :server)
  (title (:child :title :body))
  (description (:child :description :body)))

(defapistruct flickr-photoset-info
  (id :id)
  (owner :owner)
  (primary :primary)
  (photos :photos)
  (title (:child :title :body))
  (description (:child :description :body)))

(defapistruct flickr-photoset-photo
  (id :id)
  (secret :secret)
  (server :server)
  (farm :farm)
  (title :title)
  (isprimary :isprimary :boolean))

(defapistruct flickr-public-contact
  (id :nsid)
  (username :username)
  (ignored :ignored :boolean))

(defapistruct flickr-search-photo
  (id :id)
  (owner :owner)
  (secret :secret)
  (server :server)
  (farm :farm)
  (title :title)
  (ispublic :ispublic :boolean)
  (isfriend :isfriend :boolean)
  (isfamily :isfamily :boolean))

(defapistruct flickr-size
  (label :label)
  (width :width :integer)
  (height :height :integer)
  (source :source)
  (url :url))

(defapistruct flickr-tag
  (id :id)
  (author :author)
  (raw :raw)
  (machine-tag :machine_tag :boolean)
  (text (:body)))

(defapistruct flickr-url
  (type :type)
  (url (:body)))

(defapistruct flickr-user
  (id :nsid)
  (username (:child :username :body)))

(defapistruct flickr-url-user
  (id :id)
  (username (:child :username :body)))

;; Uses flickr-not, flickr-tag and flickr-url, so must come after
;; them.
(defapistruct flickr-full-photo
  (id :id)
  (secret :secret)
  (server :server)
  (farm :farm)
  (isfavorite :isfavorite :boolean)
  (license :license)
  (rotation :rotation)
  (owner (:child :owner :attrib :nsid))
  (title (:child :title :body))
  (description (:child :description :body))
  (ispublic (:child :visibility :attrib :ispublic) :boolean)
  (isfriend (:child :visibility :attrib :isfriend) :boolean)
  (isfamily (:child :visibility :attrib :isfamily) :boolean)
  (posted (:child :dates :attrib :posted))
  (taken (:child :dates :attrib :taken))
  (takengranularity (:child :dates :attrib :takengranularity))
  (lastupdate (:child :dates :attrib :lastupdate))
  (permcomment (:child :permissions :attrib :permcomment) :boolean)
  (permaddmeta (:child :permissions :attrib :permaddmeta) :boolean)
  (cancomment (:child :editability :attrib :cancomment) :boolean)
  (canaddmeta (:child :editability :attrib :canaddmeta) :boolean)
  (comments (:child :comments :attrib :body) :integer)
  (notes (fn [xml]
	     (map make-flickr-note (xml-children (xml-child :notes xml)))))
  (tags (fn [xml]
	    (map make-flickr-tag (xml-children (xml-child :tags xml)))))
  (urls (fn [xml]
	    (map make-flickr-url (xml-children (xml-child :urls xml))))))

(defn- parse-xml-from-string [string]
  (let [stream (ByteArrayInputStream. (. string getBytes "UTF-8"))]
    (parse stream)))

(defn- call-args-string [args]
  (let [keys (sort (keys args))]
    (reduce str (map (fn [k] (str k (get args k))) keys))))

(defn- arguments-signature-source [api-info args]
  (str (:shared-secret api-info) (call-args-string args)))

(defn- arguments-signature [api-info args]
  (md5-sum (arguments-signature-source api-info args)))

(defn- full-call-args [api-info method args]
  (let [full-args (reduce into (list {"api_key" (:api-key api-info)}
				     (if-let [auth-token (:token api-info)]
				       {"auth_token" auth-token}
				       {})
				     args))]
    (assoc full-args "api_sig" (arguments-signature api-info full-args))))

(defn- lookup-call [api-info method args]
  (if-let [persistence (:persistence api-info)]
    (persistence-get persistence (str method (call-args-string args)))
    nil))

(defn- memoize-call [api-info method args result]
  (when-let [persistence (:persistence api-info)]
    (persistence-put persistence (str method (call-args-string args)) result)))

(defvar *print-calls* false)

(defn- make-flickr-call [api-info method persistent string-modifier args]
  (if-let [result (and persistent (lookup-call api-info method args))]
    (parse-xml-from-string (string-modifier result))
    (do
      (when *print-calls*
	(printf "making call to %s with args %s\n" method (str args))
	(flush))
      (let [full-args (full-call-args api-info method args)
	    result (. xml-rpc-client execute method [full-args])]
	(when persistent
	  (memoize-call api-info method args result))
	(parse-xml-from-string (string-modifier result))))))

(defn- lispify-method-name [string]
  (apply str (mapcat (fn [c]
		       (cond (= c \.) "-"
			     (. Character isUpperCase c) (list \- (. Character toLowerCase c))
			     true (list c)))
		     string)))

(defmacro- defcall [name-string args persistence & body]
  (let [full-method-name-string (str "flickr." name-string)
	fun-name (symbol (lispify-method-name name-string))
	persistent (= persistence :persistent)]
    `(defn ~fun-name [~'api-info ~@args]
       (let [~'call
	     (fn [& args#]
	       (make-flickr-call ~'api-info ~full-method-name-string ~persistent identity (apply sorted-map args#)))
	     ~'call-with-string-modifier
	     (fn [modifier# & args#]
	       (make-flickr-call ~'api-info ~full-method-name-string ~persistent modifier# (apply sorted-map args#)))]
	 ~@body))))

;; returns a list of the items, the total number of pages, and the
;; total number of items.
(defn- multi-page-call [call-fun make-fun per-page page & args]
  (let [result (apply call-fun "per_page" (str per-page) "page" (str page) args)]
    {:items (map make-fun (xml-children result))
     :pages (convert-type (xml-attrib :pages result) :integer)
     :total (convert-type (xml-attrib :total result) :integer)}))

(defcall "auth.getFrob" [] :not-persistent
  (xml-body (call)))

;; returns the token, the permission string, and the user
(defcall "auth.getToken" [] :not-persistent
  (let [result (call "frob" (:frob api-info))]
    {:token (xml-body (xml-child :token result))
     :perms (xml-body (xml-child :perms result))
     :user (let [child (xml-child :user result)]
	     {:nsid (xml-attrib :nsid child)
	      :username (xml-attrib :username child)})}))

(defcall "contacts.getList" [filter per-page page] :persistent
  (if (nil? filter)
    (multi-page-call call make-flickr-contact per-page page)
    (multi-page-call call make-flickr-contact per-page page "filter" filter)))

(defcall "contacts.getPublicList" [nsid per-page page] :persistent
  (multi-page-call call make-flickr-public-contact per-page page "user_id" nsid))

(defcall "favorites.getList" [nsid per-page page] :persistent
  (multi-page-call call make-flickr-favorite per-page page "user_id" nsid))

(defcall "favorites.getPublicList" [nsid per-page page] :persistent
  (multi-page-call call make-flickr-favorite per-page page "user_id" nsid))

(defcall "groups.getInfo" [group-id] :persistent
  (make-flickr-group (call "group_id" group-id)))

(defcall "groups.pools.add" [photo-id group-id] :not-persistent
  (call "photo_id" photo-id "group_id" group-id))

(defcall "groups.pools.getPhotos" [group-id per-page page tags] :persistent
  (if (nil? tags)
    (multi-page-call call make-flickr-search-photo per-page page "group_id" group-id)
    (multi-page-call call make-flickr-search-photo per-page page "group_id" group-id "tags" tags)))

(defcall "groups.pools.remove" [photo-id group-id] :not-persistent
  (call "group_id" group-id "photo_id" photo-id))

(defcall "people.findByUsername" [name] :persistent
  (make-flickr-user (call "username" name)))

(defcall "people.getInfo" [nsid] :persistent
  (make-flickr-person (call "user_id" nsid)))

(defcall "people.getPublicGroups" [nsid] :persistent
  (let [result (call "user_id" nsid)]
    (map make-flickr-list-group (xml-children result))))

(defcall "photos.addTags" [photo-id tags] :not-persistent
  (let [tags-string (apply str (interpose " " (map #(str \" % \") tags)))]
    (call "photo_id" photo-id "tags" tags-string)))

(defcall "photos.comments.getList" [photo-id] :persistent
  (let [result (call "photo_id" photo-id)]
    (map make-flickr-comment (xml-children result))))

(defcall "photos.getAllContexts" [photo-id] :persistent
  (let [result (call-with-string-modifier #(str "<list>" % "</list>") "photo_id" photo-id)]
    (map (fn [item]
	   (case (xml-tag item)
	     :set (assoc (make-flickr-context-set item) :type :set)
	     :pool (assoc (make-flickr-context-pool item) :type :pool)
	     (throw (Exception. (str "invalid context tag " (xml-tag item))))))
	 (xml-children result))))

(defcall "photos.getInfo" [photo-id & secret] :persistent
  (make-flickr-full-photo
   (if (empty? secret)
     (call "photo_id" photo-id)
     (call "photo_id" photo-id "secret" (first secret)))))

(defcall "photos.getSizes" [photo-id] :persistent
  (map make-flickr-size (xml-children (call "photo_id" photo-id))))

(defcall "photos.removeTag" [tag-id] :not-persistent
  (call "tag_id" tag-id))

(defcall "photos.search" [per-page page & keyvals] :persistent
  (let [keyvals-map (apply hash-map keyvals)
	optional-args (mapcat #(if-let [value ((key %) keyvals-map)]
				 (list (val %) value)
				 ())
			      {:user-id "user_id" :tags "tags" :tag-mode "tag_mode" :text "text"
			       :min-upload-data "min_upload_date" :max-upload-date "max_upload_date"
			       :min-taken-date "min_taken_date" :max-taken-date "max_taken_date"
			       :licence "licence" :sort "sort"})]
    (apply multi-page-call call make-flickr-search-photo per-page page optional-args)))

(defcall "photosets.getInfo" [photoset-id] :persistent
  (make-flickr-photoset-info (call "photoset_id" photoset-id)))

(defcall "photosets.getList" [user-id] :persistent
  (let [result (call "user_id" user-id)]
    (map make-flickr-photoset (xml-children result))))

(defcall "photosets.getPhotos" [photoset-id] :persistent
  (let [result (call "photoset_id" photoset-id)]
    (map make-flickr-photoset-photo (xml-children result))))

(defcall "test.echo" [] :not-persistent
  (call-with-string-modifier #(str "<list>" % "</list>")))

(defcall "test.login" [] :not-persistent
  (call))

(defcall "urls.lookupGroup" [url] :persistent
  (make-flickr-url-group (call "url" url)))

(defcall "urls.lookupUser" [url] :persistent
  (make-flickr-url-user (call "url" url)))

(defn api-request-authorization [api-key shared-secret]
  (let [api-info {:api-key api-key :shared-secret shared-secret}
	frob (auth-get-frob api-info)
	api-info (assoc api-info :frob frob)
	perms "write"
	api-sig (arguments-signature api-info {"api_key" api-key "perms" perms "frob" frob})
	url (format "http://flickr.com/services/auth/?api_key=%s&perms=%s&frob=%s&api_sig=%s" api-key perms frob api-sig)]
    {:api-info api-info :url url}))

(defn api-complete-authorization [api-info persistence]
  (merge api-info (auth-get-token api-info) {:persistence persistence}))

(defn collect-pages [fetcher per-page first]
  (lazy-seq
    (let [previous-total (* per-page (dec first))
	  {items :items pages :pages total :total} (fetcher per-page first)]
      (if (<= total (+ previous-total (count items)))
	items
	(concat items (collect-pages fetcher per-page (inc first)))))))

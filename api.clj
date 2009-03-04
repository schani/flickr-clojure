(import '(org.apache.xmlrpc.client XmlRpcClient XmlRpcClientConfigImpl))
(import '(java.security MessageDigest))
(import '(java.io ByteArrayInputStream))
(use 'clojure.xml)
(use 'clojure.contrib.seq-utils)
(use 'clojure.contrib.fcase)

(def xml-rpc-client
     (let [config (XmlRpcClientConfigImpl.)
	   client (XmlRpcClient.)
	   url (java.net.URL. "http://www.flickr.com/services/xmlrpc/")]
       (. config setServerURL url)
       (. client setConfig config)
       client))

(defn md5-sum [string]
  (let [digest (. MessageDigest getInstance "MD5")]
    (. digest update (. string getBytes))
    (let [byte-arr (. digest digest)
	  bigint (BigInteger. 1 byte-arr)
	  bigint-str (. bigint toString 16)
	  leading-zeros (apply str (replicate (- 32 (count bigint-str)) \0))]
      (str leading-zeros bigint-str))))

(defn xml-tag [xml]
  (:tag xml))

(defn xml-body [xml]
  (apply str (:content xml)))

(defn xml-children [xml]
  (:content xml))

(defn xml-child [tag xml]
  (first (filter #(= (:tag %) tag) (xml-children xml))))

(defn xml-attrib [attrib xml]
  (attrib (:attrs xml)))

(defn xml-follow-path [xml path]
  (cond (keyword? path) (xml-attrib path xml)
	(empty? path) xml
	true
	 (let [f (first path)]
	   (cond (= f :body) (xml-body xml)
		 (= f :attrib) (xml-attrib (second path) xml)
		 (= f :child) (xml-follow-path (xml-child (second path) xml) (rest (rest path)))
		 true (throw (Exception. (str "Illegal XML Path " path)))))))

(defn convert-type [value type]
  (cond (string? value) (case type
			  :string value
			  :integer (BigInteger. value)
			  :boolean (not (zero? (BigInteger. value)))
			  (throw (Exception. (str "Illegal type " type))))
	(nil? value) :unknown
	true (throw (Exception. (str "Value to be converted (" value ") must be string or nil")))))

(defmacro defapistruct [name & members]
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
    `(defn ~parser-name [~xml-arg]
       (sorted-map ~@member-inits))))

(defapistruct flickr-comment
  (id :id)
  (author :author)
  (authorname :authorname)
  (date-create :date_create)
  (permalink :permalink)
  (text (:body)))

(defapistruct flickr-contact
  (nsid :nsid)
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
  (title :title)
  (ispublic :ispublic :boolean)
  (isfriend :isfriend :boolean)
  (isfamily :isfamily :boolean))

(defapistruct flickr-group
  (id :id)
  (name (:child :name :body))
  (description (:child :description :body))
  (members (:child :members :body) :integer)
  (privacy (:child :privacy :body)))

(defapistruct flickr-list-group
  (id :nsid)
  (name :name)
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
  (nsid :nsid)
  (username (:child :username :body)))

;; Uses flickr-not, flickr-tag and flickr-url, so must come after
;; them.
(defapistruct flickr-full-photo
  (id :id)
  (secret :secret)
  (server :server)
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

(defn parse-xml-from-string [string]
  (let [stream (ByteArrayInputStream. (. string getBytes))]
    (parse stream)))

(defn arguments-signature-source [api-info args method]
  (let [keys (sort (keys args))
	args-string (reduce str (map (fn [k] (str k (get args k))) keys))]
    (str (:shared-secret api-info) args-string)))

(defn arguments-signature [api-info args method]
  (md5-sum (arguments-signature-source api-info args method)))

(defn full-call-args [api-info method args]
  (let [full-args (reduce into (list {"api_key" (:api-key api-info)}
				     (if-let [auth-token (:token api-info)]
				       {"auth_token" auth-token}
				       {})
				     args))]
    (assoc full-args "api_sig" (arguments-signature api-info full-args method))))

(defn make-flickr-call [api-info method string-modifier args]
  (let [full-args (full-call-args api-info method args)
	result (. xml-rpc-client execute method [full-args])]
    (parse-xml-from-string (string-modifier result))))

(defn lispify-method-name [string]
  (apply str (mapcat (fn [c]
		       (cond (= c \.) "-"
			     (. Character isUpperCase c) (list \- (. Character toLowerCase c))
			     true (list c)))
		     string)))

(defmacro defcall [name-string args & body]
  (let [full-method-name-string (str "flickr." name-string)
	fun-name (symbol (lispify-method-name name-string))
	api-info 'api-info
	call 'call
	call-with-string-modifier 'call-with-string-modifier]
    `(defn ~fun-name [~api-info ~@args]
       (let [~call
	     (fn [& args#]
	       (make-flickr-call ~api-info ~full-method-name-string identity (apply sorted-map args#)))
	     ~call-with-string-modifier
	     (fn [modifier# & args#]
	       (make-flickr-call ~api-info ~full-method-name-string modifier# (apply sorted-map args#)))]
	 ~@body))))

;; returns a list of the items, the total number of pages, and the
;; total number of items.
(defn multi-page-call [call-fun make-fun per-page page & args]
  (let [result (apply call-fun "per_page" (str per-page) "page" (str page) args)]
    {:items (map make-fun (xml-children result))
     :pages (convert-type (xml-attrib :pages result) :integer)
     :total (convert-type (xml-attrib :total result) :integer)}))

(defcall "auth.getFrob" []
  (xml-body (call)))

;; returns the token, the permission string, and the user
(defcall "auth.getToken" []
  (let [result (call "frob" (:frob api-info))]
    {:token (xml-body (xml-child :token result))
     :perms (xml-body (xml-child :perms result))
     :user (let [child (xml-child :user result)]
	     {:nsid (xml-attrib :nsid child)
	      :username (xml-attrib :username child)})}))

(defcall "contacts.getList" [filter per-page page]
  (if (nil? filter)
    (multi-page-call call make-flickr-contact per-page page)
    (multi-page-call call make-flickr-contact per-page page "filter" filter)))

(defcall "contacts.getPublicList" [nsid per-page page]
  (multi-page-call call make-flickr-public-contact per-page page "user_id" nsid))

(defcall "favorites.getList" [nsid per-page page]
  (multi-page-call call make-flickr-favorite per-page page "user_id" nsid))

(defcall "favorites.getPublicList" [nsid per-page page]
  (multi-page-call call make-flickr-favorite per-page page "user_id" nsid))

(defcall "groups.getInfo" [group-id]
  (make-flickr-group (call "group_id" group-id)))

(defcall "groups.pools.add" [photo-id group-id]
  (call "photo_id" photo-id "group_id" group-id))

(defcall "groups.pools.getPhotos" [group-id per-page page tags]
  (if (nil? tags)
    (multi-page-call call make-flickr-search-photo per-page page "group_id" group-id)
    (multi-page-call call make-flickr-search-photo per-page page "group_id" group-id "tags" tags)))

(defcall "groups.pools.remove" [photo-id group-id]
  (call "group_id" group-id "photo_id" photo-id))

(defcall "people.findByUsername" [name]
  (make-flickr-user (call "username" name)))

(defcall "people.getInfo" [nsid]
  (make-flickr-person (call "user_id" nsid)))

(defcall "people.getPublicGroups" [nsid]
  (let [result (call "user_id" nsid)]
    (map make-flickr-list-group (xml-children result))))

(defcall "photos.addTags" [photo-id tags]
  (let [tags-string (apply str (interpose " " (map #(str \" % \") tags)))]
    (call "photo_id" photo-id "tags" tags-string)))

(defcall "photos.comments.getList" [photo-id]
  (let [result (call "photo_id" photo-id)]
    (map make-flickr-comment (xml-children result))))

(defcall "photos.getAllContexts" [photo-id]
  (let [result (call-with-string-modifier #(str "<list>" % "</list>") "photo_id" photo-id)]
    (map (fn [item]
	   (case (xml-tag item)
	     :set (assoc (make-flickr-context-set item) :type 'set)
	     :pool (assoc (make-flickr-context-pool item) :type 'pool)
	     (throw (Exception. (str "invalid context tag " (xml-tag item))))))
	 (xml-children result))))

(defcall "photos.getInfo" [photo-id secret]
  (make-flickr-full-photo
   (if (nil? secret)
     (call "photo_id" photo-id)
     (call "photo_id" photo-id "secret" secret))))

(defcall "photos.getSizes" [photo-id]
  (map make-flickr-size (xml-children (call "photo_id" photo-id))))

(defcall "photos.removeTag" [tag-id]
  (call "tag_id" tag-id))

(defcall "photos.search" [per-page page & keyvals]
  (let [keyvals-map (apply hash-map keyvals)
	optional-args (mapcat #(if-let [value ((key %) keyvals-map)]
				 (list (val %) value)
				 ())
			      {:user-id "user_id" :tags "tags" :tag-mode "tag-mode" :text "text"
			       :min-upload-data "min_upload_date" :max-upload-date "max_upload_date"
			       :min-taken-date "min_taken_date" :max-taken-date "max_taken_date"
			       :licence "licence" :sort "sort"})]
    (apply multi-page-call call make-flickr-search-photo per-page page optional-args)))

(defcall "photosets.getInfo" [photoset-id]
  (make-flickr-photoset-info (call "photoset_id" photoset-id)))

(defcall "photosets.getList" [user-id]
  (let [result (call "user_id" user-id)]
    (map make-flickr-photoset (xml-children result))))

(defcall "photosets.getPhotos" [photoset-id]
  (let [result (call "photoset_id" photoset-id)]
    (map make-flickr-photoset-photo (xml-children result))))

(defcall "test.echo" []
  (call-with-string-modifier #(str "<list>" % "</list>")))

(defcall "test.login" []
  (call))

(defn request-authorization [api-key shared-secret]
  (let [api-info {:api-key api-key :shared-secret shared-secret}
	frob (auth-get-frob api-info)
	api-info (assoc api-info :frob frob)
	perms "write"
	api-sig (arguments-signature api-info {"api_key" api-key "perms" perms "frob" frob} nil)
	url (format "http://flickr.com/services/auth/?api_key=%s&perms=%s&frob=%s&api_sig=%s" api-key perms frob api-sig)]
    {:api-info api-info :url url}))

(defn complete-authorization [api-info]
  (into api-info (auth-get-token api-info)))

(defn collect-pages [fetcher per-page first]
  (lazy-seq
    (let [previous-total (* per-page (dec first))
	  {items :items pages :pages total :total} (fetcher per-page first)]
      (if (<= total (+ previous-total (count items)))
	items
	(concat items (collect-pages fetcher per-page (inc first)))))))

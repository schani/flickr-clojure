(import '(org.apache.xmlrpc.client XmlRpcClient XmlRpcClientConfigImpl))
(import '(java.security MessageDigest))
(import '(java.io ByteArrayInputStream))
(refer 'clojure.xml)
(refer 'clojure.contrib.seq-utils)
(refer 'clojure.contrib.fcase)

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
	  bigint (BigInteger. 1 byte-arr)]
      (. bigint toString 16))))

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
  (if (string? value)
    (case type
      :string value
      :integer (BigInteger. value)
      :boolean (not (zero? (BigInteger. value))))
    :unknown))

(defmacro defapistruct [name & members]
  (let [parser-name (symbol (str "make-" name))
	xml-arg (gensym)
	member-inits (mapcat (fn [member]
			       (let [[name path & type-list] member
				     type (or (first type-list) :string)
				     member-keyword (keyword (str name))]
				 (if (and (list? path) (includes? (first path) '(fn fn*)))
				   `(~member-keyword (~path ~xml-arg))
				   `(~member-keyword (convert-type (xml-follow-path ~xml-arg '~path) ~type)))))
			     members)]
    `(defn ~parser-name [~xml-arg]
       (sorted-map ~@member-inits))))

(defapistruct flickr-user
  (nsid :nsid)
  (username (:child :username :body)))

(defapistruct flickr-contact
  (nsid :nsid)
  (username :username)
  (realname :realname)
  (isfriend :friend :boolean)
  (isfamily :family :boolean)
  (ignored :ignored :boolean))

(defapistruct flickr-public-contact
  (id :nsid)
  (username :username)
  (ignored :ignored :boolean))

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
				     (if-let [auth-token (:auth-token api-info)]
				       {"auth_token" auth-token}
				       {})
				     args))]
    (assoc full-args "api_sig" (arguments-signature api-info full-args method))))

(defn make-flickr-call [api-info method modifier args]
  (let [full-args (full-call-args api-info method args)
	result (. xml-rpc-client execute method [full-args])]
    (modifier (parse-xml-from-string result))))

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
	call-with-modifier 'call-with-modifier]
    `(defn ~fun-name [~api-info ~@args]
       (let [~call (fn [& args#]
		     (make-flickr-call ~api-info ~full-method-name-string identity (apply sorted-map args#)))
	     ~call-with-modifier (fn [modifier# & args#]
				   (make-flickr-call ~api-info ~full-method-name-string modifier# (apply sorted-map args#)))]
	 ~@body))))

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

;; FIXME: doesn't work (says doesn't have permission).  also make it
;; multi-page.
(defcall "contacts.getList" [filter]
  (let [result (if (nil? filter)
		 (call)
		 (call "filter" filter))]
    (map make-flickr-contact (xml-children result))))

;; FIXME: make multi-page
(defcall "contacts.getPublicList" [nsid]
  (let [result (call "user_id" nsid)]
    (map make-flickr-public-contact (xml-children result))))

(defcall "people.findByUsername" [name]
  (make-flickr-user (call "username" name)))

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

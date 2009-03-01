(import '(org.apache.xmlrpc.client XmlRpcClient XmlRpcClientConfigImpl))
(import '(java.security MessageDigest))
(import '(java.io ByteArrayInputStream))
(refer 'clojure.xml)

(def xml-rpc-client
     (let [config (new XmlRpcClientConfigImpl)
	   client (new XmlRpcClient)
	   url (new java.net.URL "http://www.flickr.com/services/xmlrpc/")]
       (. config setServerURL url)
       (. client setConfig config)
       client))

(defn md5-sum [string]
  (let [digest (. MessageDigest getInstance "MD5")]
    (. digest update (. string getBytes))
    (let [byte-arr (. digest digest)
	  bigint (new BigInteger 1 byte-arr)]
      (. bigint toString 16))))

(defn xml-body [xml]
  (apply str (:content xml)))

(defn parse-xml-from-string [string]
  (let [stream (new ByteArrayInputStream (. string getBytes))]
    (parse stream)))

(defn arguments-signature [api-info args]
  (let [keys (sort (keys args))
	args-string (reduce str (map (fn [k] (str k (get args k))) keys))]
    (md5-sum (str (:shared-secret api-info) args-string))))

(defn full-call-args [api-info method args]
  (let [full-args (reduce into (list {"api_key" (:api-key api-info)}
				     (if-let [auth-token (:auth-token api-info)]
				       {"auth_token" auth-token}
				       {})
				     args))]
    (assoc full-args "api_sig" (arguments-signature api-info full-args))))

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
	call 'call
	call-with-modifier 'call-with-modifier]
    `(defn ~fun-name [api-info# ~@args]
       (let [~call (fn [& args#]
		     (make-flickr-call api-info# ~full-method-name-string identity (apply sorted-map args#)))
	     ~call-with-modifier (fn [modifier# & args#]
				   (make-flickr-call api-info# ~full-method-name-string modifier# (apply sorted-map args#)))]
	   ~@body))))

(defcall "auth.getFrob" ()
  (xml-body (call)))

(defn request-authorization [api-key shared-secret]
  (let [api-info {:api-key api-key :shared-secret shared-secret}
	frob (auth-get-frob api-info)
	api-info (assoc api-info :frob frob)
	perms "write"
	api-sig (arguments-signature api-info {"api_key" api-key "perms" perms "frob" frob})]
    api-sig))

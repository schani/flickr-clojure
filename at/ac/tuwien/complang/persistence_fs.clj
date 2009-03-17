;;; persistence_fs.clj

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

(ns at.ac.tuwien.complang.persistence-fs
  (:use at.ac.tuwien.complang.flickr-api
	at.ac.tuwien.complang.utils)
  (:import java.io.File java.io.FileWriter))

(defn- write-to-file [filename content]
  (let [file (new File filename)
	dir (. file getParentFile)
	tmp-file (. File createTempFile "unfinished" nil dir)
	writer (new FileWriter tmp-file)]
    (. writer write content)
    (. writer close)
    (. tmp-file renameTo file)))

(defn- filename-for-key [p key]
  (let [{dir :dir} p]
    (str (. dir getCanonicalPath) File/separator (md5-sum key))))

(defmethod persistence-get ::Directory [p key]
  (try
   (slurp (filename-for-key p key))
   (catch java.io.FileNotFoundException e
     nil)))

(defmethod persistence-put ::Directory [p key value]
  (write-to-file (filename-for-key p key) value))

(defn open-persistence-fs [dir]
  (with-meta {:dir (new File dir)} {:type ::Directory}))

;;; persistence-bdb.clj

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

(ns at.ac.tuwien.complang.persistence
  (:use at.ac.tuwien.complang.flickr-api))

(defn- open-database [dir name]
  (let [env-config (new com.sleepycat.je.EnvironmentConfig)]
    (. env-config setAllowCreate true)
    (let [db-env (new com.sleepycat.je.Environment (new java.io.File dir) env-config)
	  db-config (new com.sleepycat.je.DatabaseConfig)]
      (. db-config setAllowCreate true)
      (. db-env openDatabase nil name db-config))))

(defn- close-database [db]
  (let [db-env (. db getEnvironment)]
    (. db close)
    (. db-env close)))

(defn- make-database-entry [str]
  (new com.sleepycat.je.DatabaseEntry (. str getBytes "UTF-8")))

(defn- database-sync [db]
  (. (. db getEnvironment) sync))

(defn- database-put [db key value]
  (. db put nil (make-database-entry key) (make-database-entry value))
  (database-sync db))

(defn- database-get [db key]
  (let [entry (new com.sleepycat.je.DatabaseEntry)
	status (. db get nil (make-database-entry key) entry com.sleepycat.je.LockMode/DEFAULT)]
    (if (= status com.sleepycat.je.OperationStatus/SUCCESS)
      (new String (. entry getData) "UTF-8")
      nil)))

(defn- database-delete [db key]
  (. db delete nil (make-database-entry key))
  (database-sync db))

(defmethod persistence-get ::Database [p key]
  (let [{db :db} p]
    (database-get db key)))

(defmethod persistence-put ::Database [p key value]
  (let [{db :db} p]
    (database-put db key value)))

(defn open-persistence-bdb [dir name]
  (with-meta {:db (open-database dir name)} {:type ::Database}))

(defn close-persistence-bdb [p]
  (let [{db :db} p]
    (close-database db)))

;;; utils.clj

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

(ns at.ac.tuwien.complang.utils
  (:import [java.security MessageDigest]))

(defn md5-sum [string]
  (let [digest (. MessageDigest getInstance "MD5")]
    (. digest update (. string getBytes))
    (let [byte-arr (. digest digest)
	  bigint (BigInteger. 1 byte-arr)
	  bigint-str (. bigint toString 16)
	  leading-zeros (apply str (replicate (- 32 (count bigint-str)) \0))]
      (str leading-zeros bigint-str))))

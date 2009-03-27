;;; examples.clj

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

(use 'at.ac.tuwien.complang.flickr)
(use 'clojure.contrib.seq-utils)

(defn photo-frequencies-favorited-by-users [users]
  "Returns the frequencies of photos favorited by users."
  (frequencies (reduce concat (pmap favorites users))))

(defn photos-favorited-by-n-users [users n]
  "Returns a sequence of the photos favorited by at least n users."
  (let [freqs (photo-frequencies-favorited-by-users users)]
    (keys (filter #(>= (val %) n) freqs))))

(defn photos-favorited-by-n-contacts [user n]
  "Returns a sequence of the photos favorited by at least n of user's
   contacts."
  (photos-favorited-by-n-users (contacts user) n))

(defn most-favorited-users [user threshold]
  "Returns a sequence of users each of which user has favorited at
   least threshold photos from."
  (let [freqs (frequencies (map owner (favorites user)))]
    (keys (filter #(>= (val %) threshold) freqs))))

(defn most-favorited-non-contacts [user threshold]
  "Returns a sequence of users that are not user's contacts and each
   of which user has favorited at least threshold photos from."
  (difference (set (most-favorited-users user threshold)) (set (contacts user))))

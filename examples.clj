(use 'at.ac.tuwien.complang.flickr)
(use 'clojure.contrib.seq-utils)

(defn photos-favorited-by-n-users [users n]
  "Returns a sequence of the photos favorited by at least n users."
  (let [freqs (frequencies (mapcat favorites users))]
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

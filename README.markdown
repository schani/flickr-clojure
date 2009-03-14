## Flickr API bindings for Clojure ##

### Overview ###

flickr-clojure provides an interface to the [Flickr
API](http://www.flickr.com/services/api/) for the Clojure programming
language.  It is a somewhat faithful port of
[Clickr](http://schani.wordpress.com/2006/07/20/lisping-flickr/), a
bindings library that I wrote for Common Lisp.

flickr-clojure consists of two parts.  The namespace
at.ac.tuwien.complang.flickr-api contains a direct mapping of the
Flickr API, the use of which I discourage.  The high-level interface
is in the namespace at.ac.tuwien.complang.flickr.

Both APIs are thread safe.  The high-level interface fetches lazily
and keeps the fetched data.  Getting a user's name for the first time,
for example, results in an XML-RPC call.  The second time the user's
name is requested no call is made.  Lists are fetched lazily, too,
which is an improvement over the Common Lisp code.

### Usage ###

The first step in using the Flickr API is authentication.  Assuming
that you have a API key and a shared secret, you can request
authorization like this:

    user=> (request-authorization *api-key* *shared-secret*)
    {:api-info ..., :url "http://www.flickr..."}

The function returns an API-info data structure and a URL.  Open the
URL, confirm the authorization, and then do

    user=> (def *my-user* (complete-authorization *api-info*))

with the API-info returned from the call to request-authorization.
The result is a data structure representing the authorized user.

    user=> (realname *my-user*)
    "Mark Probst"
    user=> (title (first (photos *my-user*)))
    "Watermill"
    user=> (count (contacts *my-user*))
    483

The file examples.clj contains a few more complex examples.  Here is
the number of photos favorited by at least three of my first ten
contacts:

    user=> (count (photos-favorited-by-n-users (take 10 (contacts *my-user*)) 3))
    27

### Reference ###

TBD

### Requirements ###

Apart from Clojure itself, flickr-clojure requires the [Apache XML-RPC
library](http://ws.apache.org/xmlrpc/) (version 3.1.1 is known to
work).

### License ###

flickr-clojure is licensed under the [GNU General Public License
3](http://www.gnu.org/copyleft/gpl.html) or later, a copy of which can
be found in the file COPYING.

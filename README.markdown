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

### Persistence ###

The namespaces **at.ac.tuwien.complang.persistence-fs** and
**at.ac.tuwien.complang.persistence-bdb** provide functions for
creating persistence objects.  A persistence object is used to
persistently cache results to Flickr API queries so that these queries
don't have to be done again in a different session or in the same
session if minimal instances are used or caching is turned off.

### Reference ###

TBD

**(request-authorization *api-key* *shared-secret*)**

**(complete-authorization *api-info* &key *:persistence* *:minimal-instances* *:do-cache*)**

Completes authorization with Flickr and returns the logged in user.

*:persistence* is either **nil** or a persistence object.  The default
is **nil**.

*:minimal-instances* is a boolean and specifies whether the objects
representing Flickr objects should be minimal and only contain the ID
of the Flickr object and the API info.  The alternative is for the
objects to contain slots for all the Flickr object properties.  The
advantage of the latter is that these properties are saved with the
object and thus need only be fetched once, whereas with minimal
instances they need to be fetched every time they are queried (which
might not be a big issue if persistence is used).  The advantage of
minimal instances is that less memory is used.  The default is
**false**.

*:do-cache* is a boolean specifying whether to cache instances based
on their IDs.  The consequence is that the same Flickr object fetched
via two different queries will use the same object, thereby both
conserving memory and reducing additional queries.  Turning caching
off usually makes most sense in combination with minimal instances.
The default is **true**.

### Requirements ###

Apart from Clojure itself, flickr-clojure requires the [Apache XML-RPC
library](http://ws.apache.org/xmlrpc/) (version 3.1.1 is known to
work).

### License ###

flickr-clojure is licensed under the [GNU General Public License
3](http://www.gnu.org/copyleft/gpl.html) or later, a copy of which can
be found in the file COPYING.

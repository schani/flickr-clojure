# Flickr API bindings for Clojure #

## Overview ##

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

## Usage ##

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

## Persistence ##

The namespaces **at.ac.tuwien.complang.persistence-fs** and
**at.ac.tuwien.complang.persistence-bdb** provide functions for
creating persistence objects.  A persistence object is used to
persistently cache results for Flickr API queries so that these
queries don't have to be done again in a different session or in the
same session if minimal instances are used or caching is turned off.

## Reference ##

### at.ac.tuwien.complang.flickr ###

**(request-authorization *api-key* *shared-secret*)**

Request authorization from the Flickr API.  You can request an
*api-key* on the [Flickr
website](http://www.flickr.com/services/api/keys/apply/).  The
*shared-secret* comes with it.

**(complete-authorization *api-info* &key *:persistence* *:minimal-instances* *:do-cache*)**

Completes authorization with Flickr and returns the logged in user.
After calling this function the *api-info* is invalid.  To get the
valid API info use the function **api-info** on the returned user
object.

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

**(api-info *object*)**

Returns the API info for the *object*.

**(make-user *api-info* *nsid*)**

Returns an object for the user with the given *nsid*.

**(username *user*)**  
**(realname *user*)**  
**(location *user*)**  
**(ispro *user*)**

Returns a property of *user*.

**(photos *user*)**

Returns a lazy seq of *user*'s photos.

**(photosets *user*)**

Returns a list of *user*'s photosets.

**(groups *user*)**

Returns a list of *user*'s public groups.

**(contacts *user*)**

Returns a lazy seq of *user*'s public contacts.

**(favorites *user*)**

Returns a lazy seq of *user*'s favorites.

**(make-photo *api-info* *nsid*)**

Returns an object for the photo with the given *nsid*.

**(secret *photo*)**  
**(server *photo*)**  
**(isfavorite *photo*)**  
**(license *photo*)**  
**(rotation *photo*)**  
**(title *photo*)**  
**(description *photo*)**  
**(ispublic *photo*)**  
**(isfriend *photo*)**  
**(isfamily *photo*)**  
**(posted *photo*)**  
**(taken *photo*)**  
**(takengranularity *photo*)**  
**(lastupdate *photo*)**  
**(permcomment *photo*)**  
**(permaddmeta *photo*)**  
**(cancomment *photo*)**  
**(canaddmeta *photo*)**

Returns a property of *photo*.

**(owner *photo*)**

Returns the *photo*'s owner.

**(urls *photo*)**

Returns a list of maps representing the *photo*'s URLs.  Each map
provides values for the keys *:type* and *:url*.

**(notes *photo*)**

Returns a list of the *photo*'s notes.

**(tags *photo*)**

Returns a list of the *photo*'s tags.

**(sizes *photo*)**

Returns a list of maps representing the *photo*'s sizes.  Each map
provides values for the keys *:label*, *:width*, *:height*, *:source*
and *:url*.

**(comments *photo*)**

Returns a list of the *photo*'s comments.

**(sets *photo*)**

Returns a list of the photosets the *photo* is a member of.

**(groups *photo*)**

Returns a list of the groups the *photo* is a member of.

**(make-photoset *api-info* *nsid*)**

Returns an object for the photoset with the given *nsid*.

**(primary *set*)**  
**(secret *set*)**  
**(server *set*)**  
**(title *set*)**  
**(description *set*)**

Returns a property of *set*.

**(photos *set*)**

Returns a lazy seq of *set*'s photos.

**(owner *set*)**

Returns the *set*'s owner.

**(make-group *api-info* *nsid*)**

Returns an object for the group with the given *nsid*.

**(title *group*)**  
**(description *group*)**  
**(privacy *group*)**

Returns a property of *group*.

**(photos *group*)**

Returns a lazy seq of *group*'s photos.

**(date-create *comment*)**  
**(permalink *comment*)**  
**(text *comment*)**

Returns a property of *comment*.

**(author *comment*)**

Returns the *comment*'s author.

**(x *note*)**  
**(y *note*)**  
**(w *note*)**  
**(h *note*)**  
**(text *note*)**

Returns a property of *note*.

**(author *note*)**

Returns the *note*'s author.

**(raw *tag*)**  
**(text *tag*)**

Returns a property of *tag*.

**(author *tag*)**

Return the *tag*'s author.

### at.ac.tuwien.complang.persistence-fs ###

**(open-persistence-fs *dir*)**

Creates a persistence object which stores Flickr API query results as
text files in the directory *dir*, which must already exist and be
writable.

### at.ac.tuwien.complang.persistence-bdb ###

To use this namespace you need to have the [Berkeley DB Java
Edition](http://www.oracle.com/technology/products/berkeley-db/je/index.html)
installed.

**(open-persistence-bdb *dir* *name*)**

Creates a persistence object which stores Flickr API query results in
a Berkeley DB database with the name *name* in the directory *dir*,
which must already exist and be writable.

**(close-persistence-bdb *p*)**

Closes the database associated with the BDB persistence object *p*.

### at.ac.tuwien.complang.flickr-api ###

TBD

## Requirements ##

Apart from Clojure itself, flickr-clojure requires the [Apache XML-RPC
library](http://ws.apache.org/xmlrpc/) (version 3.1.1 is known to
work).

## License ##

flickr-clojure is licensed under the [GNU General Public License
3](http://www.gnu.org/copyleft/gpl.html) or later, a copy of which can
be found in the file COPYING.

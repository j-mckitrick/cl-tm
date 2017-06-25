;;; Global variables for TM scanner.

(in-package :tm)

;(declaim (optimize debug))

(defparameter *cookie-jar* (make-instance 'cookie-jar)
  "Cookies for Drakma.")

(defparameter *saved-page* nil
  "Contents of last page saved.")

(defparameter *saved-status* nil
  "HTTP status from last request.")

(defparameter *logged-in-p* nil
  "Are we logged in to TM?")

(defparameter *expected-orders* nil
  "Number of orders expected.  Not all orders will have details or tickets.")

(defparameter *current-order* nil
  "Current order object.")

(defparameter *accounts* (list
			  (cons "daniellesusan@aol.com" "polpol1")
			  (cons "christinaspec@aol.com" "polpol1"))
  "Account names and passwords for TM.")

(defparameter *login-url* "/member"
  "URL for TM login.")

(defparameter *logout-url* "/member/logout"
  "URL for TM logout.")

(defparameter *seed-regex*
  "(?i)<input.*?name=['\"]v['\"]\\s+value=['\"]([^'\"]*)['\"]([^>]*?)/>"
  "Matches login seed.")

(defparameter *order-count-regex*
  "(?i)<strong>[-0-9]+</strong>\\s+[(]of\\s+([0-9]+)[)]"
  "Matches number of orders expected.")

(defparameter *next-page-regex* "<a\\s+.*?href=[\"']?([^\"' >]*)[\"']?[^>]*>(.*?)</a>"
  "Matches link to next page of orders.")

(defparameter *link-regex* "<a\\s+.*?href=[\"']?([^\"' >]*)[\"']?[^>]*>(.*?)</a>"
  "Matches links.")

(defparameter *table-regex* "(?is)<table([^>]*)>(.*?)</table>"
  "Matches html tables.")

(defparameter *refresh-tag-regex*
  "(?is)<meta\\s+http-equiv=[\"']refresh[\"']\\s+([^>]*)>"
  "Matches the http refresh META tag.")

(defparameter *refresh-tag-content-regex* "(?is)content=[\"']([^\"']*)[\"']"
  "Matches contents of META tag.")

(defparameter *refresh-url/delay-regex* "(?is)^(\\d);url=(.*)"
  "Matches url and delay from META tag.")

(defparameter *order-detail-validate-regex* "(?s)(?:Order Detail|Your Ticket)"
  "Matches validator for order detail page.")

(defparameter *error-regexes*
  (list
   "(?is)<div class=.container-hotZone.>\\s+<strong>([^<]*?)</strong>"
   "(?is)<(?:div|span)\\s+class=.messagetext.>(.*?)</(?:div|span)>"
   "(?is)(routine\\s*maintenance)")
  "Matches error messages in TM pages.")

(defparameter *status-regexes*
  (list
   "(?is)<div class=.container-hotZone.>\\s+<strong>([^<]*?)</strong>"
   "(?is)<(?:div|span)\\s+class=.messagetext.>(.*?)</(?:div|span)>")
  "Matches status messages in TM pages.")

(defparameter *csv-filename* "/tmp/cl-tm-data.csv"
  "Filename for csv data.")

(defparameter *csv-header*
  (concatenate 'string
	       "id, order date, "
	       "event name, event date, event time, venue, city, state, "
	       "currency, amount, seats from, seats thru, "
	       "section, row, type, price, bld chg, conv chg, desc, "
	       "quantity, total charges, card type, last4digits, billing addr, "
	       "tfbits, status, message text"
	       "~%")
  "Column names for csv data.")

(defparameter *rip-details-p* t
  "Rip order details for csv export.")

(defparameter *rip-tickets-p* t
  "Rip and fetch pdf tickets.")

(defparameter *export-orders-p* t
  "Export orders to csv.")

(defparameter *saved-headers* nil
  "Headers from last request.")

(defparameter *div/button-regex*
  "(?is)<div\\s+class=.button.([^>]*)>(View.*?Print.*?Tickets.*?)</div>"
  "Matches a div/button for downloading pdf ticket.")

(defparameter *onclick-regex*
  "(?is)onclick=[\'\"]window.open\\([\'\"]([^\'\"\\)]*)[\'\"]"
  "Matches event handler for div/button.")

(defparameter *details-table-regex*
  "(?is)<table\\s+class=.detailsTable.(?:[^>]*)>(?:.*?)</table>"
  "Matches a detailsTable.")

(defparameter *rows-regex* "(?is)<tr(?:[^>]*)>(?:.*?)</tr>"
  "Matches the contents of a row.")

(defparameter *cells-regex* "(?is)<t(?:h|d)(?:[^>]*)>(?:.*?)</t(?:h|d)>"
  "Matches an entire table heading or data cell.")

(defparameter *h-cell-regex* "(?i)<th(?:[^>]*)>(.*?)</th>"
  "Matches the contents of a heading cell.")

(defparameter *d-cell-regex* "(?i)<td(?:[^>]*)>(.*?)</td>"
  "Matches the contents of a data cell.")

(defparameter *cell-contents-regex* "(?is)<t(?:h|d)(?:[^>]*)>(.*?)</t(?:h|d)>"
  "Matches the contents of a table heading or data cell.")

(defparameter *non-strong-details-regex* "(?is)<strong>(?:[^<]*)</strong>(.*)"
  "Matches details just after a STRONG tag.")

(defparameter *event-location-regex*
  "(?is)<br>([^,]*),\\s*([^,]*),\\s*([^,]*)<br>(\\d+/\\d+/\\d+)(?:.*?)<br>"
  "Matches venue, city, state, and event date from order details.")

(defparameter *date-src-regex* "(\\d+/\\d+/)(\\d+)"
  "Matches date/time source expression.")

(defparameter *date-dest-regex* "\\{1}20\\{2}"
  "Replaces year with 4-digit version.")

(defparameter *time-regex* "(\\d+:\\d+\\s+(A|P)M)"
  "Matches event time.")

(defparameter *currency-regex* "(\\S+)\\s*([$0-9,]*)"
  "Matches currency type and amount.")

(defparameter *seats-regex* "(?i)\\s*seats?:\\s*(\\d+)\\s*\\w*\\s*(\\d*)\\|?"
  "Matches seat details.")

(defparameter *seat-count-regex* "x\\s+([1-9]+)\\s*<br>"
  "Matches seat count.")

;; PPCRE NOTES:
;; SCAN - VALUES: easiest way to confirm a regex matches a string
;; SCAN-TO-STRINGS - VALUES: matching STRING, VECTOR of registers (good for one match)
;; DO-REGISTER-GROUPS - BINDS only registers while iterating matches
;; REGISTER-GROUPS-BIND - BIND registers for one match
;; DO-MATCHES-AS-STRINGS - ?
;; ALL-MATCHES-AS-STRINGS - LIST of matching strings
;; NB: all ppcre functions that return/bind regs *must* have regs in regex!

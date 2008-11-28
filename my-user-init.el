;; Start *after* display-time
(when (and window-system (not running-as-root) (would-like 'slashdot))
  ;; madpenguin.org does not work if User-Agent is Wget
  (setq slashdot-wget-options (append http-wget-options '("-A" "Mozilla")))
  ;; If you edit this, run (slashdot-load-headings)
  (setq slashdot-url-alist
	'(
	  ("Slashdot" . "http://rss.slashdot.org/Slashdot/slashdot/to")
	  ;; ("The Register" . "http://www.theregister.co.uk/tonys/slashdot.rdf")
	  ;; ("Freshmeat" . "http://freshmeat.net/backend/fm.rdf")
	  ;; ("NewsForge" . "http://newsforge.com/newsforge.rdf")
	  ("OS news" . "http://www.osnews.com/files/recent.xml")
	  ;; ("MadPenguin" . "http://www.madpenguin.org/backend.php")
	  ;; ("Kerneltrap" . "http://kerneltrap.org/node/feed")
	  ;; ("Weather" . "http://www.weatheroffice.gc.ca/rss/city/on-118_e.xml")
	  ))
    (slashdot-start)
  )

(setq zenirc-server-alist
      '(("irc.ipv6.oftc.net" nil nil nil nil)))

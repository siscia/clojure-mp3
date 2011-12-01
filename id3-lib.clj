;; License MIT
(ns mp3)

(def *debug* true)

(defn pack-with-add [n-el coll] coll)

;; This is a fork of the onewland testbed MP3 parsing by siscia
(defn load-mp3
  #^{:doc "load-mp3 loads an mp3 and returns its ID3 information"}
  ([filename] 
     (def input-array (make-array Byte/TYPE 1000)) ;; I don't like it
     ;; too much but i'm not sure that there is another way, so...
     (let [input-stream (new java.io.FileInputStream filename)]
	 (printf "%d bytes read\n" (.read input-stream input-array))
	 (if (= (take 5 input-array) '(0x49 0x44 0x33 0x03 0x00))
	   (do (printf "ID3v2 tag found!\n")
	       (let [rest-of-header (drop 5 input-array)]
		 (when (bit-test (first rest-of-header) 7) (pr "Unsynchronization set.\n"))
		 (when (bit-test (first rest-of-header) 6) (pr "Extended header set.\n"))
		 (when (bit-test (first rest-of-header) 5) (pr "Experimental indicator set.\n"))
		 (let [header-size (+ (bit-shift-left (nth rest-of-header 1) 21)
				      (bit-shift-left (nth rest-of-header 2) 14)
				      (bit-shift-left (nth rest-of-header 3) 7)
				      (nth rest-of-header 4))]
		   (printf "Header length = %d\n" header-size)
		   (let [audio-frames-start (drop (+ 5 header-size) rest-of-header)]
		     (let [sync-word (take 2 audio-frames-start)]
		       
		     ))
		   (do (.close input-stream)		 
		       (read-id3-tags (drop 5 rest-of-header) header-size)))))
	   (pr "Not a valid MP3 with ID3v2 tags")))))

(defn create-unicode-char-list [list-of-two-byte-characters]
  (if (empty? list-of-two-byte-characters)
    '()
    (cons (apply + (map int (take 2 list-of-two-byte-characters)))
	  (create-unicode-char-list (drop 2 list-of-two-byte-characters)))))

(defn string-to-unicode [stringa]
  (map int stringa))

(defn to-digit [b]
  (if b 1 0))

(defn change-base
  #^{:doc "This function inputs a number, n, and a base, base, and convert the number in the base"}
  [n base]
  (Integer/toString n base))

(defn to-binary [n]
  (Integer/toString n 2))

(defn string-to-binary [string]
  (map to-binary (string-to-unicode string)))

(defn read-id3-tags [frame-array header-size]
  (loop [frame-no 0 frame-start frame-array total-bytes-so-far 0 acc (hash-map)]
    (if (< total-bytes-so-far header-size)      
      (let [size-of-frame  (+ (bit-shift-left (nth frame-start 4) 24)
			      (bit-shift-left (nth frame-start 5) 16)
			      (bit-shift-left (nth frame-start 6) 8)
			      (nth frame-start 7))]
	(let [frame-array (take size-of-frame (drop 11 frame-start))
	      frame-title (apply str (map char (take 4 frame-start)))
	      frame-content frame-array]
	  (when *debug*
	    (printf "Frame %d: %s, Size: %d, Content: %s\n" frame-no frame-title
		    size-of-frame (apply str (map char frame-content))))
	  (recur (+ 1 frame-no)
		 (drop (+ 10 size-of-frame) frame-start)
		 (+ total-bytes-so-far 10 size-of-frame)
		 (assoc acc frame-title frame-content))))
      acc)))
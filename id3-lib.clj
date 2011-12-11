;; License MIT
(ns mp3)

(def *debug* true)

(defn pack-with-add [n-el coll] coll)

(defn dimension-synchsafe [size]
  #{:doc "Return the lenght of a frame by an array of bit of the size frame"}
  (assert (= (count size) 4) "Need of 4 elements")
  (+ (bit-shift-left (nth size 0) 21)
     (bit-shift-left (nth size 1) 14)
     (bit-shift-left (nth size 2) 7)
     (nth size 3)))

(defn- read-kind-of-frames [header]
  #^{:doc "take the 10 byte of the header frame and return the kind of the frame"}
  (assert (= (count header) 10) "Need of all the frame")
  (apply str (map char (take 4 header))))

(defn- read-dimension-frame [header]
  #^{:doc "take the 10 byte header and return the dimension of the frames"}
  (assert (= (count header) 10) "Need of all the frames")
  (dimension-synchsafe (take 4 (drop 4 header))))

(defn- read-flag-frames [header]
  #^{:doc "return the flags of the frames"}
  (assert (= (count header) 10) "Need of all the frame")
  (apply str (map char (take 2 (drop 8 header)))))


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

;; This is a fork of the onewland testbed MP3 parsing by siscia
(defn load-mp3
  #^{:doc "load-mp3 loads an mp3 and returns its ID3 information"}
  ([filename]
     (def input-array-header (make-array Byte/TYPE 10)) ;; I don't like it
     ;; too much but i'm not sure that there is another way, so...
     (let [input-stream (new java.io.FileInputStream filename)]
	 (printf "%d bytes read\n" (.read input-stream input-array-header))
	 (if (= (take 5 input-array-header) '(0x49 0x44 0x33 0x03 0x00)) 
	   (do (printf "ID3v2 tag found!\n %d" (nth input-array 3))
	       (let [rest-of-header (drop 5 input-array-header)]
                 (pr "Check bit.\n")
		 (when (bit-test (first rest-of-header) 7) (pr "Unsynchronization set.\n"))
		 (when (bit-test (first rest-of-header) 6) (do (def extend-header true) (pr "Extended header set.\n")))
		 (when (bit-test (first rest-of-header) 5) (pr "Experimental indicator set.\n"))
                 (when (bit-test (first rest-of-header) 4) (pr "Footer present.\n"))
		 (let [header-size 
                       (dimension-synchsafe (take 4 (drop 1 rest-of-header)))
                       id3-no-header (byte-array header-size)] ;;No considerate a Extended header
		   (printf "\nHeader length = %d\n" header-size )
		   (do (.read input-stream id3-no-header) ;;copy the
                       ;;byte from the stream (input-stream) to the
                       ;;array (id3-no-header) TODO better 
                       (.close input-stream)		 
		       (read-id3-tags id3-no-header header-size)))))
	   (pr "Not a valid MP3 with ID3v2 tags")))))

;(defn get-tag [filename]
;  (let [header-array (byte-array 10),
;        input-strea (new java.io.FileInputStream filename)]
;    (do (.read input-stream header-array))
;    (let [header-size (dimension-synchsafe (drop 6 header-array))
;          tag-array (byte-array header-size)]
;      ((.read )))))

(defn read-id3-tags [frame-array header-size]
  (loop [frame-no 0 frame-start frame-array total-bytes-so-far 0 acc (hash-map)]
    (if (< total-bytes-so-far header-size)      
      (let [size-of-frame  (read-dimension-frame (take 10 frame-start))]
	(let [frame-array (take size-of-frame (drop 11 frame-start))
	      frame-title (read-kind-of-frames (take 10 frame-start))
	      frame-content frame-array]
	  (when *debug*
	    (printf "Frame %d: %s, Size: %d, Content: %s\n" frame-no frame-title
		    size-of-frame (apply str (map char frame-content))))
	  (recur (+ 1 frame-no)
		 (drop (+ 10 size-of-frame) frame-start)
		 (+ total-bytes-so-far 10 size-of-frame)
		 (assoc acc frame-title frame-content))))
      acc)))

(ns tag-as-object :extend mp3)



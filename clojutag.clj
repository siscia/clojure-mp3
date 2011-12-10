
(ns cljtag)

(deftype Song [filename tag])

(deftype Tag [version dimension flags frames])

(deftype Frame [name dimension content flag])

(defprotocol SONG
  (get-tag [byte]
    (apply str (map char (take 3 three-byte)))))

(defprotocol TAG
  (get-version [header]
    (apply str (map char (take 2 (drop 3 header)))))
  (get-dimension [header]
    (read-dimension-frame header))
  (get-flags [header]
    (read-flag-frames header))
  )

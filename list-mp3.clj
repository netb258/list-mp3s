;; Description:
;; This script takes a directory filled with MP3 files, then goes through all files
;; and extracts their metadata (artist, song, album year).

(require '[clojure.string :as s])
(import  '[java.io RandomAccessFile])

;; The MP3 directory is specified as a command line argument.
(def mp3-dir
  (-> (first *command-line-args*)
      clojure.java.io/file
      file-seq))

(def mp3-file-paths
  (filter #(s/ends-with? % ".mp3")
          (map #(.toString (.toPath %)) mp3-dir)))

(defn read-id3
  "Takes a file path as a string and returns only the ID3 portion of the file as a byte-array.
  NOTE: The most straight-foreward way to do this is to read the whole file as a byte array
  and do (take-last 128 file-bytes), but this would be very inefficient, as an MP3 file can be 5+ - 10+ mb.
  Therefore we are opting to go with random access files here."
  [file-path]
  (let [raf (RandomAccessFile. file-path "r")
        ;; The ID3 meta data is the last 128 bytes of an MP3 file.
        id3-location (- (.length raf) 128)
        _ (.seek raf id3-location)
        result (byte-array 128)
        _ (.read raf result)]
    (.close raf)
    result))

(defn bytes-to-string
  "Converts a seq of bytes to a string."
  [byte-seq]
  (letfn [(byte-to-char [b]
            (if (and (> b 0) (< b 65535)) ;; NOTE: 0 and 65535 are the min and max values of a Java char.
              (char b)
              \space))] ;; NOTE: Just return a space if the char is not valid.
    (apply str (map byte-to-char byte-seq))))

(defn get-mp3-metadata
  "This function takes a string path to an MP3 file and returns a map like this:
  {:title 'SONG NAME' :artist 'ARTIST NAME' :album 'ALBUM NAME' :year 'YEAR OF RELEASE'}"
  [mp3-file]
  (let [file-bytes (read-id3 mp3-file)
        id3-metadata (take-last 128 file-bytes)
        ;; The title and ID3 tag are the first 33 bytes of the metadata (the tag is the first 3 bytes).
        title (bytes-to-string (drop 3 (take 33 id3-metadata)))
        ;; After the ID3 tag and title, there are 30 bytes that hold the artist.
        artist (bytes-to-string (drop 33 (take 63 id3-metadata)))
        ;; After the artist, there are 30 bytes that hold the album.
        album (bytes-to-string (drop 63 (take 93 id3-metadata)))
        ;; After the album name, there are 4 bytes that hold the year of production.
        year (bytes-to-string (drop 93 (take 97 id3-metadata)))]
    ;; NOTE: We are triming the title/artist/album as their names may contain less than 30 chars and thus are padded.
    {:title (s/trim title) :artist (s/trim artist) :album (s/trim album) :year year}))

(doseq [meta-hash (map get-mp3-metadata mp3-file-paths)]
  (println "Artist: " (:artist meta-hash))
  (println "Title:  " (:title meta-hash))
  (println "Album:  " (:album meta-hash))
  (println "Year:   " (:year meta-hash))
  (println " "))

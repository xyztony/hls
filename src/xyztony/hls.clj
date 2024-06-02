(ns xyztony.hls
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defrecord MediaPlaylist [target-duration version part-inf media-sequence-number
                          media-segments skip preload-hint rendition-reports
                          server-control])

(defrecord PartInf [part-target])

(defrecord ServerControl [can-block-reload part-hold-back can-skip-until])

(defrecord MediaSegment [duration uri partial-segments program-date-time])

(defrecord PartialSegment [part-duration uri independent])

(defrecord Skip [skipped-segments recently-removed-dateranges])

(defrecord PreloadHint [type uri byterange-start byterange-length])

(defrecord RenditionReport [uri last-msn last-part])

;; TODO: There must be a more idiomatic way to do this
(def truth-table
  {"YES" true
   "NO" false})

(defn parse-attributes [s]
  (->> (str/split s #",")
       (map #(str/split % #"="))
       (into {})))

(defmulti read-tag
  (fn [tag _] tag))



;; BEGIN TO-IMPLEMENT
;; TODO this is not 100% correct. 

(defmethod read-tag :EXT-X-MAP [_ _]
  "NOTE: If using fMP4, EXT-X-MAP tags MUST be present."
  {})
(defmethod read-tag :EXT-X-DISCONTINUITY [_ _] {})
(defmethod read-tag :EXT-X-DISCONTINUITY-SEQUENCE [_ _] {})
(defmethod read-tag :EXT-X-PLAYLIST-TYPE [_ _] {})
(defmethod read-tag :EXT-X-MEDIA [_ _] {})
(defmethod read-tag :EXT-X-DATERANGE [_ _] {})
(defmethod read-tag :EXT-X-INDEPENDENT-SEGMENTS [_ _] {})
(defmethod read-tag :EXT-X-STREAM-INF [_ _] {})
(defmethod read-tag :EXT-X-I-FRAME-STREAM-INF [_ _] {})
(defmethod read-tag :EXT-X-CONTENT-STEERING [_ _] {})
(defmethod read-tag :EXT-X-I-FRAMES-ONLY [_ _] {})
;; END TO-IMPLEMENT

(defmethod read-tag :EXT-X-TARGETDURATION [_ attrs]
  {:target-duration (Integer/parseInt attrs)})

(defmethod read-tag :EXT-X-VERSION [_ attrs]
  {:version (Integer/parseInt attrs)})

(defmethod read-tag :EXT-X-PART-INF [_ attrs]
  (let [{:strs [PART-TARGET]} (parse-attributes attrs)]
    {:part-inf (->PartInf (Float/parseFloat PART-TARGET))}))

(defmethod read-tag :EXT-X-MEDIA-SEQUENCE [_ attrs]
  {:media-sequence-number (Integer/parseInt attrs)})

(defmethod read-tag :EXT-X-SKIP [_ attrs]
  (let [{:strs [SKIPPED-SEGMENTS RECENTLY-REMOVED-DATERANGES]} (parse-attributes attrs)]
    {:skip (->Skip (Integer/parseInt SKIPPED-SEGMENTS)
                   (some-> RECENTLY-REMOVED-DATERANGES (str/split #"\t")))}))

(defmethod read-tag :EXT-X-PRELOAD-HINT [_ attrs]
  (let [{:strs [TYPE URI BYTERANGE-START BYTERANGE-LENGTH]} (parse-attributes attrs)]
    {:preload-hint (->PreloadHint (keyword (str/lower-case TYPE))
                                  URI
                                  (some-> BYTERANGE-START Integer/parseInt)
                                  (some-> BYTERANGE-LENGTH Integer/parseInt))}))

(defmethod read-tag :EXT-X-RENDITION-REPORT [_ attrs]
  (let [{:strs [URI LAST-MSN LAST-PART]} (parse-attributes attrs)]
    {:rendition-report (->RenditionReport URI
                                          (Integer/parseInt LAST-MSN)
                                          (Integer/parseInt LAST-PART))}))

(defmethod read-tag :EXT-X-SERVER-CONTROL [_ attrs]
  (let [{:strs [CAN-BLOCK-RELOAD PART-HOLD-BACK CAN-SKIP-UNTIL]} (parse-attributes attrs)]
    {:server-control (->ServerControl (get truth-table CAN-BLOCK-RELOAD)
                                      (Float/parseFloat PART-HOLD-BACK)
                                      (Float/parseFloat CAN-SKIP-UNTIL))}))

(defmethod read-tag :EXTINF [_ attrs]
  (let [[duration] (str/split attrs #",")]
    {:duration (Float/parseFloat duration)}))

(defmethod read-tag :EXT-X-PART [_ attrs]
  (let [{:strs [DURATION URI INDEPENDENT]} (parse-attributes attrs)]
    {:partial-segment (->PartialSegment (Float/parseFloat DURATION)
                                        URI
                                        (get truth-table INDEPENDENT))}))

(defmethod read-tag :EXT-X-PROGRAM-DATE-TIME [_ attrs]
  {:program-date-time (java.time.Instant/parse attrs)})

(defmethod read-tag :default [tag _]
  (throw (ex-info
          (str "Unable to recognize playlist tag:" tag
               ".\nAll allowed tags may be found here:" 
               "\nhttps://developer.apple.com/documentation/http-live-streaming/enabling-low-latency-http-live-streaming-hls#Utilize-New-Media-Playlist-Tags-for-Low-Latency-HLS")
          {:tag tag})))

(defn parse-media-playlist [lines]
  (let [parse-result (reduce
                      (fn [{:keys [current-segment] :as acc} line]
                        (cond
                          (str/starts-with? line "#EXT")
                          (let [[tag attrs] (str/split line #":" 2)
                                tag (subs tag 1)
                                {:keys [rendition-report partial-segment] :as parsed} (read-tag (keyword tag) attrs)]
                            (cond-> (merge acc parsed)
                              rendition-report (update :rendition-reports conj rendition-report)
                              partial-segment (update-in [:current-segment :partial-segments] conj partial-segment)))

                          (or (str/starts-with? line "#") 
                              (str/blank? line))
                          acc

                          :else
                          (-> acc
                              (update :media-segments conj current-segment)
                              (assoc :current-segment (->MediaSegment nil line [] nil)))))
                      {:media-segments []
                       :rendition-reports []
                       :current-segment (->MediaSegment nil nil [] nil)}
                      lines)
        {:keys [media-segments rendition-reports]} parse-result]
    (map->MediaPlaylist (assoc parse-result
                               :media-segments media-segments
                               :rendition-reports rendition-reports))))

(defn read-media-playlist [reader]
  (let [lines (line-seq (io/reader reader))
        parsed (do 
                 (when (not= (first lines) "#EXTM3U")
                  (throw (ex-info "Invalid playlist format. Expected an #EXTM3U tag at beginning."
                                  {:first-line (first lines)})))
                (parse-media-playlist (rest lines)))]
    (dissoc parsed :current-segment :rendition-report :partial-segment)))

(comment
  (with-open [r (io/reader "test/examples/apple-ex-playlist.m3u8")]
    (read-media-playlist r)))
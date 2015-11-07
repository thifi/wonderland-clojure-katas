(ns alphabet-cipher.coder)

(def a-to-z "abcdefghijklmnopqrstuvwxyz")

(def encode-dict ["abcdefghijklmnopqrstuvwxyz",
                  "bcdefghijklmnopqrstuvwxyza",
                  "cdefghijklmnopqrstuvwxyzab",
                  "defghijklmnopqrstuvwxyzabc",
                  "efghijklmnopqrstuvwxyzabcd",
                  "fghijklmnopqrstuvwxyzabcde",
                  "ghijklmnopqrstuvwxyzabcdef",
                  "hijklmnopqrstuvwxyzabcdefg",
                  "ijklmnopqrstuvwxyzabcdefgh",
                  "jklmnopqrstuvwxyzabcdefghi",
                  "klmnopqrstuvwxyzabcdefghij",
                  "lmnopqrstuvwxyzabcdefghijk",
                  "mnopqrstuvwxyzabcdefghijkl",
                  "nopqrstuvwxyzabcdefghijklm",
                  "opqrstuvwxyzabcdefghijklmn",
                  "pqrstuvwxyzabcdefghijklmno",
                  "qrstuvwxyzabcdefghijklmnop",
                  "rstuvwxyzabcdefghijklmnopq",
                  "stuvwxyzabcdefghijklmnopqr",
                  "tuvwxyzabcdefghijklmnopqrs",
                  "uvwxyzabcdefghijklmnopqrst",
                  "vwxyzabcdefghijklmnopqrstu",
                  "wxyzabcdefghijklmnopqrstuv",
                  "xyzabcdefghijklmnopqrstuvw",
                  "yzabcdefghijklmnopqrstuvwx",
                  "zabcdefghijklmnopqrstuvwxy"])

;; This is so cool! I'm proud of myself.
(defn get-decode-dict [encode-dict]
  (apply map str encode-dict))

(def decode-dict (get-decode-dict encode-dict))

(defn xy->ab [& args]
  (map #(nth a-to-z %) args))

(defn ab->xy [& args]
  (map #(- (int %) (int \a)) args))

(defn dict-lookup [dict col row]
  (let [[x y] (ab->xy col row)]
    (nth (nth dict y) x)))

;; keyword = columns
;; message = rows
(defn encode [keyword message]
  (let [key       (take (count message) (cycle keyword))
        points    (partition 2 (interleave key message))
        res-chars (map #(apply dict-lookup encode-dict %) points)
        cipher    (apply str res-chars)]
    cipher))

(defn decode [keyword message]
  (let [key       (take (count message) (cycle keyword))
        points    (partition 2 (interleave key message))
        res-chars (map #(.indexOf (nth decode-dict (first (ab->xy (first %)))) (str (second %))) points)]
    (apply str (apply xy->ab res-chars)))) ;; refactpr me!!!!!

(defn min-repeat-substr [string]
  (loop [len 1]
    (let [substrings (partition len string)]
      (if (apply = substrings)
        (apply str (first substrings))
        (recur (inc len))))))

(min-repeat-substr "abcabcabc")

(defn decipher [cipher message]
  (let [
        points    (partition 2 (interleave cipher message))
        res-chars (map #(.indexOf (nth encode-dict (first (ab->xy (second %)))) (str (first %))) points)]
    (min-repeat-substr (apply str (apply xy->ab res-chars))))) ;; refactpr me!!!!!

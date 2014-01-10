(ns markov-domains.core
  (:require [clojure.core.async :refer [>! alts!! chan go timeout]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def words
  (lazy-seq
    (with-open [r (io/reader "/usr/share/dict/words")]
      (doall (map (comp string/lower-case string/trim) (line-seq r))))))

(def ascii (for [n (range 97 123)] (char n)))
(defn ascii? [s] (every? (set ascii) s))

(def words (filter ascii? words))
(def words (for [w words] (str "^" w "$")))

(defn inc-key [n] (inc (or n 0)))

(defn word->grams
  [grams-map word n]
  (let [grams (partition n 1 word)]
    (loop [gram (first grams) more (next grams) grams-map grams-map]
      (if more
        (recur (first more)
               (next more)
               (update-in grams-map [gram (first more)] inc-key))
        grams-map))))

(defn words->grams
  [words n]
  (reduce (fn [grams-map word]
            (if (>= (count word) n)
              (let [grams-map (update-in grams-map [:prefix (take n word)] inc-key)]
                (word->grams grams-map word n))
              (word->grams grams-map word n)))
          {} words))

(defn map-vals [f m] (into {} (for [[k v] m] [k (f v)])))

(defn update-vals
  [m]
  (let [sum (reduce + (vals m))]
    (map-vals #(/ % sum) m)))

(defn normalized [grams] (map-vals update-vals grams))

(defn sample
  [gram-map]
  (loop [more gram-map cdf 0 sam (rand)]
    (let [[gram weight] (first more)
          cdf           (-> weight (or 0) (+ cdf))]
      (cond
        (>= cdf sam) gram
        (not more)   (-> gram-map keys rand-nth)
        :else        (recur (next more) cdf sam)))))

(defn multi-word
  [word grams]
  (if (and (-> (last word) (= \$))
           (> (rand) 7/10)
           (< (count word) 8))
    (sample (:prefix grams))
    word))

(defn get-grams [n] (-> words (words->grams n) normalized))
(def get-grams-memo (memoize get-grams))

(defn gen-word
  [n & [prefix]]
  (let [grams (get-grams-memo n)]
    (loop [word (or (seq prefix) (sample (:prefix grams)))]
      (if (-> (last word) (not= \$))
        (let [gram (take-last n word)]
          (if-let [gram (get grams gram)]
            (recur (concat word (->> gram sample (take-last 1))))
            (recur word)))
        (string/replace (apply str (multi-word word grams)) #"\^|\$" "")))))

(defn available?
  [domain]
  (let [ch (chan)]
    (go (>! ch (:out (sh "whois" domain))))
    (re-find #"No match for" (-> (alts!! [ch (timeout 1000)])
                                 first
                                 (or "")))))

(defn find-domains
  [& [n]]
  (while true
    (let [word   (repeatedly 3 #(gen-word (or n 5)))
                 domain (-> (sort-by count word) first (str ".com"))]
      (if (available? domain)
        (prn (str domain " <-- Available"))
        (prn domain)))))

(def cli-options
  [["-n" "--size N" "N sized grams"
    :id :n
    :default 5
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 2 % 8) "Must be a number between 2 and 8"]]])

(defn -main [& args]
  (let [n (-> (parse-opts args cli-options) :options :n)]
    (prn (str "Searching for domains using " n "-grams"))
    (find-domains n)))

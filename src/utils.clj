(ns src.utils)
(require '[src.lang :refer :all]
         '[src.samples :refer :all]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(defn name-is [expr name]
  {:doc "Check if tag name is equal to @name
         typeof name: [string, keyword]"}
  (if (keyword? name)
    (= (tag-name expr) name)
    (= (tag-name expr) (keyword name))))

(defn value-is [expr value]
  {:doc "Check if (first) argument of expr is equal to value"}
  (let [values (tag-args expr)]
    (if (or (= values nil) (> (count values) 1))
      false
      (= (first values) value))))

(defn list-tag-args [tags]
  {:doc "Get list of children from @tags"}
  (filter tag? (reduce
                (fn [acc, x]
                  (concat acc (tag-args x)))
                (list)
                tags)))

(defn values-contain [expr value]
  {:doc "Check if arguments of expr contain value"}
  (let [values (tag-args expr)]
    (some (fn [x] (= x value)) values)))

(defn match-query-tag [expr query]
  (let [name (query :tag)]
    (if (= name "*")
      true
      (name-is expr name))))

;; why do we ever need this?
;; beeing able to process single values and list in same functions
;; is very handy
;; it is also nice to be able to iterate over 
(defn turn-into-list [expr]
  {:doc "Make sure expr is list so it's iterable"}
  (if (tag? expr)
    (list expr)
    (if (seq? expr)
      expr
      (list expr))))

(defn query-matching-expressions [expr query]
  (loop [list-exprs (turn-into-list expr) results []]
    (if (empty? list-exprs)
      results
      (let [first-expr (first list-exprs)]
        (if (match-query-tag first-expr query)
          (recur (rest list-exprs) (conj results first-expr))
          (recur (rest list-exprs) results))))))


(defn filter-value-is [tags value]
  (filter
   (fn [x]
     (value-is x value))
   tags))

(defn filter-values-contain [tags value]
  (filter
   (fn [x]
     (values-contain x value))
   tags))

(defn filter-by-index [tags idx]
  (loop [values tags index 0]
    (if (and (= index idx) (not-empty values))
      (list (first values))
      (if (empty? values)
        (list)
        (recur (rest values) (inc index))))))

(defn process-id-filter [tags query]
  (let [idx (query :id)]
    (filter-by-index tags idx)))

(defn process-is-filter [tags query]
  (let [value (query :is)]
    (filter-value-is tags value)))

(defn process-contains-filter [tags query]
  (let [value (query :contains)]
    (filter-values-contain tags value)))

(def filter-reducer
  {:id process-id-filter
   :is process-is-filter
   :contains process-contains-filter})

(defn process-filters [tags query]
  (let [key (first (second query)) reducer (filter-reducer key)]
    (if-not reducer
      tags ;; if not reducer found query was wrong - just keep going
      (reducer tags query))))

(defn find-query-abs-impl [tags q results]
  (if (empty? tags)
    results ;; looked over all tags - nothing to look for
    (let [matches (query-matching-expressions tags (first q))
          filtered (process-filters matches (first q))]
      (if (<= (count q) 1) ;; last query part - if we match here result is @matches
        (concat results filtered)
        (concat results
                (reduce
                 (fn [acc, x]
                   (concat acc (find-query-abs-impl (list-tag-args (list x)) (rest q) [])))
                 []
                 filtered)))))) ;; bite head and go down

(defn find-query-abs [expr query]
  {:doc "Find elements by query with absolute path;
         @expr - tag or list of tags"}
  (let [list-exprs (turn-into-list expr) queries (turn-into-list query) results []]
    (find-query-abs-impl list-exprs queries results)))


(defn find-query-rel-impl [tags q results]
  (if (empty? tags)
    results ;; looked over all tags - nothing to look for
    (let [matches (query-matching-expressions tags (first q))
          filtered (process-filters matches (first q))]
      (concat results
              filtered
              (find-query-rel-impl (list-tag-args tags) q results)
              (reduce
               (fn [acc, x]
                 (concat acc (find-query-rel-impl (list-tag-args (list x)) (rest q) [])))
               []
               filtered)))))

(defn find-query-rel [expr query]
  {:doc "Find elements by query with relative path;
         @expr - tag or list of tags
         @query - query or list of queries"}
  (let [list-exprs (turn-into-list expr) queries (turn-into-list query) results []]
    (find-query-rel-impl list-exprs queries results)))

(find-query-rel use-list-sample {:tag "br"})

(find-query-rel use-list-sample {:tag "br" :is "br2"})

(defn find-all-query [expr query]
  (let [q-list (turn-into-list query) relative ((first q-list) :rel)]
    (if relative
      (find-query-rel expr query)
      (find-query-abs expr query))))

(defn get-name-and-content [s]
  (let [matches (re-find #"(.*)\[(.*)\]" s)]
    (if (= (count matches) 3)
      (list (second matches) (nth matches 2))
      (list s ""))))

(defn get-q-params [s]
  (let [res (get-name-and-content s) name (first res) content (second res)]
    (if (= (first content) "=")
      (list name :is (subs content 1))
      (if (= (first content) "%")
        (list name :contains (subs content 1))
        (let [num (edn/read-string content) is-number (number? num)]
          (if is-number
            (list name :id num)
            (list name :none nil)))))))

(defn get-rel-name [s is-rel]
  (if is-rel
    (subs s 1)
    s))

(defn transform-into-dict [s]
  (let [is-rel (= "~" (first s))
        text (get-rel-name s is-rel)
        params (get-q-params text)
        name (nth params 0) opts-key (nth params 1) opts-val (nth params 2)]
    {:tag name (keyword opts-key) opts-val :rel is-rel}))

(defn query-from-string [s]
  (map
   (fn [x] (transform-into-dict x))
   (str/split s "/")))

(defn find-all [expr q]
  (find-all-query expr (query-from-string q)))
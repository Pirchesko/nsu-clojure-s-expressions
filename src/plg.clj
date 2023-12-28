(ns src.plg)
(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[src.lang :refer :all]
         '[src.utils :refer :all]
         '[src.samples :refer :all])



;; This file is for testing things it may even be invalid 

(element-type (tag :div))
(element-type "sd")

(string? (::tag "asd"))

(tag? (::tag "asd"))

(tag? (tag :div "ds"))
(tag? "sda")
(tag? (tag :div))

(tag-args (tag :div "asd"))
(second (tag-args (tag :div "1" "2" "3" "4")))
(tag-args (tag :div "asd" "dsa" (tag :br)))
(tag-name (tag :div "asd" "dsa" (tag :br)))

;; matchers

(name-is (tag :div "aa") :div)
(name-is (tag :div "aa") "div")
(name-is (tag :div "aa") :br)

(value-is (tag :div "x") "x")
(value-is (tag :div "x") "d")
(value-is (tag :div "x" "d") "x")

(values-contain (tag :div "x" "d") "x")
(values-contain (tag :div "x" "d") "x2")
(values-contain (tag :div "x" (tag :br)) (tag :div))

(first (tag-args (last (tag-args use-sample))))

(tag :br)

;; ---

(tag-args use-sample)
(tag-args (tag :key))

(str/split "path/d/te" #"/")
(re-matches #"(a+)(b+)(\d+)" "abb234")

;; list of tag predicates (...)
;; {:tag "name of tag" :matcher *condition on value of tag (fn)*}

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

(filter-value-is use-students "bob")
(filter-values-contain use-students "tom")
(filter-by-index use-students 0)
(filter-by-index use-students 10)

(def s-filter-is {:tag "div" :is "name1"})
(def s-filter-id {:tag "div" :id 1})

(= (first (second s-filter-id)) :id)
(first (second match-all-q))

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

(process-filters use-students {:tag "div" :is "name1"})
(process-filters use-students {:tag "div" :id 5})

(defn list-tag-args [tags]
  {:doc "Get list of children from @tags"}
  (filter tag? (reduce
                (fn [acc, x]
                  (concat acc (tag-args x)))
                (list)
                tags)))

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

(find-query-abs use-list-sample (list match-div-q {:tag "br"}))
(find-query-abs use-list-sample (list {:tag "students"} {:tag "student"}))

(find-query-abs use-list-sample
                (list {:tag "students"} {:tag "student" :id 1}))
(find-query-abs use-list-sample (list {:tag "*" :id 2}))
(find-query-abs use-list-sample (list {:tag "*"} {:tag "*" :id 0}))


(defn find-query-rel-impl [tags q results]
  (if (empty? tags)
    results ;; looked over all tags - nothing to look for
    (let [matches (query-matching-expressions tags (first q))
          filtered (process-filters matches (first q))]
      (if (empty? filtered)
        (concat results (find-query-rel-impl (list-tag-args tags) q results))
        (if (and (not-empty filtered) (<= (count q) 1))
          (concat results filtered)
          (concat results (reduce
                           (fn [acc, x]
                             (concat acc (find-query-rel-impl (list-tag-args (list x)) (rest q) [])))
                           []
                           filtered)))))))

(defn find-query-rel [expr query]
  {:doc "Find elements by query with absolute path;
         @expr - tag or list of tags
         @query - query or list of queries"}
  (let [list-exprs (turn-into-list expr) queries (turn-into-list query) results []]
    (find-query-rel-impl list-exprs queries results)))

(defn find-all-query [expr query]
  (let [q-list (turn-into-list query) relative ((first q-list) :rel)]
    (if relative
      (find-query-rel expr query)
      (find-query-abs expr query))))

(find-query-rel use-list-sample (list {:tag "student" :id 1}))
(find-all-query use-list-sample {:tag "student" :rel true})
(find-all-query use-list-sample {:tag "student"})

(find-all-query use-list-sample (list {:tag "*"} {:tag "*"}))
(find-all-query use-list-sample (list {:tag "*" :rel true} {:tag "*" :id 0}))
(find-all-query use-list-sample (list {:tag "*"} {:tag "*" :id 0}))


(list-tag-args use-list-sample)

(str/includes? "asd" "d")

(def use-schema
  (tag :root
       (tag :tank
            (tag :t34 "*")
            (tag :abrams))
       (tag :plane)))

(defn validate-tag-name [tag node]
  (= (tag-name tag) (tag-name node)))

(validate-tag-name (tag :s) (tag "s"))

(defn schema-validation-impl [tags schema]
  (let [first-tag (first tags) first-node (first schema)]
    (if (and (empty? tags) (empty? schema))
      true
      (and
       (and
        (validate-tag-name first-tag first-node)
        (if (value-is first-node "*")
          true
          (schema-validation-impl
           (list-tag-args (list first-tag))
           (list-tag-args (list first-node)))))
       (schema-validation-impl (rest tags) (rest schema))))))

(defn validate-by-schema [tags schema]
  (let [tag-list (turn-into-list tags) schema-list (turn-into-list schema)]
    (schema-validation-impl tag-list schema-list)))

(validate-by-schema (tag :root
                         (tag :tank
                              (tag :t34
                                   (tag :crew))
                              (tag :abrams))
                         (tag :plane))
                    use-schema)

(defn get-name-and-content [s]
  (let [matches (re-find #"(.*)\[(.*)\]" s)]
    (if (= (count matches) 3)
      (list (second matches) (nth matches 2))
      (list s ""))))

(get-name-and-content "div[=text]")
(get-name-and-content "div")

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

(get-q-params "div[=text]")
(get-q-params "div[%text]")
(get-q-params "div[1]")
(get-q-params "div")

(js/parseInt "5")
(number? (edn/read-string "23"))
(if (Integer/parseInt "sd") true false)

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

(transform-into-dict "~div[=text]")
(transform-into-dict "div[=text]")
(transform-into-dict "~div")
(transform-into-dict "div")

(defn query-from-string [s]
  (map
   (fn [x] (transform-into-dict x))
   (str/split s "/")))

(defn find-all [expr q]
  (find-all-query expr (query-from-string q)))

(find-all use-list-sample "students/*")
(find-all use-list-sample "~student")
(find-all use-list-sample "~student[=name1]")
(find-all use-list-sample "div")

(query-from-string "div")
(query-from-string "~div[=t]/br")

(str/split "s/da/d" "/")

(re-find #"(.*)\[(.*)\]" "div[=text]")
(re-find #"(.*)\[(.*)\]" "div")
(re-find #"\[(.*)\]" "div[]")

({:tag "s" :rel true} :key "d")

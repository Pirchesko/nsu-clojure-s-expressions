(ns plg)
(require '[clojure.string :as str]
         '[clojure.edn :as edn])



;; Testing stuff

(defn array [& values]
  (cons ::array values))

(defn tag-args [tag-expr]
  {:doc "Return tag arguments"}
  (first (drop 2 tag-expr)))

(defn tag-name [tag-expr]
  {:doc "Return tag name"}
  (first (rest tag-expr)))

(defn tag [name & children]
  {:doc "Create named tag with children"}
  (if children
    (list ::tag name children)
    (list ::tag name)))

(defn tag? [expr]
  {:doc "Check if expr is tag"}
  (= (key expr) ::tag))

(defn element-type [expr]
  {:doc "what is type of this element"}
  (if (tag? expr)
    ::tag
    (if (string? expr)
      ::sring
      ::rest)))

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


(defn name-is [expr name]
  {:doc "Check if tag name is equal to @name
         typeof name: [string, keyword]"}
  (if (keyword? name)
    (= (tag-name expr) name)
    (= (tag-name expr) (keyword name))))

;; matchers

(defn value-is [expr value]
  {:doc "Check if (first) argument of expr is equal to value"}
  (let [values (tag-args expr)]
    (if (or (= values nil) (> (count values) 1))
      false
      (= (first values) value))))

(defn values-contain [expr value]
  {:doc "Check if arguments of expr contain value"}
  (let [values (tag-args expr)]
    (some (fn [x] (= x value)) values)))

(name-is (tag :div "aa") :div)
(name-is (tag :div "aa") "div")
(name-is (tag :div "aa") :br)

(value-is (tag :div "x") "x")
(value-is (tag :div "x") "d")
(value-is (tag :div "x" "d") "x")

(values-contain (tag :div "x" "d") "x")
(values-contain (tag :div "x" "d") "x2")
(values-contain (tag :div "x" (tag :br)) (tag :div))

;; ------


;; sample data

(def use-sample
  (tag :root
       (tag :div "empty")
       (tag :students
            (tag :student "name1")
            (tag :student "name2")
            (tag :student "name3")
            (tag :student "name4"))))

(first (tag-args (last (tag-args use-sample))))

(def use-students
  (list
   (tag :student "name1")
   (tag :student "name2")
   (tag :student "bob" "tom")))

(def use-list-sample
  (list
   (tag :div "empty")
   (tag :students
        (tag :student "name1")
        (tag :student "name2")
        (tag :student "bob"))
   (tag :div "element")
   (tag :div
        (tag :br "br"))
   (tag :br)
   (tag :br "br here")))
(tag :br)

;; ---

(tag-args use-sample)
(tag-args (tag :key))

(str/split "path/d/te" #"/")
(re-matches #"(a+)(b+)(\d+)" "abb234")

;; list of tag predicates (...)
;; {:tag "name of tag" :matcher *condition on value of tag (fn)*}

(def match-all-q {:tag "*"})
(def match-div-q {:tag "div"})
(def match-br-q {:tag "br"})

(defn match-query-tag [expr query]
  (let [name (query :tag)]
    (if (= name "*")
      true
      (name-is expr name))))

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
    (if (= index idx)
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

(defn find-query-abs-impl [expr queries]
  (loop [tags expr q queries results []]
    (if (empty? tags)
      results ;; looked over all tags - nothing to look for
      (let [matches (query-matching-expressions tags (first q))
            filtered (process-filters matches (first q))
            children (list-tag-args filtered)]
        (if (<= (count q) 1) ;; last query part - if we match here result is @matches
          (concat results filtered)
          (recur children (rest q) results)))))) ;; bite head and go down

(defn find-query-abs [expr query]
  {:doc "Find elements by query with absolute path;
         @expr - tag or list of tags"}
  (let [list-exprs (turn-into-list expr) queries (turn-into-list query)]
    (find-query-abs-impl list-exprs queries)))

(find-query-abs use-list-sample (list match-div-q {:tag "br"}))
(find-query-abs use-list-sample (list {:tag "students"} {:tag "student"}))

(find-query-abs use-list-sample
                (list {:tag "students"} {:tag "student" :id 2}))
(find-query-abs use-list-sample
                (list {:tag "*" :id 2}))

(find-query-abs use-list-sample (list {:tag "student"}))

(str/includes? "asd" "d")

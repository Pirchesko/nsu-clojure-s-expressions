(ns plg)

;; Testing stuff

(defn test1 [x] (+ x 1))

(test1 2)

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
    (list ::tag children)))

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
  {:doc "Check if tag name is equal to @name"}
  (= (tag-name expr) name))

(defn value-is [expr value]
  {:doc "Check if (first) argument of expr is equal to value"}
  (let [values (tag-args expr)]
    (if (or (= values nil) (> (count values) 1))
      false
      (= (first values) value))))

(defn value-contains [expr value]
  {:doc "Check if arguments of expr contain value"}
  (let [values (tag-args expr)]
    (some (fn [x] (= x value)) values)))

(name-is (tag :div "aa") :div)
(name-is (tag :div "aa") :br)

(value-is (tag :div "x") "x")
(value-is (tag :div "x") "d")
(value-is (tag :div "x" "d") "x")

(value-contains (tag :div "x" "d") "x")
(value-contains (tag :div "x" "d") "x2")
(value-contains (tag :div "x" (tag :br)) (tag :div))

(def use-sample
  (tag :root
       (tag :div "empty")
       (tag :students
            (tag :student "name1")
            (tag :student "name2")
            (tag :student "name3")
            (tag :student "name4"))))

(tag-args use-sample)
(tag-args (tag :key))

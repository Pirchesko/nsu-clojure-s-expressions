(ns src.utils)
(require '[src.lang :refer :all])

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

(defn values-contain [expr value]
  {:doc "Check if arguments of expr contain value"}
  (let [values (tag-args expr)]
    (some (fn [x] (= x value)) values)))

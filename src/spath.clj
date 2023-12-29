(ns src.spath)
(require '[src.lang :refer :all]
         '[src.utils :refer :all]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[src.samples :refer :all])

(defn tag-for [query & options]
  {:doc "Create named tag with children"}
  (if options
    (list ::tag-for query options)
    (list ::tag-for query)))

(defn tag-for? [expr]
  {:doc "Check if expr is tag"}
  (= (first expr) ::tag-for))

(defn tag-query [expr]
  (second expr))


(defn repeat-str [x n]
  (str/join "" (repeat n x)))

(defn pad [depth]
  (repeat-str "  " (max depth 0)))

(defn to-string-impl [val depth]
  {:doc "Convert given tags into html string"}
  (if (tag? val)
    (let [name (subs (str (tag-name val)) 1) values (tag-args val)]
      (if (empty? values)
        (str (pad depth) "<" name "/>")
        (str (pad depth) "<" name ">\n"
             (to-string-impl values (inc depth)) "\n"
             (pad depth) "</" name ">")))
    (if (seq? val)
      (str/join "\n" (map (fn [x] (to-string-impl x depth)) val))
      (str (pad depth) "\"" val "\""))))

(defn to-string [val]
  (to-string-impl val 0))


(defn get-options [val]
  (let [len (count val)]
    (if (= len 3)
      (let [opts (first (last val))]
        (if opts
          opts
          {}))
      {})))


(defn process-selector [schema tags]
  (let [values ((get-options schema) :values)]
    (if values
      (list-tag-args tags)
      tags)))


(defn transform-impl [schema val depth]
  (if (tag? schema)
    (let [name (subs (str (tag-name schema)) 1) values (tag-args schema)]
      (if (empty? values)
        (str (pad depth) "<" name "/>")
        (str (pad depth) "<" name ">\n"
             (transform-impl values val (inc depth)) "\n"
             (pad depth) "</" name ">")))
    (if (tag-for? schema)
      (transform-impl
       (process-selector schema (find-all val (tag-query schema)))
       val depth)
      (if (seq? schema)
        (str/join "\n" (map (fn [x] (transform-impl x val depth)) schema))
        (str (pad depth) "\"" schema "\"")))))

(defn transform [schema val]
  (transform-impl schema val 0))

(println (transform (tag-for "*/*") use-list-sample))

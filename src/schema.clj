(ns src.schema)
(require '[src.lang :refer :all]
         '[src.utils :refer :all]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(defn validate-tag-name [tag node]
  (= (tag-name tag) (tag-name node)))

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
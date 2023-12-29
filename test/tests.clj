(ns test.tests)
(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[src.lang :refer :all]
         '[src.utils :refer :all]
         '[src.samples :refer :all]
         '[src.schema :refer :all]
         '[src.spath :refer :all]
         '[clojure.test :refer :all])

(element-type (tag :div))
(element-type "str")

(tag? (tag :div "data"))
(tag? "totally not a tag")
(first (tag-args (tag :div "asd")))
(first (tag-args (tag :div "")))
(tag-name (tag :div (tag :br)))
(tag-name (tag :br (tag :br)))

(name-is (tag :div "data") "div")
(name-is (tag :div "data") "data")

(value-is (tag :div "x") "x")
(value-is (tag :div "x") "d")
(value-is (tag :div "x" "d") "x")

(values-contain (tag :div "x" "d") "x")
(values-contain (tag :div "x" "d") "d")
(values-contain (tag :div) "d")

(filter-by-index use-students 0)
(filter-by-index use-students 10)
(filter-values-contain use-students "tom")

(process-filters use-students {:tag "div" :is "name1"})
(process-filters use-students {:tag "div" :id 1})

(find-all use-list-sample "students/*")
(find-all use-list-sample "~student")
(find-all use-list-sample "~student[=name1]")
(find-all use-list-sample "~student[%bob]")
(find-all use-list-sample "*/*[0]")
(find-all use-list-sample "div[2]")
(find-one use-list-sample "br")
(find-one use-list-sample "nope")

(deftest test-lang
  (testing "lang-types-tests"
    (is (= (element-type (tag :div)) :src.lang/tag))
    (is (= (element-type "str") :src.lang/string)))

  (testing "tag-types"
    (is (= (tag? (tag :div "data")) true))
    (is (= (tag? "totally not a tag") false))
    (is (= (first (tag-args (tag :div "asd"))) "asd"))
    (is (= (first (tag-args (tag :div ""))) ""))
    (is (= (tag-name (tag :div (tag :br))) :div))
    (is (not (= (tag-name (tag :br (tag :br))) :div))))

  (testing "value-is"
    (is (value-is (tag :div "x") "x"))
    (is (not (value-is (tag :div "x") "d")))
    (is (not (value-is (tag :div "x" "d") "x"))))

  (testing "value-contains"
    (is (values-contain (tag :div "x" "d") "x"))
    (is (values-contain (tag :div "x" "d") "d"))
    (is (not (values-contain (tag :div) "d"))))

  (testing "index-filter"
    (is (= (filter-by-index use-students 0) (list (tag :student "name1"))))
    (is (= (filter-by-index use-students 10) (list)))
    (is (= (filter-values-contain use-students "tom") (list (tag :student "bob" "tom")))))

  (testing "Test filters"
    (is (= (process-filters use-students {:tag "div" :is "name1"}) (list (tag :student "name1"))))
    (is (= (process-filters use-students {:tag "div" :id 1}) (list (tag :student "name2")))))

  (testing "Find all/one"
    (is (=
         (find-all use-list-sample "students/*")
         (list (tag :student "name1") (tag :student "name2") (tag :student "bob"))))
    (is (=
         (find-all use-list-sample "~student")
         (list (tag :student "name1") (tag :student "name2") (tag :student "bob"))))
    (is (=
         (find-all use-list-sample "~student[=name1]")
         (list (tag :student "name1"))))
    (is (=
         (find-all use-list-sample "~student[%bob]")
         (list (tag :student "bob"))))
    (is (=
         (find-all use-list-sample "*/*[0]")
         (list (tag :student "name1") (tag :br "br") (tag :br "br2"))))
    (is (=
         (find-all use-list-sample "div[2]")
         (list (tag :div
                    (tag :br "br")))))
    (is (=
         (find-one use-list-sample "br")
         (tag :br)))
    (is (=
         (find-one use-list-sample "nope")
         nil))))

(run-tests `test.tests)
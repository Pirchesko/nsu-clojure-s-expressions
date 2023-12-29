(ns src.samples)
(require '[src.lang :refer :all]
         '[src.utils :refer :all])


(def use-sample
  (tag :root
       (tag :div "empty")
       (tag :students
            (tag :student "name1")
            (tag :student "name2")
            (tag :student "name3")
            (tag :student "name4"))))

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
   (tag :div
        (tag :br "br2"))))

(def use-list-sample2
  (list
   (tag :students
        (tag :student "name1")
        (tag :student "name2")
        (tag :div
             (tag :br "br")))))

;; queries 
(element-type use-sample)

(def match-all-q {:tag "*"})
(def match-div-q {:tag "div"})
(def match-br-q {:tag "br"})

;; schema

(def use-schema
  (tag :root
       (tag :tank
            (tag :t34 "*")
            (tag :abrams))
       (tag :plane)))
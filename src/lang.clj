(ns src.lang)

(defn tag [name & children]
  {:doc "Create named tag with children"}
  (if children
    (list ::tag (keyword name) children)
    (list ::tag (keyword name))))

(defn tag? [expr]
  {:doc "Check if expr is tag"}
  (= (first expr) ::tag))

(defn tag-args [tag-expr]
  {:doc "Return tag arguments"}
  (first (drop 2 tag-expr)))

(defn tag-name [tag-expr]
  {:doc "Return tag name"}
  (first (rest tag-expr)))

(defn element-type [expr]
  {:doc "What is the type of this element"}
  (if (tag? expr)
    ::tag
    (if (string? expr)
      ::sring
      ::rest)))

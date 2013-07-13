(ns def.me)

(defn- create-def [sexp prefix c]
  `(def ~(symbol (str "*" prefix c)) ~(nth sexp c)))

(defmacro def-args [sexp prefix]
  `(do     
     ~@(for [c (range 1 (+ 1 (count (rest sexp))))]
         (create-def sexp prefix c))
     ~sexp))

(defmacro def-let [sexp prefix]
  (let [defs (map second (partition 2 (second sexp)))]
    `(do
       (~(first sexp)
        ~(second sexp)
        ~@(for [c (range (count defs))]           
            (create-def defs prefix c))
        ~@(rest (rest sexp))))))

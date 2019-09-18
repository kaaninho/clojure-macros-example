(ns clojure-macros-example.core)

(println "\n====Blogpost 1====")
(println "\n----Warum DU Makros willst----\n")

;; Variante als Funktion
(defn my-when1
  [pred then]
  (if pred
    then
    nil))

;; Funktioniert, gibt im ersten Fall "Hallo Du", im zweiten nil zurück.
(my-when1 (= 1 1) "Hallo Du")
(my-when1 (= 2 1) "Hallo Du")

;; Funktioniert nicht, printet den String in die Konsole trotz 'false'
(my-when1 (= 2 1) (println "Ich sollte nicht geprintet werden (Funktion)"))

;; Gibt die Liste (if true "Hallo" nil) zurück
(list 'if true "Hallo" nil)

;; Variante als Makro
(defmacro my-when
  [pred then]
  (list 'if pred
        then
        'nil))

;; Funktioniert nun auch wie erwartet, ohne Konsolenprint
(my-when (= 2 1) (println "Ich sollte nicht geprintet werden (Makro)"))

;; Makro-Expansion
(println "Expandiertes my-when: " (macroexpand-1 '(my-when (= 1 1)
                                                           "Hallo!")))

;; Infix-Makro
(defmacro calc-infix
  [form]
  (list (second form) (first form) (nth form 2)))

(println "Calc-Infix: " (calc-infix (2 + 3)))


;;;; Code zum zweiten Blogpost

(println "\n\n====Blogpost 2====")

(println "\n----Apostroph (Quote) und Syntax-Quote----\n")

`(quote (+ 1 2))


(defmacro my-when-mit-quote
  [pred then]
  '(if pred
     then
     nil))

(macroexpand-1 '(my-when-mit-quote (= 1 1) "Hallo"))
;; => (if pred then nil)


;; Normal gequotet
(println '(1 2 (+ 1 2) (+ 2 2)))

;; syntax-gequotet mit unquote
(println `(1 2 ~(+ 1 2) (+ 2 2)))

;; my-when mit syntax-quoting
(defmacro my-when-mit-syntax-quote
  [pred then]
  `(if ~pred
     ~then
     'nil))

(macroexpand-1 '(my-when-mit-syntax-quote (= 1 1) "Hallo"))
;; => (if (= 1 1) "Hallo" nil)

(println "\n----Unquote-Splicing----\n")

(let [thens (list 3 4 5)]
  (println `(1 2 ~thens 6 7))   ; => (1 2 (3 4 5) 6 7)
  (println `(1 2 ~@thens 6 7))) ; => (1 2 3 4 5 6 7)


(defmacro my-when+ [pred & thens]
  `(if ~pred
     (do ~@thens)
     nil))

(macroexpand-1 '(my-when+ (= 1 1) (println "Hallo Hallo") 3))

(println "\n----Makro-Hygiene----\n")

;; Accidental variable capture
(defmacro my-identity [a]
  (list 'let ['x 0] a))

;; Gensym
(println "Ein neues Symbol" (gensym))
(println "Ein neues Symbol mit Präfix" (gensym "blub"))


;; Vermeintliche Identitätsfunktion
(println "15 = " (my-identity 15) "?")
(println "15 = "
         (let [x 12] (my-identity x)) "?")





;;; Code zum dritten Blogpost

(println "\n====Blogpost 3====")

;; Threading operator `my->>`

(defmacro my->> [expr & args]
  (if (empty? args)
    expr
    (let [[f & f-args] (first args)]
      `(my->> (~f ~@f-args ~expr)
              ~@(rest args)))))

(my->> [1 2 3 4 5 6 7 8 9]
       (map inc)
       (remove #(= 0 (mod % 2)))
       (reverse)
       (rest))



(rest (reverse (remove #(= 0 (mod % 2))
                       (map inc
                            [1 2 3 4 5 6 7 8 9]))))


;; Threading operator, anaphorisch
(defmacro as->> [expr name & args]
  (if (empty? args)
    expr
    `(let [~name ~expr]
       (as->> ~(first args) ~name ~@(rest args)))))

(let [it 100]
  (as->> 15 it
         (+ it it)))

(defmacro testi [f]
  (let [it 'it]
    `(let [~it 3]
       ~f)))

(let [it 100]
  (testi (+ 1 it)))






;;; Das Record-Makro
(println "\n----Das Record-Makro----\n")

(defrecord Computer [cpu ram])
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16))

(println (str "Der Office-PC hat " (:ram office-pc) "GB RAM und einen "
              (:cpu office-pc) " Prozessor.\n"))


;; Konstruktor eines Computers
(defn ->>Computer
  [ram-value]
  {:ram ram-value})
(->>Computer 3)


;; 1. Versuch mit Makro
(defmacro def-my-record1
  [type-name field]
  (list 'defn type-name ['arg] {(keyword field) 'arg}))

(println "Expansion def-my-record1: " (macroexpand-1 '(def-my-record1 Computer1 ram)))

(def-my-record1 Computer1 ram)
(Computer1 2)
(:ram (Computer1 2))


(defmacro def-my-record2
  [type-name field]
  (list 'defn (symbol (str "->>" type-name)) '[arg] {(keyword field) 'arg}))

(def-my-record2 Computer2 ram)

(->>Computer2 2)


;; Version mit Praedikat
(defmacro def-my-record3
  [type-name field]
  (list 'do
        (list 'defn (symbol (str "->>" type-name)) '[arg] {(keyword field) 'arg
                                                           :__type__ (str type-name)})
        (list 'defn (symbol (str type-name "?")) '[el] (list '= (str type-name)
                                                             '(:__type__ el)))))

(def-my-record3 Computer3 ram)

(def my-office-pc (->>Computer3 2))
(println "Mein Office-PC hat" (:ram my-office-pc) "GB RAM und ist tatsaechlich"
         "vom Typ 'Computer5', denn: " (Computer3? my-office-pc))

(def-my-record3 Car color)
(println "Expansion (mit Prädikat): " (macroexpand-1 '(def-my-record3 Car color)))

(println "Ein Auto mit Farbe rot: " (->>Car "rot"))

(println "Mein Office-PC ist nicht vom Typ 'Car', denn: " (Car? my-office-pc))

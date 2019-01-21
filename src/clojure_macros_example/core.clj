(ns clojure-macros-example.core)


(println "\n\n----Warum DU Makros willst----\n")

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


;; Variante als Makro
(defmacro my-when
  [pred then]
  (list 'if pred
        then
        'nil))

;; Funktioniert nun auch wie erwartet, ohne Konsolenprint
(my-when (= 2 1) (println "Ich sollte nicht geprintet werden (Makro)"))


;;; Code is Data, Data is Code
(println "\n----Code is Data - Data is Code----\n")

(println "1. " (+ 5 7))

(println "2. " '(+ 5 7))

(println "3. " (eval '(+ 5 7)))

(println "4. " (list + 5 7))

(println "5. " (list '+ 5 7))


;;; Makros
(println "\n----Makros----\n")

(println "Expandiertes my-when: " (macroexpand-1 '(my-when (= 1 1)
                                                           "Hallo!")))

(defmacro calc-infix
  [form]
  (list (second form) (first form) (nth form 2)))

(println "Calc-Infix: " (calc-infix (2 + 3)))

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

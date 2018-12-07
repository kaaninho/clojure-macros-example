(ns clojure-macros-example.core)

;; Variante als Funktion
(defn my-when1
  [pred then]
  (if pred
    then
    nil))

;; Funktioniert, gibt im ersten Fall "Hallo Du", im zweiten nil zurück.
(my-when1 true "Hallo Du")
(my-when1 false "Hallo Du")

;; Funktioniert nicht, printet den String in die Konsole trotz 'false'
(my-when1 false (println "Ich sollte nicht geprintet werden (Funktion)"))

;; Variante als Makro
(defmacro my-when
  [pred then]
  (if pred
    then
    nil))

;; Funktioniert nun auch wie erwartet, ohne Konsolenprint
(my-when false (println "Ich sollte nicht geprintet werden (Makro)"))


;;; Code is Data, Data is Code

(println (+ 5 7))

(println '(+ 5 7))

(println (eval '(+ 5 7)))

(println (list + 5 7))

;; Kein String, sondern Symbol!
(println (pr-str (quote ich-bin-ein-symbol)))
(println (pr-str "ich-bin-ein-String"))

;; Fehlermeldung
;; (println (pr-str ich-bin-ein-symbol))

(println (list '+ 5 7))


;;; Das Record-Makro

(defrecord Computer [cpu ram])
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16))

(println (str "\nDer Office-PC hat " (:ram office-pc) "GB RAM und einen "
              (:cpu office-pc) " Prozessor.\n\n"))


(defn def-my-record1
  [type-name field]
  (defn type-name [arg] {field arg}))

(def-my-record1 'Computer2 'ram)

;; Konstruktoraufruf schlaegt fehl
;; (Computer2 2)
;; Stattdessen die Funktion `type-name`
(type-name 2)


(defn def-my-record2
  [type-name field]
  (eval (list 'defn type-name '[arg] {field 'arg})))

(def-my-record2 'Computer2 :ram)

(Computer2 2)

;; 1. Versuch mit Makro
(defmacro def-my-record3
  [type-name field]
  (list 'defn type-name '[arg] {(keyword field) 'arg}))

(def-my-record3 Computer3 ram)

(Computer3 2)


(defmacro def-my-record4
  [type-name field]
  (list 'defn (symbol (str "make-" type-name)) '[arg] {(keyword field) 'arg}))

(def-my-record4 Computer4 ram)

(make-Computer4 2)


;; Version mit Praedikat
(defmacro def-my-record5
  [type-name field]
  (list 'do
        (list 'defn (symbol (str "make-" type-name)) '[arg] {(keyword field) 'arg
                                                             :__type__ (str type-name)})
        (list 'defn (symbol (str type-name "?")) '[el] (list '= (str type-name)
                                                             '(:__type__ el)))))

(def-my-record5 Computer5 ram)

(def my-office-pc (make-Computer5 2))
(println "Mein Office-PC hat" (:ram my-office-pc) "GB RAM und ist tatsaechlich"
         "vom Typ 'Computer5', denn: " (Computer5? my-office-pc))

(def-my-record5 Car color)

(println "Mein Office-PC hingegen ist nicht vom Typ 'Car', denn: " (Car? my-office-pc))

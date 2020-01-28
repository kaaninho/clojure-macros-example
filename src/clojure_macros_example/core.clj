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

(when-let [x (:name {:name "Kim" :age 42})]
  (str "Hallo " x))

(when-let [x (:last-name {:name "Kim" :age 42})]
  (str "Hallo, Dein Nachname ist " x))

(when-let [m {:person {:name "Kim" :age "42"}}]
  (when-let [person (:person m)]
    (when-let [name (:name person)]
      (str "Die Person-Map " person " enthält den Namen " name))))


(defmacro when-let*
  [bindings & body]
  (assert (vector? bindings) "bindings must be a vector")
  (assert (= 0 (mod (count bindings) 2))
          "when-let* requires an even number of forms in bindings")
  (if (empty? bindings)
    `(do
       ~@body)
    (let [sym (first bindings)
          value (second bindings)]
      `(when-let [~sym ~value]
         (when-let* ~(vec (drop 2 bindings))
           ~@body)))))

(when-let* [m {:person {:name "Kim" :age "42"}}
            person (:person m)
            name (:name person)]
  (str "Die Person-Map " person " enthält den Namen " name))





;;; Das Record-Makro
(println "\n----Das Record-Makro----\n")

(defrecord Computer [cpu ram hard-drive])
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2 500))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16 1000))

(println (str "Der Office-PC hat " (:ram office-pc) "GB RAM, einen "
              (:cpu office-pc) " Prozessor und " (:hard-drive office-pc)
              " GB Festplattenkapazität.\n"))


;; Nicht automatisch generierter Computer-Konstruktor
(defn ->>computer
  [cpu ram hard-drive]
  {:cpu cpu
   :ram ram
   :hard-drive hard-drive})

;; Recordkonstruktor-Erzeugerfunktion
(defn create-record-constructor
  [type-name field-names]
  `(defn ~(symbol (str "->>" type-name))
     ~field-names
     ~(into {}
            (map (fn [field-name]
                   [(keyword field-name) field-name])
                 field-names))))

;; Test
(create-record-constructor 'computer ['cpu 'ram 'hard-drive])


(defmacro def-my-record-1
  [type-name field-names]
  `(do
     ~(create-record-constructor type-name field-names)))

(println "Expansion def-my-record-1: "
         (macroexpand-1 '(def-my-record-1 computer1 [cpu ram hard-drive])))


;; Nicht automatisch generierter Computer-Selektor
(defn computer-cpu
  [computer]
  (:cpu computer))


;; Recordselektoren-Erzeugerfunktion
(defn create-record-accessors
  [type-name field-names]
  (map (fn [field-name]
         `(defn ~(symbol (str type-name "-" field-name))
            [~type-name]
            (~(keyword field-name) ~type-name)))
       field-names))

(defmacro def-my-record-2
  [type-name field-names]
  `(do
     ~(create-record-constructor type-name field-names)
     ~@(create-record-accessors type-name field-names)))

(def-my-record-2 computer-2 [cpu ram hard-drive])
(def office-pc-2 (->>computer-2 "Athlon XP" 2 500))

(computer-2-hard-drive office-pc-2)



;; Prädikat

;; Neue Recordkonstruktor-Erzeugerfunktion
(defn create-record-constructor-2
  [type-name field-names]
  `(defn ~(symbol (str "->>" type-name))
     ~field-names
     ~(vary-meta (into {}
                       (map (fn [field-name]
                              [(keyword field-name) field-name])
                            field-names))
                 (fn [m] (assoc m :__type__ `'~type-name)))))

;; Prädikat-Erzeugerfunktion
(defn create-predicate
  [type-name]
  `(defn ~(symbol (str type-name "?"))
     [~'thing]
     (= '~type-name (:__type__ (meta ~'thing)))))


(defmacro def-my-record-3
  [type-name field-names]
  `(do
     ~@(create-record-accessors type-name field-names)
     ~(create-predicate type-name)
     ~(create-record-constructor-2 type-name field-names)))

(def-my-record-3 car [color])
(def-my-record-3 computer [cpu ram hard-drive])
(def fire-truck (->>car "red"))

(car? fire-truck)
(computer? fire-truck)









;;; Snippets


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




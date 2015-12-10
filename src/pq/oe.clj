(ns pq.oe
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defmacro ignore
  "Cancels the evaluation of an expression, returning nil instead."
  [expr]
  nil)

(ignore (+ 1 2))

(def x 1)
(ignore (def x 2))

(defmacro rev [fun & args]
  (cons fun (reverse args)))

(macroexpand '(rev str "hi" (+ 1 2)))
(eval (macroexpand '(rev str "hi" (+ 1 2))))

(let [x 2] `(inc x))
(let [x 2] `(inc ~x))
(let [x 2] (list 'clojure.core/inc x))

`(foo ~[1 2 3])
`(foo ~@[1 2 3])

;; (require '[clojure.pprint :as ppr])
(pprint (macroexpand '(or a b c d)))

(pprint (clojure.walk/macroexpand-all
         '(or (mossy? stone) (cool? stone) (wet? stone))))

(gensym "hi")

`(let [x# 2] x#)

(print (into {} (dir pq.oe)))

(if (= 2 2) :a :b)

(if (pos? -5)
  (prn "-5 is positive")
  (do
    (prn "-5 is negative")
    (prn "Who would have thought!")))

(when false
  (prn :hi)
  (prn :there))

(when true
  (prn :hi)
  (prn :there))

(when-not (number? "a string")
  :here)

(if-not (vector? (list 1 2 3))
  :a
  :b)

(when-let [x (+ 1 2 3 4)]
  (str x))

(pprint (macroexpand '(when-let [x (+ 1 2 3 4)]
                       (str x))))


(def x 0)
(while (< x 5)
  (prn x)
  (def x (inc x)))

(cond
   (= 2 5) :nope
   (= 3 3) :yep
   (= 5 5) :cant-get-here
   :else   :a-default-value)

(defn category
  "Determines the Saffir-Simpson category of a hurricane,
  by wind speed in meters/sec"
  [wind-speed]
  (condp <= wind-speed
    70 :F5
    58 :F4
    49 :F3
    42 :F2
    :F1)) ; Default value

(category 10)
(category 50)
(category 100)

(defn with-tax
  "Computes the total cost, with tax, of a purchase in the given state."
  [state subtotal]
  (case state
    :WA (* 1.065 subtotal)
    :OR subtotal
    :CA (* 1.075 subtotal)
               ; ... 48 other states ...
    subtotal)) ; a default case

(with-tax :OR 97)
(with-tax :CA 97)
(with-tax :WA 97)


(defn sum [numbers]
  (if-let [n (first numbers)]
    (+ n (sum (rest numbers)))
    0))

(sum (range 100))


(defn sum-tco
  ([numbers]
   (sum-tco 0 numbers))
  ([subtotal numbers]
   (if-let [n (first numbers)]
     (recur (+ subtotal n) (rest numbers))
     subtotal)))

(sum-tco (range 10000000))

(loop [i 0
       nums []]
  (if (< 10 i)
    nums
    (recur (inc i) (conj nums i))))

(defn integers
  [x]
  (lazy-seq
   (cons x (integers (inc x)))))

(def xs (integers  0))
(take 10 xs)

(def x (delay
        (prn "computing a really big number!")
        (last (take 10000000 (iterate inc 0)))))

(deref x)
(realized? x)

(map - (range 10))

(for [x (range 10)]
  (- x))

(for [x [1 2 3]
      y [:a :b]]
  [x y])

(for [x     (range 5)
      y     (range 5)
      :when (and (even? x) (odd? y))]
  [x y])

(pprint (clojure.walk/macroexpand-all
         '(->>
           (range 10)
           (filter odd?)
           (reduce +))))

(reduce + (filter odd? (range 10)))

(reduce +
        (filter odd?
                [9 2 3 1 3 4 5 6 3 4]))

(reduce +
        (filter odd?
                (range 50)))


(->>
 (range 10)
 (filter odd?)
 (reduce +))

(<<-              ;; The Back Arrow (Swiss-Arrows library)
 (reduce +)
 (filter odd?)
 (range 10))

(pprint (clojure.walk/macroexpand-all
         '(-> {:a :b}
              (assoc :c :d)
              (assoc :e :f))))


(-> {:a :b}
     (assoc :c :d)
     (assoc :e :f))


(require '[swiss.arrows :refer :all])


(-<> 2
     (* <> 5)
     (vector 1 2 <> 3 4))

(-<> 'foo {:a <> :b 'bar})
(-<> 10 [1 2 3 <> 4 5])
(-<> :a
     (map <> [{:a 1} {:a 2}])
     (map (partial + 2) <>)
     reverse)




(defn titleize
  [topic]
  (str topic " for the Brave and True"))

(map titleize ["Hamsters" "Ragnarok"])

(map titleize '("Empathy" "Decorating"))

(map titleize #{"Elbows" "Soap Carving"})

(defn label-key-val
  [[key val]]
  (str "key: " key ", val: " val))

(map label-key-val {:name "Edward"
                    :occupation "perennial high-schooler"})

(into {} (map (fn [[key val]] [key (inc val)])
              {:max 30 :min 10}))

(defn to-base [radix n]
  (Integer/toString n radix))

(def base-two (partial to-base 2))

(base-two 9001)

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(mean [1 2 3 4 5 6 7])

(mean [1 1.6 7.4 10 13])

(mean [])

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (int (/ cnt 2))]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
          (mean [bottom-val top-val])))))

(median [7 0 2 3])

(defn mode [coll]
  (let [freqs (frequencies coll)
        occurences (group-by second freqs)
        modes (last (sort occurences))
        modes (->> modes
                   second
                   (map first))]
      modes))

(mode [:alan :bob :alan :greg])

(mode [:smith :carpenter :doe :smith :doe])

(defn standard-deviation [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (- total 1))
        (Math/sqrt))))

(standard-deviation [4 5 2 9 5 7 4 5 4])
(standard-deviation [4 5 5 4 4 2 2 6])

;; 1.22

(defn roll-d6 []
  (inc (rand-int 6)))

(roll-d6)

(rand-nth [1 2 3])
(rand-nth (seq #{:heads :tails}))
(shuffle [1 2 3 4 5 6])

;; 1.23

(require '[clojurewerkz.money.amounts     :as ma])
(require '[clojurewerkz.money.currencies  :as mc])

(def two (ma/amount-of mc/USD 2))

(ma/< two (ma/amount-of mc/USD 2.01))
(ma/total [two two two two])

(ma/round (ma/amount-of mc/USD 3.14) 0 :down)
(ma/convert-to (ma/amount-of mc/AUD 1500.00) mc/USD 0.7234 :down)

(java.util.UUID/randomUUID)

(defn now []
  (java.util.Date.))


(System/currentTimeMillis)

(require '[clj-time.core :as timec])
(require '[clj-time.periodic :as time-period])

(def one-second-ago (now))
(Thread/sleep 1000)
(compare (now) one-second-ago)
(compare one-second-ago (now))
(compare one-second-ago one-second-ago)


(def occurrences
  [#inst "2013-04-06T17:40:57.688-00:00"
   #inst "2002-12-25T00:40:57.688-00:00"
   #inst "2025-12-25T11:23:31.123-00:00"])

(sort occurrences)

(def since-april-first
  (timec/interval (timec/date-time 2015 04 01) (timec/now)))

(timec/in-days since-april-first)
(timec/in-years (timec/interval (timec/date-time 1969 07 20) (timec/now)))
(timec/in-days (timec/interval (timec/date-time 2012 02 28)
                               (timec/date-time 2012 03 01)))
(timec/in-days (timec/interval (timec/date-time 2014 02 28)
                               (timec/date-time 2014 03 01)))

(defn time-range
  "Return a lazy sequence of DateTimes from start to end,
  incremented by 'step' units of time."
  [start end step]
  (let [inf-range (time-period/periodic-seq start step)
        below-end? (fn [t] (timec/within? (timec/interval start end)
                                          t))]
    (take-while below-end? inf-range)))

(def months-of-the-year (time-range (timec/date-time 2012 01)
                                    (timec/date-time 2013 01)
                                    (timec/months 1)))

(realized? months-of-the-year)
(count months-of-the-year)

(-> 1
    timec/days
    timec/from-now)


(-> 3
    timec/days
    timec/ago)

(timec/plus (timec/now) (timec/years 1))
(timec/from-now (timec/years 1))

(-> 1
    timec/years
    timec/from-now)

(def some-date (timec/date-time 2053 12 25))
(timec/minus some-date (timec/years 2))

(def birthday (timec/from-time-zone (timec/date-time 2012 02 18 18)
                                    (timec/time-zone-for-offset -6)))

(def australia-birthday
  (timec/to-time-zone birthday (timec/time-zone-for-id "Australia/Brisbane")))

(compare birthday australia-birthday)

(def la-timezone (timec/time-zone-for-id "America/Los_Angeles"))

(timec/from-time-zone (timec/date-time 2012 01 01) la-timezone)

(defn from-unix-time
  "Return a Java Date object from a a Unix time representation expressed
  in whole seconds."
  [unix-time]
  (java.util.Date. unix-time))

(from-unix-time 1333333333339)

(require '[clj-time.coerce :as timeco])

(defn datetime-from-unix-time
  "Return a DateTime object from a Unix time representation expressed
  in whole seconds"
  [unix-time]
  (timeco/from-long unix-time))

(datetime-from-unix-time 13661275200)

(reverse [1 2 3 4 5])
(sort [2 3 5 4 1])
(sort-by (fn [n] (/ 1 n)) [2 3 5 4 1])

(split-at 4 [:a :b :c :d :e :f])
(partition 2 [:a :b :c :d :e :f])
(partition 2 1 [:a :b :c :d :e :f])

(map pos? [2 1 5 -3 6 -2 -1])

(map + [2 4 8] [1 3 5])

(first [1 2 3 4])
(second [1 2 3 4])
(last [1 2 3 4])
(butlast [1 2 3 4])

(nth [:a :b :c :d :e :f] 5)

(reduce + [1 2 3 4 5])
(reduce (fn [my-map value]
          (assoc my-map value (/ 1 value)))
        {}
        [1 2 3 4 5])

(apply + 1 [2 3])
(apply + (range 1 6))

(empty? [1 2 3 4])
(empty? [])

(some (fn [n] (< n 5)) [6 9 7 3])
(some (fn [n] (< n 5)) [6 9 7 5])

(every? (fn [n] (< n 5)) [2 1 4 3])
(every? (fn [n] (< n 5)) [2 1 5 3])

(def result (map println (range 1 5)))
(doall result)
(realized? result)


(def account1 (ref 1000))
(def account2 (ref 1500))

(defn transfer
  "transfers amount of money from a to be"
  [a b amount]
  (dosync
   (alter a - amount)
   (alter b + amount)))

(transfer account1 account2 300)
(transfer account2 account1 50)
(println "Account #1:" @account1)
(println "Account #2:" @account2)

(def my-contacts (ref []))

(defn add-contact
  "adds a contact to the provided contact list"
  [contacts contact]
  (dosync
   (alter contacts conj (ref contact))))

(defn print-contacts
  "prints a list of contacts"
  [contacts]
  (doseq [c @contacts]
    (println (str "Name: " (@c :lname) ", " (@c :fname)))))

(add-contact my-contacts {:fname "Luke" :lname "VanderHart"})
(add-contact my-contacts {:fname "Stuart" :lname "Sierra"})
(add-contact my-contacts {:fname "John" :lname "Doe"})

(print-contacts my-contacts)

(defn add-initials
  "adds initials to a single contact and returns it"
  [contact]
  (assoc contact :initials
         (str (first (contact :fname)) (first (contact :lname)))))

(defn add-all-initials
  "adds initials to each of the contacts in a list of contacts"
  [contacts]
  (dosync
   (doseq [contact (ensure contacts)]
     (alter contact add-initials))))

(defn print-contacts-and-initials
  "prints a list of contacts, with initials"
  [contacts]
  ;; (dorun (map (fn [c]
  ;;               (println (str "Name: " (@c :lname) ", " (@c :fname) " (" (@c :initials) ")")))
  ;;             @contacts)))

  (doseq [c @contacts]
    (println (str "Name: " (@c :lname) ", " (@c :fname)
                  " (" (@c :initials) ")"))))

(add-all-initials my-contacts)
(print-contacts-and-initials my-contacts)

(def card (promise))
(def dealer (future
              (Thread/sleep 2000)
              (deliver card [(inc (rand-int 13))
                             (rand-nth [:clubs :spades :hearts :diamonds])])))
(deref card)

(def x :mouse)
(def box (fn [] x))
(box)
(def x :cat)

(defn decouple [glider]
  (prn "bolts released"))

(defn launch [glider]
  (decouple glider)
  (prn glider "away!"))

(launch "albatross")

(def ^:dynamic *board* :maple)

(derive java.util.Date ::evil)
(isa? java.util.Date ::evil)
(isa? Float Number)

(defmulti invert class)
(defmethod invert Number [x]
(- x))
(defmethod invert String [x]
  (apply str (reverse x)))

(invert 3.14)
(invert "hello")

(new String)
(new java.util.Date)
(new java.util.Date 55 10 12)
(. Integer valueOf "42")
(Integer/valueOf "42")
(Integer/MAX_VALUE)

(def s "Hello, World!")
(. s substring 0 5)
(map #(.toUpperCase %) ["one" "two" "three"])
(Integer/parseInt "101")
(Integer/MIN_VALUE)

(defmacro triple-do [form]
  (list 'do form form form))

(triple-do (println "test"))
(macroexpand '(triple-do (println "test")))
(macroexpand '(triple-do (do (println "a") (println "b"))))

(defmacro infix [form]
  (cons (second form) (cons (first form) (nnext form))))

(infix (2 + 3))
(macroexpand '(infix (2 + 3)))
(macroexpand '(infix (+ 2 3)))

(set! *warn-on-reflection* true)

(defn nth-char [s n]   ;; Reflection warning
  (.charAt s n))

(defn nth-char [#^String s n]
  (.charAt s n))

(defn nth-char [s n]
  (let [#^String st s]
    (.charAt st n)))

(defn str-replace [#^String s a b]    ;; Reflection warning
  (.replace s a b))

(defn str-replace [#^String s
                   #^CharSequence a
                   #^CharSequence b]
  (.replace s a b))

(defn str-replace [#^String s
                   #^Integer a    ;; incorrect type
                   #^Integer b]   ;; incorrect type
  (.replace s a b))


(class (+ 1 1))

(defn gcd [a b]
  (loop [a (int a), b (int b)]
    (cond (zero? a) b
          (zero? b) a
          (> a b) (recur (- a b) b)
          :else (recur a (- b a)))))

(gcd 76 1000)
(gcd 78 1000)
(gcd 80 1000)
(gcd 10 1000)
(gcd 60 1000)
(gcd 64 1000)

(let [max (int 100)]
  (loop [sum (int 0)
         i (int 1)]
    (if (> i max)
      sum
      (recur (+ sum i) (inc i)))))

(let [max (int 62)
      two (int 2)]
  (loop [total (int 1), n (int 0)]
    (if (== n max)
      total
      (recur (* total two) (inc n)))))

(unchecked-inc Integer/MAX_VALUE)
(unchecked-negate Integer/MIN_VALUE)
(unchecked-divide-int 403 100)

(time (loop [m {}, i 0]
        (if (> i 127)
          m
          (recur (assoc m (char i) i) (inc i)))))

(time (loop [m (transient {}), i 0]
        (if (> i 127)
          (persistent! m)
          (recur (assoc! m (char i) i) (inc i)))))

(loop [m {}, i 96]
  (if (> i 122)
    m
    (recur (assoc m (char i) i) (inc i))))

(loop [m (transient {}), i 96]
  (if (> i 122)
    (persistent! m)
    (recur (assoc! m (char i) i) (inc i))))


(defn sum-down-from [initial-x]
  (loop [sum 0, x initial-x]
    (if (pos? x)
      (recur (+ sum x) (dec x)))))

(defn absolute-value [x]
  (if (pos? x)
    x
    (- x)))

(absolute-value -25)

(let [x 2]
  `(1 ~x 3))

`(1 ~(2 3))

(let [x '(2 3)]
  `(1 ~x))

(let [x '( 2 3)]
  `(1 ~@x))

(new java.awt.Point 0 1)
(java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"})
(new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"})
(type (new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"}))
(.-x (java.awt.Point. 10 20))
(.divide (java.math.BigDecimal. "42") 2M)

(let [origin (java.awt.Point. 0 0)]
  (set! (.-x origin) 15)
  (str origin))

(.endsWith (.toString (java.util.Date.)) "2015")
(.. (java.util.Date.) toString (endsWith "2015"))

(doto (java.util.HashMap.)
  (.put "HOME" "/home/ck")
  (.put "SRC" "src")
  (.put "BIN" "classes"))

(throw (Exception. "I done throwed"))

(defn throw-catch [f]
  [(try
     (f)
     (catch ArithmeticException e "No dividing by zero!")
     (catch Exception e (str "You are so bad " (.getMessage e)))
     (finally (println "returning... ")))])

(throw-catch #(/ 10 5))
(throw-catch #(/ 10 0))
(throw-catch #(throw (Exception. "Crybaby")))

(require '[clojure.set :as s])
(s/intersection #{1 2 3} #{3 4 5})

(require '[clojure.string :refer (capitalize)])
(map capitalize ["kilgore" "trout"])
(import '[java.util HashMap]
        '[java.util.concurrent.atomic AtomicLong])

(HashMap. {"happy?" true})
(AtomicLong. 42)

(if true :truthy :falsey)
(if [] :truthy :falsey)
(if nil :truthy :falsey)
(if false :truthy :falsey)

(find-doc "xor")

(for [x (range 2)
      y (range 2)]
  [x y (bit-or x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-xor x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-and x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-and-not x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-clear x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-flip x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-set x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-shift-left x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-shift-right x y)])

(for [x (range 2)
      y (range 2)]
  [x y (bit-test x y)])

(for [x (range 1)
     y (range 1)]
  [x y (bit-not x) (bit-not y)])


(defn xors [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y (rem (bit-xor x y) 256)]))

(xors 10 10)


;; 4.2

(def frame (java.awt.Frame.))

(for [meth (.getMethods java.awt.Frame)
      :let [name (.getName meth)]]
  name)


(for [meth (.getMethods java.awt.Frame)
      :let [name (.getName meth)]
      :when (re-find #"Size" name)]
  name)

(for [meth (.getMethods java.awt.Frame)
      :let [name (.getName meth)]
      :when (re-find #"hid" name)]
  name)

(for [meth (.getMethods java.awt.Frame)
      :let [name (.getName meth)]
      :when (re-find #"Vis" name)]
  name)

(.isVisible frame)
(.setVisible frame true)
(.setSize frame (java.awt.Dimension. 200 200))
(javadoc frame)

(def gfx (.getGraphics frame))
(.fillRect gfx 12 10 25 40)
(.setColor gfx (java.awt.Color. 255 128 0))
(.fillRect gfx 20 150 75 50)

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(doseq [[x y xor] (xors 500 500)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(defn clear [g]
  (.clearRect g 0 0 200 200))

(clear gfx)

(defn f-values [f xs ys]
  (for [x (range xs)
        y (range ys)]
    [x y (rem (f x y) 256)]))

(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y 1 1)))

(draw-values bit-and 256 256)
(draw-values + 256 256)
(draw-values * 256 256)

(.setVisible frame false)

(let [imadeuapi 3.141599758765876726434720394701928374019273401239470927M]
  (println (class imadeuapi))
  imadeuapi)

(let [butiedit 3.141599758765876726434720394701928374019273401239470927]
  (println (class butiedit))
  butiedit)

(def clueless 9)
(class clueless)
(class (+ clueless 9.0))

(unchecked-add (Long/MAX_VALUE) 0)
(unchecked-add (Long/MAX_VALUE) 1)
(unchecked-add (Long/MAX_VALUE) (Long/MAX_VALUE))

(float 0.00000000000000000000000000000000000000000000000001)

(let [approx-interval (/ 209715 2097152)
      actual-interval (/ 1 10)
      hours           (* 3600 100 10)
      actual-total    (double (* hours actual-interval))
      approx-total    (double (* hours approx-interval))]
  (- actual-total approx-total))

(def population {:zombies 2700, :humans 9})

(get population :zombies)
(:zombies population)

(println (/ (:zombies population)
            (:humans population))
         "zombies per capita")

(println (/ (get population :zombies)
            (get population :humans))
         "zombies per capita")

(identical? 'goat 'goat)
(= 'goat 'goat)

(name 'goat)

(let [x 'goat
      y x]
  (identical? x y))

(let [x (with-meta 'goat {:ornery true})
      y (with-meta 'goat {:ornery false})]
  [(= x y)
   (identical? x y)
   (meta x)
   (meta y)])

(def a-symbol
  'where-am-i)

(resolve 'a-symbol)

(defn best [f xs]
  (reduce #(if (f % %2) % %2) xs))

(best > [1 3 4 2 7 5 3])

(class #"example")
(java.util.regex.Pattern/compile "\\d")
(re-seq #"\w+" "one-two/three")
(re-seq #"\w*(\w)" "one-two/three")

(def ds (into-array [:willie :barnabas :adam]))
(seq ds)
(aset ds 1 :quentin)

(def ds [:willie :barnabas :adam])
(def ds1 (replace {:barnabas :quentin}))



(.toUpperCase "fred")
(.getName String)
(.-x (java.awt.Point. 1 2))
(System/getProperty "java.version")
(Math/PI)
(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
(map (memfn charAt i) ["fred" "ethel" "lucy"] [1 2 3])
(map #(.charAt %1 %2) ["fred" "ethel" "lucy"] [1 2 3])

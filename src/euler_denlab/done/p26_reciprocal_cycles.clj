(ns euler-denlab.done.p26-reciprocal-cycles
  (:refer-clojure :exclude [inc reify ==])
  (:use
   [clojure.core.logic]
   [clojure
    [pprint              :only [pprint pp print-table              ]]
    [repl                :only [doc find-doc apropos               ]]
    [inspector           :only [inspect-tree inspect inspect-table ]]]
   [clojure.java.javadoc :only [javadoc                            ]]
   [clojure.tools.trace  :only [trace deftrace trace-forms trace-ns
                                untrace-ns trace-vars              ]])
  (:require [clojure.java.io     :as io]
            [clojure
             [string             :as s]
             [xml                :as x]
             [walk               :as w]
             [set                :as set]
             [test               :as t]]))

;; Reciprocal cycles
;; Problem 26
;;
;; A unit fraction contains 1 in the numerator. The decimal
;; representation of the unit fractions with denominators 2 to 10 are
;; given:
;;
;; 1/2	= 	0.5
;; 1/3	= 	0.(3)
;; 1/4	= 	0.25
;; 1/5	= 	0.2
;; 1/6	= 	0.1(6)
;; 1/7	= 	0.(142857)
;; 1/8	= 	0.125
;; 1/9	= 	0.(1)
;; 1/10	= 	0.1
;;
;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It
;; can be seen that 1/7 has a 6-digit recurring cycle.
;;
;; Find the value of d < 1000 for which 1/d contains the longest
;; recurring cycle in its decimal fraction part.
;;
;; notes ----------------------------------------------------------------------
;;
;; how to find the cycles

;; general utils --------------------------------------------------------------

(defn- take-while+1 "Like take while, but also include the first element that do not matche the predicate"
  [pred coll] (let [[a b] (split-with pred coll)]
                   (lazy-cat a [(first b)])))

;; actual problem -------------------------------------------------------------

(defn- decimal-raw
  [d] (
       iterate
       (fn [{:keys [num] :as state}]
         (cond (= num d)           {:res 1            :num num         :continue? false}
               (< num d)           {:res nil          :num (* num 10)  :continue? true}
               (zero? (rem num d)) {:res (quot num d) :num num         :continue? false}
               :else               {:res (quot num d) :num (rem num d) :continue? true}))
       {:num 1 :res 0 :continue? true}))

(defn- decimal "returns a (lazy) seq of decimal of a 1/n"
  [n]
  (->> n
       decimal-raw
       (take-while+1 :continue?)
       (map :res)
       (filter identity)))

(comment "decimal" (->> 7
                        decimal
                        (take 25)))

(defn- cycle-nb "Find the lenght of the cycle of 1/n"
  [n] (let [s (->> n
                   decimal-raw
                   (take-while+1 :continue?)
                   (filter :res)
                   (map (fn [x] [(:res x) (:num x)])))

            it (iterate (fn [{seen :seen [f & r] :s}] (cond (seen f)   f
                                                           (empty? r) nil
                                                           :else      {:seen (conj seen f) :s r}))
                        {:seen #{} :s s})
            lst (first (drop-while map? it))
            cycle-at (if lst lst nil)]
        (if cycle-at
          (+ 1 (count (take-while (complement #{cycle-at}) (nnext s))))
          0)))

(t/deftest cycle-nb-test
  (t/are [in ex] (= (cycle-nb in) ex)
         1 0
         2 0
         3 1
         4 0
         5 0
         6 1
         7 6
         8 0
         9 1
         10 0
         13 5))

(defn p26 "Find the d such as 1/d has the biggest cycle, d < n"
  [n] (->> n
           (+ 1)
           (range 1)
           (map (fn [x] [x (cycle-nb x)]))
           (reduce (partial max-key second))
           first))

(comment "solution: "
         (time (p26 1000))
         "Elapsed time: 292.168087 msecs"
         983)

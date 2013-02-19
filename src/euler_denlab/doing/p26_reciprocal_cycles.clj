(ns euler-denlab.doing.p26-reciprocal-cycles
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
  [d]
  (let
      [s (decimal-raw d)
       [a b] (split-with :continue? s)]

    (->> a
         (map :res)
         (filter identity))))

(comment "decimal" (->> 7
                        decimal
                        (take 25)))

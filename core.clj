(ns intseq2.core
  (:require [clojure.math.combinatorics :as com])
  (:require [clojure.math.numeric-tower :as maths]))

;; Code for finding formulas for unknown integer sequences (Numbers 2).
;; Lee Spector (lspector@amherst.edu) 20200219

;; The top-level call function, gp, takes:

;; population-size = the number of individuals in the population
;; generations = the number of generations for which it will run evolution
;; test-pairs = list of the form ((input output)(input output) ...)

;; We will represent an individual as a map with these keys:

;; :genome = a vector of instructions and/or constants
;; :error = total error across test-pairs

;; Programs are evaluated on a stack machine as follows:
;; - Constants are pushed on the stack
;; - Supported instructions pop needed values from the stack and push results on the stack
;; - If there aren't sufficient arguments for an instruction, then it does nothing

(def ingredients '(+ - * / sin cos x 0.0 1.0))
;;expt mod sqrt gcd lcm tan

;;added by lee
(defn factorial [n]
  (loop [current (biginteger n)
         next (dec current)
         total 1]
    (if (> current 1)
      (recur next (dec next) (* total current))
      total)))

(defn error [genome test-pairs]
  "Returns the error of genome in the context of test-pairs."
  (reduce + (for [pair test-pairs]
              (let [input (first pair)
                    output (second pair)]
                (loop [program genome
                       stack ()]
                  ;(println "Program:" program "Stack:" stack)
                  (if (empty? program)
                    (if (empty? stack)
                      1000000.0
                      (Math/abs (double (- output (first stack))))) ;; Math/abs only takes in floating points, which causes the "No matching method abs found taking 1 args"
                    (recur (rest program)
                           (case (first program)
                             + (if (< (count stack) 2)
                                 stack
                                 (cons (+ (second stack) (first stack))
                                       (rest (rest stack))))
                             - (if (< (count stack) 2)
                                 stack
                                 (cons (- (second stack) (first stack))
                                       (rest (rest stack))))
                             * (if (< (count stack) 2)
                                 stack
                                 (cons (* (second stack) (first stack))
                                       (rest (rest stack))))
                             / (if (or (< (count stack) 2)
                                       (zero? (first stack)))
                                 stack
                                 (cons (/ (second stack) (first stack))
                                       (rest (rest stack))))
                             sin (if (< (count stack) 1)
                                   stack
                                   (cons (Math/sin (first stack))
                                         (rest stack)))
                             cos (if (< (count stack) 1)
                                   stack
                                   (cons (Math/cos (first stack))
                                         (rest stack)))
                             ! (if (< (count stack) 1)
                                 stack
                                 (cons (factorial (first stack))
                                       (rest stack)))
                             tan (if (< (count stack) 1)
                                   stack
                                   (cons (Math/tan (first stack))
                                         (rest stack)))
                             log (if (< (count stack) 1)
                                   stack
                                   (cons (Math/log (first stack))
                                         (rest stack)))
                             x (cons input stack)
                             #_(
                                 expt (if (< (count stack) 2)
                                        stack
                                        (cons (long (maths/expt (first stack) (second stack)))
                                              (rest (rest stack))))
                                      mod (if (or (< (count stack) 2) (zero? (second stack)))
                                            stack
                                            (cons (long (mod (first stack) (second stack)))
                                                  (rest (rest stack))))
                                      sqrt (if (< (count stack) 1)
                                             stack
                                             (cons (long (maths/sqrt (first stack)))
                                                   (rest stack)))
                                      gcd (if (< (count stack) 2)
                                            stack
                                            (cons (maths/gcd (second stack) (first stack))
                                                  (rest (rest stack))))
                                      lcm (if (< (count stack) 2)
                                            stack
                                            (cons (maths/gcd (second stack) (first stack))
                                                  (rest (rest stack))))
                                      per (if (< (count stack) 1)
                                            stack
                                            (cons (com/count-permutations (range (first stack)))
                                                  (rest stack)))
                                      comb (if (< (count stack) 2)
                                             stack
                                             (cons (com/count-combinations (range (first stack)) (second stack))
                                                   (rest stack)))

                                      )
                             (cons (first program) stack)))))))))

;; In the following test the program multiplies x by 5.0. For the input 2.0 this will produce
;; 10.0, which is exactly what's specified, so the error for that will be 0. For the second
;; input, 3.0, this will produce 15.0, which will have an error of 1.0 since the specified
;; correct answer is 16.0. For the third input, -0.5, it will produce -2.5, which will have
;; an error of 0.5. So the total error should be 1.5.
#_(error '[5.0 x *]
         '((2.0 10.0) (3.0 16.0) (-0.5 -3.0)))


;;added by lee
;;creates a random formula, we can change ingredients later
(defn new-formula [max-depth]
  (vec (repeatedly max-depth #(if (< (rand) 0.5)
                                (first '(x)) (rand-nth '( + * / - sin cos))))))
;;added by lee
;;creates individual with formula and error key
;;have to adjust how we measure error later
;;I think we are going to discuss as a group later of how long we want to start off our formulas, rn it is 5
;;later the '(3 2) will be replaced with '(what n equals and the integer that coincides with n in the sequence)
;; rn n=3 and 2 is the answer aka the number in the sequence
(defn new-individual [test-pairs]
  (let [form (new-formula 5)]
    {:genome form
     :error  (error form test-pairs)}))

;;previous new-individual
;;(defn new-individual [test-pairs]
  ;;"Returns a new, random individual in the context of test-pairs."
  ;;(let [genome (vec (repeatedly 5 #(rand-nth ingredients)))]
    ;;{:genome genome
     ;;:error  (error genome test-pairs)}))

(defn best [individuals]
  "Returns the best of the given individuals."
  (reduce (fn [i1 i2]
            (if (< (:error i1) (:error i2))
              i1
              i2))
          individuals))

(defn select [population]
  "Returns an individual selected from population using a tournament."
  (best (repeatedly 2 #(rand-nth population))))

(defn mutate [genome]
  "Returns a possibly-mutated copy of genome."
  (let [with-additions (flatten (for [g genome]
                                  (if (< (rand) 1/10)
                                    (shuffle (list g (rand-nth ingredients)))
                                    g)))
        with-deletions (flatten (for [g with-additions]
                                  (if (< (rand) 1/11)
                                    ()
                                    g)))]
    (vec with-deletions)))

(defn crossover [genome1 genome2]
  "Returns a one-point crossover product of genome1 and genome2."
  (let [crossover-point (rand-int (inc (min (count genome1)
                                            (count genome2))))]
    (vec (concat (take crossover-point genome1)
                 (drop crossover-point genome2)))))

(defn make-child [population test-pairs]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (mutate (crossover (:genome (select population))
                                      (:genome (select population))))]
    {:genome new-genome
     :error  (error new-genome test-pairs)}))

(defn report [generation population]
  "Prints a report on the status of the population at the given generation."
  (let [current-best (best population)]
    (println {:generation   generation
              :best-error   (:error current-best)
              :diversity    (float (/ (count (distinct population))
                                      (count population)))
              :average-size (float (/ (->> population
                                           (map :genome)
                                           (map count)
                                           (reduce +))
                                      (count population)))
              :best-genome  (:genome current-best)})))

(defn gp [population-size generations test-pairs elitism]
  "Runs genetic programming to solve, or approximately solve, a floating-point
  symbolic regression problem in the context of the given population-size,
  number of generations to run, and test-pairs."
  (loop [population (repeatedly population-size
                                #(new-individual test-pairs))
         generation 0]
    (report generation population)
    (if (or (< (:error (best population)) 0.1)
            (>= generation generations))
      (best population)
      (if elitism
        (recur (conj (repeatedly (dec population-size)
                           #(make-child population test-pairs))
                     (best population))
               (inc generation))
        (recur (repeatedly population-size
                         #(make-child population test-pairs))
             (inc generation))))))

(def testseq
  (let [seq [1,2,3,4,5]
        ind (range (count seq))]
    (map #(vec [%1 %2]) ind seq)))

;; A simple test, symbolic regression of x^2 + x + 1
(def simple-regression-data
  (for [x (range -2.0 2.0 0.1)]
    [x (+ (* x x) x 1)]))

(def polynomial2
  (for [x (range -10.0 10.0 1.0)]
    [x (+ (* x x) x 1)]))

#_(gp 200 100 simple-regression-data true)

#_(gp 200 100 testseq)
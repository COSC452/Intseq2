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

(def ingredients '(+ - * / x 1 0))

(defn factorial [n] (reduce *' (range 1 (inc n))))

;;returns x^y
(defn pow [x y]
  (reduce * (repeat y x)))

(defn error [genome test-pairs]
  "Returns the error of genome in the context of test-pairs."
  (reduce + (for [pair test-pairs]
              (let [input (first pair)
                    output (second pair)]
                (loop [program genome
                       stack ()]
                  ;;(println "Program:" program "Stack:" stack)
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
                             pow (if (< (count stack) 2)
                                   stack
                                   (cons (pow (second stack) (first stack))
                                         (rest (rest stack))))
                             / (if (or (< (count stack) 2)
                                       (zero? (first stack)))
                                 stack
                                 (cons (long (/ (second stack) (first stack)))
                                       (rest (rest stack))))
                             sin (if (< (count stack) 1)
                                   stack
                                   (cons (long (Math/sin (first stack)))
                                         (rest stack)))
                             cos (if (< (count stack) 1)
                                   stack
                                   (cons (long (Math/cos (first stack)))
                                         (rest stack)))
                             ! (if (< (count stack) 1)
                                 stack
                                 (cons (factorial (first stack))
                                       (rest stack)))
                             tan (if (< (count stack) 1)
                                   stack
                                   (cons (long (Math/tan (first stack)))
                                         (rest stack)))
                             log (if (< (count stack) 1)
                                   stack
                                   (cons (long (Math/log (first stack)))
                                         (rest stack)))
                             x (cons input stack)
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
                             (cons (first program) stack)))))))))

(defn individual-error [genome input output]
  "Returns the error of genome in the context of a single pair."
  (loop [program genome
         stack ()]
    ;;(println "Program:" program "Stack:" stack)
    (if (empty? program)
      (if (empty? stack)
        1000000.0
        (Math/abs (double (- output (first stack)))))       ;; Math/abs only takes in floating points, which causes the "No matching method abs found taking 1 args"
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
               pow (if (< (count stack) 2)
                     stack
                     (cons (pow (second stack) (first stack))
                           (rest (rest stack))))
               / (if (or (< (count stack) 2)
                         (zero? (first stack)))
                   stack
                   (cons (long (/ (second stack) (first stack)))
                         (rest (rest stack))))
               sin (if (< (count stack) 1)
                     stack
                     (cons (long (Math/sin (first stack)))
                           (rest stack)))
               cos (if (< (count stack) 1)
                     stack
                     (cons (long (Math/cos (first stack)))
                           (rest stack)))
               ! (if (< (count stack) 1)
                   stack
                   (cons (factorial (first stack))
                         (rest stack)))
               tan (if (< (count stack) 1)
                     stack
                     (cons (long (Math/tan (first stack)))
                           (rest stack)))
               log (if (< (count stack) 1)
                     stack
                     (cons (long (Math/log (first stack)))
                           (rest stack)))
               x (cons input stack)
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
               (cons (first program) stack))))))

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
  (vec (repeatedly max-depth #(if (< (rand) 0.67)
                                (first '(x)) (rand-nth ingredients)))))
;;added by lee
;;creates individual with formula and error key
(defn new-individual [test-pairs]
  (let [form (new-formula 5)]
    {:genome         form
     :error          (error form test-pairs)
     :lexicase-error 0}))

(defn best [individuals]
  "Returns the best of the given individuals."
  (reduce (fn [i1 i2]
            (if (< (:error i1) (:error i2))
              i1
              i2))
          individuals))

(defn select [population type test-pairs]
  (case type
    :tournament (tournament-select population)
    :lexicase (lexicase-select population test-pairs)
    )
  )

(defn tournament-select [population]
  "Returns an individual selected from population using a tournament."
  (best (repeatedly 20 #(rand-nth population))))

(defn generate-candidate [candidate case]
  (let [input (first case)
        output (second case)]
    (assoc candidate :lexicase-error (individual-error (:genome candidate) input output))))

(defn lexicase-select [population test-pairs]
  "Returns an individual selected from population using lexicase selection"
  (loop [candidates (population)
         cases (shuffle test-pairs)]
    (if (or (= (count candidates) 1)                        ;; based on paper from class
            (= (count cases) 1))
      (rand-nth candidates)                                 ;;in either case, pick a random candidate or it will return the only element in candidate
      (let [lexicase-candidates (map #(generate-candidate % (first cases)) candidates) ;;need to add the test case error to each candidate
            min-error (apply min (map :lexicase-error lexicase-candidates))] ;;just apply min the smallest of the test case errors
        (recur (filter #(= min-error (:lexicase-error %)) lexicase-candidates) (rest cases)) ;;filters out candidates with greater error than the min error
        ))))


(defn mutate [genome add-rate delete-rate]
  "Returns a possibly-mutated copy of genome."
  (let [with-additions (flatten (for [g genome]
                                  (if (< (rand) add-rate)
                                    (shuffle (list g (rand-nth ingredients)))
                                    g)))
        with-deletions (flatten (for [g with-additions]
                                  (if (< (rand) delete-rate)
                                    ()
                                    g)))]
    (vec with-deletions)))

;;Mutate 2 allows for a greater change/chance of mutation
(defn mutate2 [genome]
  (let [with-additions (flatten (for [g genome]
                                  (if (< (rand) 1/4)
                                    (shuffle (list g (rand-nth ingredients)))
                                    g)))
        with-deletions (flatten (for [g with-additions]
                                  (if (< (rand) 1/5)
                                    ()
                                    g)))]
    (vec with-deletions)))

(defn crossover [genome1 genome2]
  "Returns a one-point crossover product of genome1 and genome2."
  (let [crossover-point (rand-int (inc (min (count genome1)
                                            (count genome2))))]
    (vec (concat (take crossover-point genome1)
                 (drop crossover-point genome2)))))

(defn make-child [population test-pairs add-rate delete-rate mutate? crossover? doublemutate? select-type]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [genome1 (:genome (select population select-type test-pairs))
        genome2 (:genome (select population select-type test-pairs))
        crossover12 (crossover genome1 genome2)
        new-genome (if crossover? ;;if crossover
                     (if mutate?  ;;if mutate
                       (mutate crossover12 add-rate delete-rate) ;;new genome is crossover+mutated
                       (if doublemuate? ;;if double mutation
                         (if (> (rand) 1/20) ;; if crossover and double mutation (no mutation)
                           (mutate crossover12 add-rate delete-rate) ;;new genome is crossover+mutated if < 1/2
                           (mutate2 crossover12)) ;;new genome is crossover+mutated2 if > 1/2
                         crossover12)) ;;new genome is just crossover
                     (if mutate? ;; if no crossover
                       (if (< (rand) 1/2) ;; if no crossover but mutate
                         (if (< (rand) 1/2) add-rate delete-rate) ;; mutate genome1 if < 1/2 as newgenome
                         (mutate genome2 add-rate delete-rate)) ;; mutate genome2 if > 1/2 as newgenome
                       (if doublemutate? ;; if double mutation
                         (if (< (rand) 1/20)
                           (if (< (rand) 1/2)
                             (mutate2 genome) ;; mutate2 genome1 if < 1/20 and then < 1/2
                             (mutate2 genome2)) ;; mutate2 genome2 if < 1/20 and the > 1/2
                           (if (< (rand) 1/2)
                             (mutate genome add-rate delete-rate) ;; mutate genome1 if > 1/20 and then < 1/2
                             (mutate genome2 add-rate delete-rate))) ;; mutate genome2 if > /120 and then > 1/2
                       (if (< (rand) 1/2)
                         genome1 ;;if no crossover, no mutate, no doublemuate return genome1 as new genome if < 1/2
                         genome2));;if no crossover, no mutate, no doublemutate return genome2 as new genome if > 1/2
                       ))]
    {:genome         new-genome
     :error          (error new-genome test-pairs)
     :lexicase-error 0}))


;;UMAD + UMAD2 + CROSSOVER + Tournament
(defn make-child_neg1 [population test-pairs add-rate delete-rate]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (if (> (rand) 1/20)
                     (mutate (crossover (:genome (select population))
                                        (:genome (select population))) add-rate delete-rate)
                     (mutate2 (crossover (:genome (select population))
                                         (:genome (select population)))))]
    {:genome new-genome
     :error  (error new-genome test-pairs)}))

;;UMAD + CROSSOVER + TOURNAMENT
(defn make-child1 [population test-pairs add-rate delete-rate]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (mutate (crossover (:genome (select population))
                                      (:genome (select population))) add-rate delete-rate)]
    {:genome         new-genome
     :error          (error new-genome test-pairs)
     :lexicase-error 0}))

;; CROSSOVER + TOURNAMENT
(defn make-child2 [population test-pairs]
  "Returns a new, evaluated child, produced by crossing over parents that are selected from the given population."
  (let [new-genome (crossover (:genome (select population))
                              (:genome (select population)))]
    {:genome new-genome
     :error  (error new-genome test-pairs)}))

;;UMAD + TOURNAMENT
(defn make-child3 [population test-pairs add-rate delete-rate]
  "Returns a new, evaluated child, produced by mutating the result
  of parents that are selected from the given population."
  (let [new-genome (mutate (:genome (select population)) add-rate delete-rate)]
    {:genome new-genome
     :error  (error new-genome test-pairs)}))

;;UMAD + UMAD2 + TOURNAMENT
(defn make-child_neg3 [population test-pairs add-rate delete-rate]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (if (> (rand) 1/20)
                     (mutate (:genome (select population)) add-rate delete-rate)
                     (mutate2 (:genome (select population))))]
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

;;[n] represents different combination of selection and mutation methods
;; n=-1 uses UMAD, UMAD2, crossover, tournament
;; n=1 uses UMAD, crossover, tournament
;; n=2 uses crossover, tournament (this combination always fails)
;; n=3 uses UMAD, tournament
;; n=-3 uses UMAD, UMAD2, tournamnet
;;NEEDS to build a boolean tree later
(defn gp [population-size generations test-pairs elitism add-rate delete-rate n]
  (loop [population (repeatedly population-size
                                #(new-individual test-pairs))
         generation 0]
    (report generation population)
    (if (or (< (:error (best population)) 0.1)
            (>= generation generations))
      (best population)
      (if elitism
        (recur (conj (repeatedly (dec population-size)
                                 (case n
                                   -1 #(make-child_neg1 population test-pairs add-rate delete-rate)
                                   1 #(make-child1 population test-pairs add-rate delete-rate)
                                   2 #(make-child2 population test-pairs)
                                   3 #(make-child3 population test-pairs add-rate delete-rate)
                                   -3 #(make-child_neg3 population test-pairs add-rate delete-rate)))
                     (best population))
               (inc generation))
        (recur (repeatedly population-size
                           (case n
                             -1 #(make-child_neg1 population test-pairs add-rate delete-rate)
                             1 #(make-child1 population test-pairs add-rate delete-rate)
                             2 #(make-child2 population test-pairs)
                             3 #(make-child3 population test-pairs add-rate delete-rate)
                             -3 #(make-child_neg3 population test-pairs add-rate delete-rate)))
               (inc generation))))))

(defn gp-main [population-size generations test-pairs elitism add-rate delete-rate mutate? crossover? select-type]
  "Runs genetic programming to solve, or approximately solve, a
  symbolic regression problem in the context of the given population-size,
  number of generations to run, and test-pairs.
  This version of gp has mutate and crossover as conditionals."
  (loop [population (repeatedly population-size
                                #(new-individual test-pairs))
         generation 0]
    (report generation population)
    (if (or (< (:error (best population)) 0.1)
            (>= generation generations))
      (best population)
      (if elitism
        (recur (conj (repeatedly (dec population-size)
                                 #(make-child population test-pairs add-rate delete-rate mutate? crossover? select-type))
                     (best population))
               (inc generation))
        (recur (repeatedly population-size
                           #(make-child population test-pairs add-rate delete-rate mutate? crossover? select-type))
               (inc generation))))))

(def testseq
  (let [seq [1, 2, 3, 4, 5]
        ind (range (count seq))]
    (map #(vec [%1 %2]) ind seq)))

;; A simple test, symbolic regression of x^2 + x + 1
(def simple-regression-data
  (for [x (range -100 100 1)]
    [x (+ (* x x) x 1)]))

;; x^3 - x + 1
(def polynomial
  (for [x (range -100 100 1)]
    [x (+ 1 (+ x (- (pow x 3) (* x 2))))]))

;; x^3 - x^2 + x+ 1
(def polynomial2
  (for [x (range -100 100 1)]
    [x (+ 1 (+ x (- (pow x 3) (pow x 2))))]))

;; x^5 + x^2 + 6
(def polynomial3 
  (for [x (range -20 20 1)]  
                            [x (+ 6 (+ (* x x) (pow x 5)))]))

#_(gp 200 100 testseq true 1/10 1/11 2)
#_(gp 200 100 simple-regression-data true 1/10 1/11 2)
#_(gp 200 200 polynomial true 1/10 1/11 2)
#_(gp 200 200 polynomial2 true 1/10 1/11 2)
#_(gp 200 200 polynomial3 true 1/10 1/11 -1)

;; n=-1 uses UMAD, UMAD2, crossover, tournament
;; n=1 uses UMAD, crossover, tournament
;; n=2 uses crossover, tournament (this combination always fails)
;; n=3 uses UMAD, tournament
;; n=-3 uses UMAD, UMAD2, tournamnet

;;added by lee
(defn gp_error [population-size generations test-pairs elitism add-rate delete-rate n]
  (loop [population (repeatedly population-size
                                #(new-individual test-pairs))
         generation 0]
    (if (or (< (:error (best population)) 0.1)
            (>= generation generations))
      (best population)
      (if elitism
        (recur (conj (repeatedly (dec population-size)
                                 (case n
                                   -1 #(make-child_neg1 population test-pairs add-rate delete-rate)
                                   1 #(make-child1 population test-pairs add-rate delete-rate)
                                   2 #(make-child2 population test-pairs)
                                   3 #(make-child3 population test-pairs add-rate delete-rate)
                                   -3 #(make-child_neg3 population test-pairs add-rate delete-rate)))
                     (best population))
               (inc generation))
        (recur (repeatedly population-size
                           (case n
                             -1 #(make-child_neg1 population test-pairs add-rate delete-rate)
                             1 #(make-child1 population test-pairs add-rate delete-rate)
                             2 #(make-child2 population test-pairs)
                             3 #(make-child3 population test-pairs add-rate delete-rate)
                             -3 #(make-child_neg3 population test-pairs add-rate delete-rate)))
               (inc generation))))))

(defn average_error [test_seq n]
  (loop [sum 0 run 0]
    (if (>= run 100)
      (println (/ sum 100))
      (recur (+ (:error (gp_error 200 100 test_seq true 1/10 1/11 n)) sum) (inc run)))))

#_(average_error testseq -1)
#_(average_error simple-regression-data -1)
#_(average_error polynomial -1)
#_(average_error polynomial2 -1)
#_(average_error polynomial3  -1)

(defn gp_gen [population-size generations test-pairs elitism add-rate delete-rate n]
  (loop [population (repeatedly population-size
                                #(new-individual test-pairs))
         generation 0]
    (if (or (< (:error (best population)) 0.1)
            (>= generation generations))
      generation
      (if elitism
        (recur (conj (repeatedly (dec population-size)
                                 (case n
                                   -1 #(make-child_neg1 population test-pairs add-rate delete-rate)
                                   1 #(make-child1 population test-pairs add-rate delete-rate)
                                   2 #(make-child2 population test-pairs)
                                   3 #(make-child3 population test-pairs add-rate delete-rate)
                                   -3 #(make-child_neg3 population test-pairs add-rate delete-rate)))
                     (best population))
               (inc generation))
        (recur (repeatedly population-size
                           (case n
                             -1 #(make-child_neg1 population test-pairs add-rate delete-rate)
                             1 #(make-child1 population test-pairs add-rate delete-rate)
                             2 #(make-child2 population test-pairs)
                             3 #(make-child3 population test-pairs add-rate delete-rate)
                             -3 #(make-child_neg3 population test-pairs add-rate delete-rate)))
               (inc generation))))))

(defn average_gen [test_seq n]
  (loop [sum 0 run 0]
    (if (>= run 100)
      (println (/ sum 100))
      (recur (+ (gp_gen 200 100 test_seq true 1/10 1/11 n) sum) (inc run)))))

#_(average_gen testseq -1)
#_(average_gen simple-regression-data -1)
#_(average_gen polynomial -1)
#_(average_gen polynomial2 -1)
#_(average_gen polynomial3  -1)
(ns hours.core
  (:use clojure.test))

;; TIME

(defn current-date []
  (let [calendar (java.util.GregorianCalendar.)]
    {:year (.get calendar java.util.Calendar/YEAR)
     :month  (+ 1 (.get calendar java.util.Calendar/MONTH))
     :day (.get calendar java.util.Calendar/DAY_OF_MONTH)}))

(defn current-time []
  (let [calendar (java.util.GregorianCalendar.)]
    {:hour (.get calendar java.util.Calendar/HOUR_OF_DAY)
     :minute  (.get calendar java.util.Calendar/MINUTE)}))

(defn time-to-minutes [time]
  (+ (* (:hour time)
        60)
     (:minute time)))

(defn time-greater-than? [a b]
  (or (> (:hour a)
         (:hour b))
      (and (= (:hour a)
              (:hour b))
           (> (:minute a)
              (:minute b)))))

#_(tabular
   (fact (time-greater-than? ?a ?b) => ?expected)
   ?a                  ?b                  ?expected
   {:hour 1 :minute 1} {:hour 1 :minute 1} false
   {:hour 2 :minute 1} {:hour 1 :minute 1} true
   {:hour 1 :minute 2} {:hour 1 :minute 1} true
   {:hour 1 :minute 2} {:hour 2 :minute 1} false)

(deftest time-greater-than-test
  (doseq [[a b result] (partition 3 [{:hour 1 :minute 1} {:hour 1 :minute 1} false
                                     {:hour 2 :minute 1} {:hour 1 :minute 1} true
                                     {:hour 1 :minute 2} {:hour 1 :minute 1} true
                                     {:hour 1 :minute 2} {:hour 2 :minute 1} false])]
    (is (= (time-greater-than? a b)
           result))))

(defn minutes-to-time [minutes]
  {:hour (int (Math/floor (/ minutes
                             60)))
   :minute (mod minutes 60)})

(defn time-difference-in-minutes [a b]
  (- (time-to-minutes b)
     (time-to-minutes a)))

(defn time-to-string [time]
  (str (:hour time) ":" (:minute time)))

;; MODEL

(defn create-day []
  (assoc (current-date)
    :sessions []))

(defn create-session []
  {:task "work"
   :start-time (current-time)})

(defn calculate-durations [log]
  (for [[a b] (partition 2 1 log)]
    (assoc a
      :duration-in-minutes (time-difference-in-minutes (:start-time a)
                                                       (:start-time b)))))
(defn sum-up-sessions [day-log]
  (for [task-sessions (vals (group-by :task day-log))]
    {:task (:task (first task-sessions))
     :duration-in-minutes (reduce + (map :duration-in-minutes task-sessions))}))

(defn tasks [log]
  (->> log
       (mapcat :sessions)
       (map :task)
       (apply hash-set)))


(def log [{:year 2013
           :month 10
           :day 10
           :sessions [{:start-time {:hour 8 :minute 0}
                       :task "koodausta"}
                      {:start-time {:hour 14 :minute 0}
                       :task "kahvi"}
                      {:start-time {:hour 14 :minute 30}
                       :task "koodausta"}
                      {:start-time {:hour 15 :minute 0}
                       :task "hommia"}
                      {:start-time {:hour 16 :minute 0}
                       :task "kotiin"}]}
          {:year 2013
           :month 10
           :day 11
           :sessions [{:start-time {:hour 8 :minute 0}
                       :task "koodausta"}
                      {:start-time {:hour 14 :minute 0}
                       :task "kahvi"}
                      {:start-time {:hour 15 :minute 0}
                       :task "koodausta"}
                      {:start-time {:hour 16 :minute 0}
                       :task "kotiin"}]}])


(defn valid? [code-with-checksum]
  (when-let [[_ code checksum] (re-find #"(^\d{6,7})-(\d$)" code-with-checksum)]
    (let [code (if (= 6 (count code))
                 (str "0" code)
                 code)
          modulo (mod (->> (map str code)
                           (map #(Integer/parseInt %))
                           (map * [7 9 10 5 8 4 2])
                           (reduce +))
                      11)]
      (case modulo
        1 false
        0 (= checksum "0")
        (= checksum (str (- 11 modulo)))))))
 
(deftest tunnus-test
  (let [invalid ["12312" "0737546-22" "07375A6-2" "073754622" "0737546-3" "00012-3" "00000012-3" "0000012-z"]
        valid ["1572860-0" "0737546-2" "0000012-3" "000012-3"]]
    (doseq [x invalid] (is (not (valid? x))))
    (doseq [x valid] (is (= false (valid? x))))))

(run-tests)

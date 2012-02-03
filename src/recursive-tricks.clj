(ns recursive-tricks.recursive-tricks)

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn tail-recursive [f]
  (let [state (ref {:func f :first-call true :continue (uuid)})]
    (fn [& args]
      (let [cont (:continue @state)]
        (if (:first-call @state)
          (let [fnc (:func @state)]
            (dosync
              (alter state assoc :first-call false))
            (try
              (loop [targs args]
                (let [result (apply fnc targs)]
                  (if (= result cont)
                    (recur (:args @state))
                    result)))
              (finally
                (dosync (alter state assoc :first-call true)))))
          (dosync
            (alter state assoc :args args)
            cont))))))

(defmacro defrec [name args & body]
  `(def ~name (tail-recursive (fn ~args ~@body))))

(defrec factorial [n acc]
  (if (= n 0)
    acc
    (factorial (- n 1) (* n acc))))

(defn factorial2 [n acc]
  (if (= n 0)
    acc
    (factorial2 (- n 1) (* n acc))))


(println (factorial 10000 (bigint 1)))
;(println (factorial2 10000 (bigint 1)))

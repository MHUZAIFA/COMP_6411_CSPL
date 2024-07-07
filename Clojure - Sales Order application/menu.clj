(ns menu
  (:require [clojure.string :as string]
            [db :refer [display-customers display-products display-sales total-sales-for-customer total-count-for-product]]))

(defn clear-screen []
  (print "\u001b[2J\u001b[H"))

(defn main-loop []
  (loop [running true]
    (clear-screen)
    (println "\n*** Sales Menu ***")
    (println "1. Display Customer Table")
    (println "2. Display Product Table")
    (println "3. Display Sales Table")
    (println "4. Total Sales for Customer")
    (println "5. Total Count for Product")
    (println "6. Exit")
    (println "\nEnter an option: ")
    (let [option (read-line)]
      (cond
        (= option "1") (do (display-customers) (println "\nPress enter key to continue...") (read-line) (recur running))
        (= option "2") (do (display-products) (println "\nPress enter key to continue...") (read-line) (recur running))
        (= option "3") (do (display-sales) (println "\nPress enter key to continue...") (read-line) (recur running))
        (= option "4") (do (println "Enter customer name: ") (total-sales-for-customer (read-line)) (println "\nPress enter key to continue...") (read-line) (recur running))
        (= option "5") (do (println "Enter product description: ") (total-count-for-product (read-line)) (println "\nPress enter key to continue...") (read-line) (recur running))
        (= option "6") (do (println "Good Bye!") (System/exit 0))
        :else (do (println "Invalid option. Please try again.") (recur running))))))

(defn -main []
  (main-loop))

;; Call -main directly to ensure the script runs
(-main)

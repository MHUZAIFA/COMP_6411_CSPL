(ns db
  (:require [clojure.string :as string]))

;; Function to load and parse a file
(defn load-data [filename]
  (if (.exists (java.io.File. filename))
    (->> (slurp filename)
         (string/split-lines)
         (map #(string/split % #"\|"))
         (map #(map string/trim %))
         (map vec))
    []))

;; Load data into memory
(def customers (atom (load-data "cust.txt")))
(def products (atom (load-data "prod.txt")))
(def sales (atom (load-data "sales.txt")))

;; Helper function to get customer name by ID
(defn get-customer-name [cust-id]
  (second (first (filter #(= (first %) (str cust-id)) @customers))))

;; Helper function to get product description by ID
(defn get-product-description [prod-id]
  (second (first (filter #(= (first %) (str prod-id)) @products))))

;; Display functions
(defn display-customers []
  (println "\n** Customer Table **")
  (let [sorted-customers (sort-by #(Integer/parseInt (first %)) @customers)]
      (if (> (count sorted-customers) 1) ; Check if there are records after sorting
        (doseq [customer sorted-customers]
          (println (format "%s: %s" (first customer) (string/join ", " (rest customer)))))
        (println "No records found."))))

(defn display-products []
  (println "\n** Product Table **")
  (if (empty? @products)
    (println "Invalid data.")
    (let [sorted-products (sort-by #(Integer/parseInt (first %)) @products)]
      (doseq [product sorted-products]
        (if (> (count product) 1)
          (println (format "%s: %s" (first product) (string/join ", " (rest product))))
          (println "No records found.")))))) ; Move this inside the doseq block


(defn display-sales []
  (println "\n** Sales Table **")
  (let [sorted-sales (sort-by #(Integer/parseInt (first %)) @sales)]
      (doseq [sale sorted-sales]
        (if (>= (count sale) 3)
          (let [cust-name (get-customer-name (second sale))
                prod-desc (get-product-description (nth sale 2))]
            (println (format "%s: [\"%s\" \"%s\" \"%s\"]" (first sale) cust-name prod-desc (last sale))))
          (println "No records found.")))))



;; Functions for calculating totals
(defn total-sales-for-customer [cust-name]
  (let [cust-id (first (first (filter #(= (second %) cust-name) @customers)))]
    (if cust-id
      (let [total (reduce
                   (fn [sum sale]
                     (if (= (second sale) cust-id)
                       (+ sum (* (Double/parseDouble (nth (first (filter #(= (first %) (nth sale 2)) @products)) 2)) (Integer/parseInt (last sale))))
                       sum))
                   0
                   @sales)]
        (println)
        (println (str cust-name " total purchase: $" (format "%.2f" total))))
      (do
        (println)
        (println (str cust-name " total purchase: $0.00"))))))

(defn total-count-for-product [prod-desc]
  (let [prod-id (first (first (filter #(= (second %) prod-desc) @products)))]
    (if prod-id
      (let [count (reduce
                   (fn [sum sale]
                     (if (= (nth sale 2) prod-id)
                       (+ sum (Integer/parseInt (last sale)))
                       sum))
                   0
                   @sales)]
        (println)
        (println (str prod-desc " count: " count)))
      (do
        (println)
        (println (str prod-desc " count: 0"))))))

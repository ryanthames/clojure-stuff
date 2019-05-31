(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn glitter-filter-list
  [minimum-glitter records]
  (map :name (glitter-filter minimum-glitter records)))

(defn append
  [new-suspect suspects]
  (cons new-suspect suspects))

(def validations {:name #(contains? % :name) :glitter-index #(contains? % :glitter-index)})

(defn validate
  [suspect]
  (every? true? (map #((get validations %) suspect) vamp-keys)))

;; exercise #4
(defn convert-to-csv
  [suspects]
  (clojure.string/join "\n"
                       (map #(clojure.string/join "," %)
                            (map #(vector (:name %) (:glitter-index %)) suspects))))

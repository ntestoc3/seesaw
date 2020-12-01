;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.table
  (:use seesaw.table)
  (:use clojure.test))

(deftest table-model-test
  (testing "should create a table model"
    (instance? javax.swing.table.TableModel (table-model)))

  (testing "should create columns from a list of keys"
    (let [t (table-model :columns [:key1 :key2])]
      (is (= "key1" (.getColumnName t 0)))
      (is (= "key2" (.getColumnName t 1)))))

  (testing "should create columns from a list of maps and keys"
    (let [t (table-model :columns [{:key :key1 :text "KEY1" :class java.lang.Integer} :key2])]
      (is (= "KEY1" (.getColumnName t 0)))
      (is (= java.lang.Integer (.getColumnClass t 0)))
      (is (= "key2" (.getColumnName t 1)))))

  (testing "should create rows from a list of maps"
    (let [t (table-model :columns [:a :b] :rows [{:a "a0" :b "b0"} {:a "a1" :b "b1"}])]
      (is (= 2 (.getRowCount t)))
      (is (= "a0" (.getValueAt t 0 0)))
      (is (= "b0" (.getValueAt t 0 1)))
      (is (= "a1" (.getValueAt t 1 0)))
      (is (= "b1" (.getValueAt t 1 1)))))

  (testing "should create row from a map with extraneous fields without crashing"
    (let [t (table-model :columns [:a] :rows [{:a "a0" :b "b0"}])]
      (is (= "a0" (.getValueAt t 0 0)))))

  (testing "should create rows from a map and retain non-column fields"
    (let [t (table-model :columns [:a] :rows [{:a 99 :b 98}])
          v (value-at t 0)]
      (is (= {:a 99 :b 98} v))))

  (testing "should create rows from a list of vectors"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])]
      (is (= 2 (.getRowCount t)))
      (is (= "a0" (.getValueAt t 0 0)))
      (is (= "b0" (.getValueAt t 0 1)))
      (is (= "a1" (.getValueAt t 1 0)))
      (is (= "b1" (.getValueAt t 1 1)))))

  (testing "should throw IllegalArgumentException if an entry in :rows is not a map or vector"
    (try
      (table-model :columns [:a] :rows [1 2 3 4]) false
      (catch IllegalArgumentException e true)))

  (testing "returns false for isCellEditable"
    (let [t (table-model :columns [:a :b] :rows [[0 0]])]
      (is (not (.isCellEditable t 0 0))))))

(deftest value-at-test
  (testing "gets the value of a single row index as a map"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])]
      (is (= {:a "a0" :b "b0" } (value-at t 0)))))
  (testing "returns non-column values originally inserted in the map"
    (let [t (table-model :columns [:a :b] :rows [{:a 0 :b 1 :c 2} {:a 3 :b 4 :d 5}])]
      (is (= {:a 0 :b 1 :c 2} (value-at t 0)))
      (is (= {:a 3 :b 4 :d 5} (value-at t 1)))))
  (testing "gets the value of a sequence of row indices as a list of maps"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])]
      (is (= [{:a "a0" :b "b0" } {:a "a1" :b "b1" }] (value-at t [0 1])))))
  (testing "returns nil for a nil rows parameter"
    (let [t (table-model :columns [{:key :a :text "a"} {:key :b :text "b"}]
                        :rows [[:a "bee" :b "cee"] [:a "tree" :b "four"]])]
      (is (nil? (value-at t nil)))))
  (testing "survives an out-of-bounds value-at call"
    (let [t (table-model :columns [{:key :a :text "a"} {:key :b :text "b"}]
                         :rows [{:a "bee" :b "cee"} {:a "tree" :b "four"}])]
      (is (= {:a "bee" :b "cee"} (value-at t 0)))
      (try (value-at t 9) (catch Exception e))
      (is (= {:a "bee" :b "cee"} (value-at t 0)))))
  
  )

(deftest update-at!-test
  (testing "updates a row with the same format as :rows option of (table-model)"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])
          r (update-at! t 0 ["A0" "B0"])]
      (is (= t r))
      (is (= {:a "A0" :b "B0"} (value-at t 0)))))
  (testing "updates a only the columns specified in a row"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])
          r (update-at! t 0 {:a "A0"})]
      (is (= t r))
      (is (= {:a "A0" :b "b0"} (value-at t 0)))))
  (testing "preserves the values of unspecified 'hidden' columns"
    (let [t (table-model :columns [:name :phone :email]
                         :rows [{:name "S"
                                 :phone 12345
                                 :email "s@email.com"
                                 :twitter "@s"}])]
      ; :twitter should survive the update even though it's not a column
      ; in the table model
      (update-at! t 0 {:email "s@snailmail.com"})
      (is (= {:name "S" :phone 12345 :email "s@snailmail.com" :twitter "@s"}
                 (value-at t 0)))))
  (testing "updates multiple rows with the same format as :rows option of (table-model)"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])
          r (update-at! t 1 ["A1" "B1"] 0 {:a "A0" :b "B0"})]
      (is (= t r))
      (is (= {:a "A0" :b "B0"} (value-at t 0)))
      (is (= {:a "A1" :b "B1"} (value-at t 1)))))
  (testing "supports `false` boolean values"
    (let [t (table-model :columns [{:class java.lang.Boolean :key :a}]
                         :rows [[false] [true]])
          r (update-at! t 0 [true] 1 [false])]
      (is (= t r))
      (is (= {:a true} (value-at t 0)))
      (is (= {:a false} (value-at t 1))))))

(deftest insert-at!-test
  (testing "inserts a row with the same format as :rows option of (table-model)"
    (let [t (table-model :columns [:a :b] :rows [["a0" "b0"] ["a1" "b1"]])
          r (insert-at! t 0 ["A0" "B0"])]
      (is (= t r))
      (is (= 3 (.getRowCount t)))
      (is (= {:a "A0" :b "B0"} (value-at t 0)))
      (is (= {:a "a0" :b "b0"} (value-at t 1)))))
  (testing "inserts multiple rows with the same format as :rows option of (table-model)"
    (let [t (table-model :columns [:a] :rows (map vector (range 5)))
          r (insert-at! t 1 ["A"] 3 ["B"])]
      (is (= t r))
      (is (= 7 (.getRowCount t)))
      (is (= [{:a 0} {:a "A"} {:a 1} {:a 2} {:a "B"} {:a 3} {:a 4}] (value-at t (range (.getRowCount t)))))))
  (testing "inserts multiple rows without crashing. Issue #146"
    (let [t (table-model :columns [:name] :rows [])
          r (insert-at! t 0 ["A"] 0 ["B"])]
      (is (= t r))
      (is (= 2 (.getRowCount t)))
      (is (= [{:name "A"} {:name "B"}]
                 (value-at t (range (.getRowCount t))))))))

(deftest setRowCount-test
  (testing "can extend the number of rows in the table with nils"
    (let [t (table-model :columns [:a])]
      (.setRowCount t 5)
      (is (= 5 (row-count t)))
      (is (= nil (value-at t 3)))))

  (testing "can reduce the number of rows in the table"
    (let [t (table-model :columns [:a] :rows [[1] [2] [3] [4] [5]])]
      (.setRowCount t 2)
      (is (= 2 (row-count t))))))

(deftest remove-at!-test
  (testing "removes a row"
    (let [t (table-model :columns [:a] :rows (map vector (range 5)))
          r (remove-at! t 2)]
      (is (= t r))
      (is (= 4 (.getRowCount t)))))
  (testing "remove the last row in a table (seesaw issue 49)"
    (let [t (table-model :columns [:a] :rows [])
          _ (insert-at! t 0 {:a 99})
          r (remove-at! t 0)]
      (is (= t r))
      (is (= 0 (.getRowCount t)))))
  (testing "removes multiple rows, assuming that they are sorted!"
    (let [t (table-model :columns [:a] :rows (map vector (range 5)))
          r (remove-at! t 1 2 3)]
      (is (= t r))
      (is (= 2 (.getRowCount t)))
      (is (= [{:a 0} {:a 4}] (value-at t [0 1]))))))

(deftest clear!-test
  (testing "removes all rows from a table"
    (let [t (table-model :columns [:a] :rows (map vector (range 5)))
          r (clear! t)]
      (is (= r t))
      (is (= 0 (.getRowCount t))))))

(deftest row-count-test
  (testing "retrievies number of rows in a table"
    (let [t (table-model :columns [:a] :rows (map vector (range 5)))]
      (is (= 5 (row-count t))))))

(deftest column-count-test
  (testing "retrievies number of columns in a table"
    (let [t (table-model :columns [:a :b :c :d])]
      (is (= 4 (column-count t))))))

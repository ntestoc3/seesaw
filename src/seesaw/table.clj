;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.table
  (:use [seesaw.util :only [illegal-argument]])
  (:require [clojure.core.rrb-vector :as fv])
  (:import javax.swing.table.AbstractTableModel))

(defn- normalize-column
  "格式化column key只能包含:

  `:key` column键

  `:text` column显示的名称

  `:class` column class

  `:editable` column是否可编辑"
  [c]
  (conj {:text  (get c :text ((fnil name c) (:key c)))
         :class (get c :class Object)}
        (if (map? c)
          (select-keys c [:key :text :class :editable])
          {:key c})))

(defn- normalize-row
  [col-info row]
  (cond
    (map? row) row
    (vector? row) (-> (map :key col-info)
                      (zipmap row))
    :else (illegal-argument "row must be a map or vector, got %s" (type row))))

(defn- insert-at [row-vec pos item]
  (println "insert-at:" pos "value:" item)
  (fv/catvec (fv/subvec row-vec 0 pos) [item] (fv/subvec row-vec pos)))

(defn- remove-at [row-vec pos]
  (fv/catvec (fv/subvec row-vec 0 pos) (fv/subvec row-vec (inc pos))))

(comment
  (= [1] (insert-at [] 0 1))

  (= [1 2] (insert-at [1] 1 2))

  (= [1] (remove-at [1 2] 1))

  (= [] (remove-at [1] 0))
  )

(definterface IEditableTableModel
  (^void setRowCount [rows])
  (^void addRow [row-data])
  (^void insertRow [row row-data])
  (^void removeRow [row])
  (^void updateRow [row row-data])
  (getColumnInfo [])
  (getRowData [row]))

(defn- proxy-table-model
  "table model内部表示为[{} {}]"
  [columns-info]
  (let [full-values (atom (fv/vector))
        get-col-name (fn [column]
                       (:text (nth columns-info column)))
        get-col-key (fn [column]
                      (:key(nth columns-info column)))]
    (proxy [AbstractTableModel IEditableTableModel] []
      (getRowCount []
        (count @full-values))
      (getColumnCount []
        (count columns-info))
      (getValueAt [row column]
        (let [row-info (nth @full-values row)
              col-key (get-col-key column)]
          (get row-info col-key)))
      (setValueAt​ [value row column]
        (let [col-key (get-col-key column)
              new-info (-> (nth @full-values row)
                           (assoc col-key value))]
          (swap! full-values assoc row new-info)
          (proxy-super fireTableCellUpdated​ row column)))

	    (isCellEditable​ [row column]
        (:editable (nth columns-info column)))

	    (getColumnName [column]
        (if (<= 0 column (dec (count columns-info)))
          (get-col-name column)
          ""))
      (getColumnClass [column]
        (if (<= 0 column (dec (count columns-info)))
          (:class (nth columns-info column))
          ""))
      (findColumn [column-name]
        (->> (map-indexed vector)
             (filter (comp #(= column-name (:name %1)) second))
             ffirst))

      (setRowCount [rows]
        (let [old-count (count @full-values)]
          (cond
            (= rows old-count)
            nil

            (< rows old-count)
            (do (swap! full-values #(fv/subvec %1 0 rows))
                (proxy-super fireTableRowsDeleted rows (dec old-count)))

            :else
            (do (swap! full-values #(fv/catvec %1
                                               (fv/vec (repeat (- rows old-count)
                                                               nil))))
                (proxy-super fireTableRowsInserted old-count (dec rows))))))

      (addRow [row-data]
        (->> (normalize-row columns-info row-data)
             (swap! full-values conj))
        (let [rows (count @full-values)]
          (proxy-super fireTableRowsInserted rows rows)))

      (insertRow [row row-data]
        (->> (normalize-row columns-info row-data)
             (swap! full-values insert-at row))
        (proxy-super fireTableRowsInserted row row))


      (updateRow [row row-data]
        (let [new-info (-> (nth @full-values row)
                           (merge (normalize-row columns-info row-data)))]
          (swap! full-values assoc row new-info)
          (proxy-super fireTableRowsUpdated row row)))

      (removeRow [row]
        (swap! full-values remove-at row)
        (proxy-super fireTableRowsDeleted row row))

      (getColumnInfo []
        columns-info)

      (getRowData [row]
        (nth @full-values row)))))


(defn table-model
  "Creates a TableModel from column and row data. Takes two options:

    :columns - a list of keys, or maps. If a key, then (name key) is used as the 
               column name. If a map, it can be in the form 
               {:key key :text text :class class :editable bool}
               where key is use to index the row data,
               text (optional) is used as the column name,
               editable (optional) is used to specify the column is editable and 
               class (optional) specifies the object class of the column data
               returned by getColumnClass. The order establishes the order of the
               columns in the table.

    :rows - a sequence of maps or vectors, possibly mixed. If a map, must contain
            row data indexed by keys in :columns. Any additional keys will
            be remembered and retrievable with (value-at). If a vector, data
            is indexed by position in the vector.

  Example:

    (table-model :columns [:name
                           {:key :age :text \"Age\" :class java.lang.Integer}]
                 :rows [ [\"Jim\" 65]
                         {:age 75 :name \"Doris\"}])

    This creates a two column table model with columns \"name\" and \"Age\"
    and two rows.

  See:
    (seesaw.core/table)
    http://download.oracle.com/javase/6/docs/api/javax/swing/table/TableModel.html
  "
  [& {:keys [columns rows] :as opts}]
  (let [norm-cols   (map normalize-column columns)
        model (proxy-table-model norm-cols)]
    (doseq [row rows]
      (.addRow model row))
    model))

(defn- to-table-model [v]
  (cond
    (instance? IEditableTableModel v) v
    ; TODO replace with (to-widget) so (value-at) works with events and stuff
    (instance? javax.swing.JTable v) (.getModel ^javax.swing.JTable v)
    :else (illegal-argument "Can't get table model from %s" v)))

(defn value-at
  "Retrieve one or more rows from a table or table model. target is a JTable or TableModel.
  rows is either a single integer row index, or a sequence of row indices. In the first case
  a single map of row values is returns. Otherwise, returns a sequence of maps.

  If a row index is out of bounds, returns nil.

  Notes:

  If target was not created with (table-model), the returned map(s) are indexed
  by column name.

  Any non-column keys passed to (update-at!) or the initial rows of (table-model)
  are *remembered* and returned in the map.

  Examples:

    ; Retrieve row 3
    (value-at t 3)

    ; Retrieve rows 1, 3, and 5
    (value-at t [1 3 5])

    ; Print values of selected rows
    (listen t :selection
      (fn [e]
        (println (value-at t (selection t {:multi? true})))))
  See:
    (seesaw.core/table)
    (seesaw.table/table-model)
    http://download.oracle.com/javase/6/docs/api/javax/swing/table/TableModel.html
  "
  [target rows]
  (let [target      (to-table-model target)]
    (cond
      (nil? rows)     nil
      (integer? rows) (.getRowData target rows)
      :else           (map #(.getRowData target %) rows))))

(defn update-at!
  "Update a row in a table model or JTable. Accepts an arbitrary number of row/value
  pairs where row is an integer row index and value is a map or vector of values
  just like the :rows property of (table-model).

  Notes:

    Any non-column keys, i.e. keys that weren't present in the original column
    spec when the table-model was constructed will be remembered and retrievable
    later with (value-at).

  Examples:

    ; Given a table created with column keys :a and :b, update row 3 and 5
    (update-at! t 3 [\"Col0 Value\" \"Col1 Value\"]
                  5 { :a \"A value\" \"B value\" })

  See:
    (seesaw.core/table)
    (seesaw.table/table-model)
    http://download.oracle.com/javase/6/docs/api/javax/swing/table/TableModel.html
  "
  ([target row value]
   (-> (to-table-model target)
       (.updateRow row value))
   target)
  ([target row value & more]
   (let [tm (to-table-model target)]
     (.updateRow tm row value)
     (doseq [[r v] (partition 2 more)]
       (.updateRow tm r v)))
   target))

(defn add!
  "Add one or more rows into a table. The arguments are one or more value
  where value is either a map or a vector with the right number of columns.

  Returns target.

  Examples:

    ; add a row at the end of the table
    (add! {:name \"Agent Cooper\" :likes \"Cherry pie and coffee\"})
  "
  ([target value]
   (-> (to-table-model target)
       (.addRow value))
   target)
  ([target value & more]
   (let [tm (to-table-model target)]
     (.addRow tm value)
     (doseq [v more]
       (.addRow tm v)))
   target))

(defn insert-at!
  "Inserts one or more rows into a table. The arguments are one or more row-index/value
  pairs where value is either a map or a vector with the right number of columns. Each
  row index indicates the position before which the new row will be inserted. All indices
  are relative to the starting state of the table, i.e. they shouldn't take any shifting
  of rows that takes place during the insert. The indices *must* be in ascending sorted
  order!!

  Returns target.

  Examples:

    ; Insert a row at the front of the table
    (insert-at! 0 {:name \"Agent Cooper\" :likes \"Cherry pie and coffee\"})

    ; Insert two rows, one at the front, one before row 3
    (insert-at! 0 {:name \"Agent Cooper\" :likes \"Cherry pie and coffee\"}
                3 {:name \"Big Ed\"       :likes \"Norma\"})

  "
  ([target ^Integer row value]
   (-> (to-table-model target)
       (.insertRow row value))
   target)
  ([target row value & more]
   (when more
     (apply insert-at! target more))
   (insert-at! target row value)
   target))

(defn remove-at!
  "Remove one or more rows from a table or table model by index. Args are a list of row indices at
  the start of the operation. The indices *must* be in ascending sorted order!

  Returns target.

  Examples:

    ; Remove first row
    (remove-at! t 0)

    ; Remove first and third row
    (remove-at! t 0 3)
  "
  ([target row]
   (-> (to-table-model target)
       (.removeRow row))
   target)
  ([target row & more]
   (when more
     (apply remove-at! target more))
   (remove-at! target row)
   target))

(defn clear!
  "Clear all rows from a table model or JTable.

  Returns target.
  "
  [target]
  (.setRowCount (to-table-model target) 0)
  target)

(defn row-count
  "Return number of rows in a table model or JTable."
  [target]
  (.getRowCount (to-table-model target)))

(defn column-count
  "Return number of columns in a table model or JTable."
  [target]
  (.getColumnCount (to-table-model target)))


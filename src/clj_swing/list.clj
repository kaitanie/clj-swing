(ns clj-swing.list
  (:use [clj-swing core panel]
	[clojure.contrib.swing-utils :only [do-swing]])
  (:import (javax.swing JList ListModel JTable)
	   (javax.swing.table TableModel)
	   (javax.swing.event ListDataEvent ListDataListener ListSelectionListener)))

(def *list-known-keys* [:action :on-selection-change])


(defmacro add-list-selection-listener [obj [[event] & code]]
  `(doto ~obj
     (.addListSelectionListener
      (proxy [ListSelectionListener] []
	(valueChanged [~event]
			 ~@code)))))

(defmacro list-model [& {[[] & size-code] :size
			 [[get-idx] & get-code] :get
			 [[a-l-l] & add-listener-code] :add-listener
			 [[r-l-l] & remove-listener-code] :remove-listener}]
  `(proxy [ListModel] []
     (getSize []
	      ~@size-code)
     (getElementAt [~get-idx]
		   ~@get-code)
     (addListDataListener [~a-l-l]
			  ~@add-listener-code)
     (removeListDataListener [~r-l-l]
			     ~@remove-listener-code)))

;; void	addTableModelListener(TableModelListener l) OK
;; Adds a listener to the list that is notified each time a change to
;; the data model occurs.
;;
;; Class getColumnClass(int columnIndex) OK
;; Returns the most specific superclass for all the cell values in the
;; column.
;;
;; int	getColumnCount() OK
;; Returns the number of columns in the model.
;;
;; String getColumnName(int columnIndex) OK
;; Returns the name of the column at columnIndex.
;;
;; int getRowCount() OK
;; Returns the number of rows in the model.
;;
;; Object getValueAt(int rowIndex, int columnIndex) OK
;; Returns the value for the cell at columnIndex and rowIndex.
;;
;; boolean isCellEditable(int rowIndex, int columnIndex)
;; Returns true if the cell at rowIndex and columnIndex is editable.
;;
;; void	removeTableModelListener(TableModelListener l) OK
;; Removes a listener from the list that is notified each time a
;; change to the data model occurs.
;;
;; void setValueAt(Object aValue, int rowIndex, int columnIndex)
;; Sets the value in the cell at columnIndex and rowIndex to aValue.

(defmacro table-model [& {[[] & row-count-code] :row-count
			  [[] & column-count-code] :column-count
			  [[col-class-idx] & column-class-code] :column-class
			  [[col-name-idx] & column-name-code] :column-name
			  [[row-editable-idx col-editable-idx] & is-cell-editable-code] :is-cell-editable
			  [[row-get-idx col-get-idx] & get-code] :get
			  [[value-set row-set-idx col-set-idx] & set-code] :set
			  [[a-l-l] & add-listener-code] :add-listener
			  [[r-l-l] & remove-listener-code] :remove-listener}]
  `(proxy [TableModel] []
     (getRowCount []
		  ~@row-count-code)
     (getColumnCount []
		     ~@column-count-code)
     (getColumnClass [~col-class-idx]
		  ~@column-class-code)
     (getColumnName [~col-name-idx]
		    ~@column-name-code)
     (isCellEditable [~row-editable-idx ~col-editable-idx]
		     ~@is-cell-editable-code)
     (getValueAt [~row-get-idx ~col-get-idx]
		   ~@get-code)
     (setValueAt [~value-set ~row-set-idx ~col-set-idx]
		 ~@set-code)
     (addTableModelListener [~a-l-l]
			    ~@add-listener-code)
     (removeTableModelListener [~r-l-l]
			       ~@remove-listener-code)))

(defmacro mapref-table-model-getter [mapref i j]
  `(let [col (nth (vals ~@mapref) ~i)
	 item (nth col ~j)]
     item))

(defn mapref-table-model [map-ref]
  (let [listeners (atom #{})
	key (gensym "map-ref-list-model-watch")
	getter (fn [i j] (let [col (nth (vals @map-ref) i)
			       item (nth col j)]
			   item))
	m (table-model :row-count ([] (count (first (vals @map-ref))))
		       :column-count ([] (count (keys @map-ref)))
		       :column-class ([i] (class (first (nth (vals @map-ref) i))))
		       :column-name ([i] (str (nth (keys @map-ref) i)))
		       :is-cell-editable ([i j] true)
		       :set ([value i j] (str "i = " i " j = " j "value = " value))
		       :get ([i j] (do
				     (nth (nth (into [] (vals @map-ref)) j) i)))
		       :add-listener ([l] (swap! listeners conj l))
		       :remove-listener ([l] (swap! listeners disj l)))]
    (add-watch map-ref key
	       (fn [_ _ _ state]
		 (println "table ref map changed")))
    m))

(defn column-wise-vecref-table-model [col-names-ref data-ref]
  (let [listeners (atom #{})
	key (gensym "vec-ref-list-model-watch")
	cell-set-value! (fn [value i j]
			  (let [selected-column (nth @data-ref i)
				new-column (assoc selected-column j value)
				new-data (assoc @data-ref i new-column)]
			    (dosync (ref-set data-ref (ref new-data)))))
	m (table-model :row-count ([] (count (first @data-ref)))
		       :column-count ([] (count @col-names-ref))
		       :coumn-class ([i] (class (first (nth @data-ref i))))
		       :column-name ([i] (nth @col-names-ref i))
		       :is-cell-editable ([i j] false)
		       :set ([value i j] (println "hoi"))
		       :get ([i j] (print "hoi"))
		       :add-listener ([l] (swap! listeners conj l))
		       :remove-listener ([l] (swap! listeners disj l)))]
    (add-watch data-ref key
	       (fn [_ _ _ state]
		 (prinln "Table data changed!")))
    m))

(defmacro jtable [& {action :action on-selection-change :on-selection-change items :items scrolling :scrolling :as opts}]
  (let [l (gensym "jtable")]
  `(let [~l (doto (JTable.)
	      ~@(if action
		  [`(add-action-listener ~action)])
	      ~@(if on-selection-change
		  [`(add-list-selection-listener ~on-selection-change)])
	      ~@(auto-setters JTable *list-known-keys* opts)
	      ~@(map #(list '.addItem %) items))]
     ~@(if scrolling
	`[(scroll-panel ~l)]
	`[~l]))))

(defn seq-ref-list-model [seq-ref]
  (let [listeners (atom #{})
	key (gensym "seq-ref-list-model-watch")
	m (list-model
	   :size ([] (count @seq-ref))
	   :add-listener ([l] (swap! listeners conj l))
	   :remove-listener ([l] (swap! listeners disj l))
	   :get ([i] (if (has-index? @seq-ref i) (nth @seq-ref i) nil))
	   )]
    (add-watch seq-ref key 
	       (fn [_ _ _ state]
		 (do-swing
		  (let [m (ListDataEvent. m (ListDataEvent/CONTENTS_CHANGED) 0 (count state))]
		    (doseq [l @listeners]
		      (.contentsChanged l m))))))
    m))

(defmacro jlist [& {action :action on-selection-change :on-selection-change items :items scrolling :scrolling :as opts}]
  (let [l (gensym "jlist")]
  `(let [~l (doto (JList.)
	      ~@(if action  
		  [`(add-action-listener ~action)])
	      ~@(if on-selection-change  
		  [`(add-list-selection-listener ~on-selection-change)])    
	      ~@(auto-setters JList *list-known-keys* opts)
	      ~@(map #(list '.addItem %) items))]
     
     ~@(if scrolling 
	`[(scroll-panel ~l)]
	`[~l]))))

;; TODO Add list cell renderer proxy stuff
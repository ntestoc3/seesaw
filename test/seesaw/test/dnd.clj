;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.dnd
  (:use seesaw.dnd)
  (:use [seesaw.graphics]
        [clojure.test]
        )
  (:import [java.awt.datatransfer DataFlavor StringSelection
                                  UnsupportedFlavorException]
           [javax.swing TransferHandler]))

(deftest local-object-flavor-test
  (testing "creates a JVM local flavor for an arbitrary class"
    (let [c (class [])
          f (local-object-flavor c)]
      (is (= (format "%s; class=%s" DataFlavor/javaJVMLocalObjectMimeType (.getName c)) (.getMimeType f)))))
  (testing "creates a JVM local flavor for an arbitrary value"
    (= (local-object-flavor (class [])) (local-object-flavor []))))

(deftest uri-list-flavor-test
  (testing "implements to-remote to convert list of URIs to uri-list"
    (= "http://google.com\r\nhttp://github.com" 
       (to-remote uri-list-flavor 
                  [(java.net.URI. "http://google.com")
                   (java.net.URI. "http://github.com")])))
  (testing "implements to-local to convert uri-list to list of URIs"
    (= [(java.net.URI. "http://google.com") (java.net.URI. "http://github.com")]
       (to-local uri-list-flavor "http://google.com\r\nhttp://github.com" ))))

(deftest default-transferable-test
  (testing "resulting transferable"
    (testing "can hold an arbitrary object"
      (let [o ["hi"]
            t (default-transferable [string-flavor o])]
        (is (identical? o (.getTransferData t (to-raw-flavor string-flavor))))))
    (testing "can hold arbitrary objects or functions"
      (let [t (default-transferable [string-flavor "hi" 
                                     (local-object-flavor Integer) (fn [] 99)])]
        (is (= "hi" (.getTransferData t (to-raw-flavor string-flavor))))
        (is (= 99 (.getTransferData t (to-raw-flavor (local-object-flavor Integer)))))))
    (testing "throws UnsupportedFlavorException correctly"
      (let [t (default-transferable [string-flavor "hi"])]
        (try (.getTransferData t (to-raw-flavor file-list-flavor)) false 
             (catch UnsupportedFlavorException e true))))
    (testing "implements (getTransferDataFlavors)"
      (let [t (default-transferable [(local-object-flavor []) []])
            flavors (.getTransferDataFlavors t)]
        (is (= (to-raw-flavor (local-object-flavor [])) (aget flavors 0)))))
    (testing "implements (isDataFlavorSupported)"
      (let [t (default-transferable [(local-object-flavor []) []])]
        (is (.isDataFlavorSupported t (to-raw-flavor (local-object-flavor []))))
        (is (not (.isDataFlavorSupported t (to-raw-flavor string-flavor))))))))

(defn fake-transfer-support [t]
  (javax.swing.TransferHandler$TransferSupport. (javax.swing.JLabel.) t))

(deftest default-transfer-handler-test
  (testing "(default-transfer-handler)" 
     (testing "creates a transfer handler"
         (instance? javax.swing.TransferHandler (default-transfer-handler)))
     (testing "throws an ex-info if there is a handler-map without an on-drop key"
         (try 
           (default-transfer-handler :import [string-flavor {}]) false
           (catch clojure.lang.ExceptionInfo e true))))

  (testing "(canImport)"
    (testing "returns false if the :import map is missing or empty"
      (not (.canImport (default-transfer-handler) (fake-transfer-support (StringSelection. "hi")))))

    (testing "only accepts flavors in the keys of the :import map"
      (let [th (default-transfer-handler :import [string-flavor (fn [info])])]
        (is (.canImport th (fake-transfer-support (StringSelection. "hi"))))
        (is (not (.canImport th (fake-transfer-support (default-transferable [])))))))
    
    (let [transfer-handler (default-transfer-handler
                             :import [string-flavor {:on-drop   (fn [info])
                                                     :can-drop? (fn [info] 
                                                                  (= info "should match"))}])]
      (testing ":can-drop?"
               (testing "returns false if the import handler is a map and :can-drop? returns false"
                   (not (.canImport transfer-handler  
                          (fake-transfer-support (StringSelection. "should not match")))))
               
               (testing "returns true if the import handler is a map and :can-drop? returns true"
                   (.canImport transfer-handler  
                     (fake-transfer-support (StringSelection. "should match"))))
               
               (let [transfer-handler (default-transfer-handler
                                        :import [string-flavor {:on-drop (fn [info])}])]
                 (testing "returns true if the import handler is a map and :can-drop? is not given"
                     (.canImport transfer-handler  
                       (fake-transfer-support (StringSelection. "should match"))))))))

  (testing "(importData)"
    (testing "returns false immediately if (canImport) returns false"
      (let [called (atom false)
            th (default-transfer-handler :import [string-flavor (fn [info] (reset! called true))])]
        (is (not (.importData th (fake-transfer-support (default-transferable [])))))
        (is (not @called))))

    (testing "calls the handler for the first matching flavor"
      (let [called (atom nil)
            th (default-transfer-handler :import [string-flavor (fn [info] (reset! called info) true)])
            support (fake-transfer-support (StringSelection. "Something"))]
        (is (.importData th support))
        (is (= @called {:data "Something"
                            :drop? false
                            :drop-location nil
                            :target (.getComponent support)
                            :support support})))))

  (testing "(createTransferable)"
    (testing "returns a transferable given :import/:start "
      (let [c (javax.swing.JTextField. "some text")
            th (default-transfer-handler :export { :start (fn [c] [string-flavor (.getText c)]) })
            trans (.createTransferable th c)]
        (is (= "some text" (.getTransferData trans (to-raw-flavor string-flavor)))))))

  (testing "(getSourceActions)"
    (testing "returns :none if :export is omitted"
      (let [c (javax.swing.JTextField. "some text")
            th (default-transfer-handler)
            actions (.getSourceActions th c)]
        (is (= TransferHandler/NONE actions))))
    (testing "returns :none if the provided function returns nil"
      (let [c (javax.swing.JTextField. "some text")
            th (default-transfer-handler :export { :actions (fn [c] nil) })
            actions (.getSourceActions th c)]
        (is (= TransferHandler/NONE actions))))
    (testing "returns :move by default"
      (let [c (javax.swing.JTextField. "some text")
            th (default-transfer-handler :export {})
            actions (.getSourceActions th c)]
        (is (= TransferHandler/MOVE actions))))
    (testing "returns the result of calling the provided function"
      (let [c (javax.swing.JTextField. "some text")
            th (default-transfer-handler :export { :actions (fn [c] :link) })
            actions (.getSourceActions th c)]
        (is (= TransferHandler/LINK actions)))))
  (testing "(exportDone)"
    (testing "returns false if :export is omitted"
      (let [th (default-transfer-handler)]
        (is (not (.exportDone th nil nil TransferHandler/MOVE)))))
    (testing "returns false if :export/:finish is omitted"
      (let [th (default-transfer-handler :export {})]
        (is (not (.exportDone th nil nil TransferHandler/MOVE)))))
    (testing "calls the :export/:finish function with a map"
      (let [source (javax.swing.JTextField. "some text")
            tr (default-transferable [string-flavor "hi" (local-object-flavor Integer) (fn [] 99)])
            called (atom nil)
            th (default-transfer-handler :export { :finish (fn [v] (reset! called v) true) })]
        (.exportDone th source tr TransferHandler/MOVE)
        (is (= {:source source :data tr :action :move} @called))))))



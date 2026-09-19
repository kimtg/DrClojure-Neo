(ns DrClojure.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [DrClojure.core :as core]
            [DrClojure.ui :as ui])
  (:import (javax.swing JTextPane KeyStroke)
           (javax.swing.text DefaultStyledDocument)))

(deftest app-metadata-test
  (testing "App title and version"
    (is (string? core/app-title))
    (is (.contains core/app-title "DrClojure"))
    (is (= "0.3.0" ui/app-version))))

(deftest bracket-matching-test
  (testing "Matching parentheses"
    (let [code "(+ (* 2 3) 4)"]
      (is (= 12 (ui/find-matching-bracket code 0)))
      (is (= 0 (ui/find-matching-bracket code 12)))
      (is (= 9 (ui/find-matching-bracket code 3)))
      (is (= 3 (ui/find-matching-bracket code 9)))))

  (testing "Matching brackets and braces"
    (let [code "[1 { :a 2 } 3]"]
      (is (= 13 (ui/find-matching-bracket code 0)))
      (is (= 0 (ui/find-matching-bracket code 13)))
      (is (= 10 (ui/find-matching-bracket code 3)))
      (is (= 3 (ui/find-matching-bracket code 10)))))

  (testing "Brackets inside comments and strings are ignored"
    (let [code "(let [s \"(foo)\"] ;; )\n (+ 1 2))"]
      ;; Index 0 '(' matches index 30 ')'
      (is (= 30 (ui/find-matching-bracket code 0)))
      (is (= 0 (ui/find-matching-bracket code 30)))
      ;; Parens inside string at pos 9 and 13 return nil
      (is (nil? (ui/find-matching-bracket code 9)))
      (is (nil? (ui/find-matching-bracket code 13)))
      ;; Paren inside comment at pos 20 returns nil
      (is (nil? (ui/find-matching-bracket code 20)))))

  (testing "Unmatched bracket returns nil"
    (let [code "(+ 1 2"]
      (is (nil? (ui/find-matching-bracket code 0))))))

(deftest ui-creation-test
  (testing "IDE Frame creation and title"
    (let [frame (ui/create-ide nil)]
      (is (instance? javax.swing.JFrame frame))
      (is (.contains (.getTitle frame) "DrClojure"))
      (is (.contains (.getTitle frame) "Untitled"))
      (is (some? (.getJMenuBar frame)))
      (is (= 5 (.getMenuCount (.getJMenuBar frame))))
      (.dispose frame))))

(deftest cheatsheet-config-test
  (testing "Cheatsheet URL points to official Clojure cheatsheet"
    (is (= "https://clojure.org/api/cheatsheet" ui/cheatsheet-url))))

(deftest dirty-flag-test
  (testing "Clean title on new and opened file"
    (let [frame-new (ui/create-ide nil)
          title-new (.getTitle frame-new)]
      (is (not (.startsWith title-new "*")))
      (is (.contains title-new "Untitled"))
      (.dispose frame-new))
    (let [frame-file (ui/create-ide "deps.edn")
          title-file (.getTitle frame-file)]
      (is (not (.startsWith title-file "*")))
      (is (.contains title-file "deps.edn"))
      (.dispose frame-file))))

(deftest indent-unindent-test
  (testing "Single-line unindent removes leading spaces"
    (let [pane (JTextPane.)]
      (.setText pane "  (println \"hello\")")
      (.setCaretPosition pane 0)
      (ui/unindent-selection! pane)
      (is (= "(println \"hello\")" (.getText pane)))))

  (testing "Single-line unindent removes leading tab"
    (let [pane (JTextPane.)]
      (.setText pane "\t(println \"hello\")")
      (.setCaretPosition pane 0)
      (ui/unindent-selection! pane)
      (is (= "(println \"hello\")" (.getText pane)))))

  (testing "Single-line unindent on zero-space line is a no-op"
    (let [pane (JTextPane.)]
      (.setText pane "(println \"hello\")")
      (.setCaretPosition pane 0)
      (ui/unindent-selection! pane)
      (is (= "(println \"hello\")" (.getText pane)))))

  (testing "Multi-line block indent"
    (let [pane (JTextPane.)]
      (.setText pane "line 1\nline 2")
      (.setSelectionStart pane 0)
      (.setSelectionEnd pane (.length (.getText pane)))
      (ui/indent-selection! pane)
      (is (= "  line 1\n  line 2" (.getText pane)))))

  (testing "Multi-line block unindent"
    (let [pane (JTextPane.)]
      (.setText pane "  line 1\n  line 2")
      (.setSelectionStart pane 0)
      (.setSelectionEnd pane (.length (.getText pane)))
      (ui/unindent-selection! pane)
      (is (= "line 1\nline 2" (.getText pane))))))

(deftest edit-menu-navigation-test
  (testing "Edit menu contains Jump to Definition, Rename, and Indent/Unindent"
    (let [frame (ui/create-ide nil)
          menubar (.getJMenuBar frame)
          edit-menu (.getMenu menubar 1) ;; 0 = File, 1 = Edit
          item-count (.getItemCount edit-menu)
          items (map (fn [i] (when-let [item (.getItem edit-menu i)] (.getText item)))
                     (range item-count))]
      (is (some #{"Jump to Definition"} items))
      (is (some #{"Rename Symbol..."} items))
      (is (some #{"Indent Selection"} items))
      (is (some #{"Unindent Selection"} items))
      (.dispose frame))))
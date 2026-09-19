(ns DrClojure.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [DrClojure.core :as core]
            [DrClojure.ui :as ui]))

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
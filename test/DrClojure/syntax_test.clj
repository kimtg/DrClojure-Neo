(ns DrClojure.syntax-test
  (:require [clojure.test :refer [deftest is testing]]
             [DrClojure.syntax :as syntax])
  (:import (javax.swing.text DefaultStyledDocument StyleConstants SimpleAttributeSet)
           (javax.swing JTextPane)
           (java.awt Color)))

(deftest tokenize-test
  (testing "Tokenizing comments"
    (let [tokens (syntax/tokenize "; a simple comment\n(def x 1)")]
      (is (= :comment (first (first tokens))))
      (is (= [0 18] [(nth (first tokens) 1) (nth (first tokens) 2)]))))

  (testing "Tokenizing strings and regexes"
    (let [tokens (syntax/tokenize "\"hello \\\"world\\\"\" #\"\\d+\"")]
      (is (= :string (first (first tokens))))
      (is (= :regex (first (second tokens))))))

  (testing "Tokenizing characters"
    (let [tokens (syntax/tokenize "\\newline \\space \\a")]
      (is (= [:char :char :char] (map first tokens)))))

  (testing "Tokenizing keywords"
    (let [tokens (syntax/tokenize ":simple ::auto-resolved :ns/qualified")]
      (is (= [:keyword :keyword :keyword] (map first tokens)))))

  (testing "Tokenizing numbers"
    (let [tokens (syntax/tokenize "42 -10 3.14 1/2 0x1f 100N")]
      (is (= [:number :number :number :number :number :number] (map first tokens)))))

  (testing "Tokenizing special forms, builtins, constants, and symbols"
    (let [tokens (syntax/tokenize "(defn square [n] (* n n))")]
      (is (= [:bracket :special-form :symbol :bracket :symbol :bracket :bracket :builtin :symbol :symbol :bracket :bracket]
             (map first tokens))))
    (let [tokens (syntax/tokenize "(= true false nil)")]
      (is (= [:bracket :builtin :constant :constant :constant :bracket]
             (map first tokens))))))

(deftest highlight-doc-test
  (testing "Document styling applies correct attributes"
    (let [doc (DefaultStyledDocument.)
          code "(defn greet [name]\n  ; greeting\n  (println (str \"Hello, \" name)))\n(greet \"World\")"]
      (.insertString doc 0 code nil)
      (syntax/highlight-doc! doc)

      ;; "defn" is at index 1-5
      (let [elem-defn (.getCharacterElement doc 1)
            attrs (.getAttributes elem-defn)]
        (is (= (Color. 0 0 205) (StyleConstants/getForeground attrs)))
        (is (true? (StyleConstants/isBold attrs))))

      ;; "; greeting" is at index 23
      (let [idx (.indexOf code "; greeting")
            elem-comment (.getCharacterElement doc idx)
            attrs (.getAttributes elem-comment)]
        (is (= (Color. 106 115 125) (StyleConstants/getForeground attrs)))
        (is (true? (StyleConstants/isItalic attrs))))

      ;; "println" is a built-in
      (let [idx (.indexOf code "println")
            elem-builtin (.getCharacterElement doc idx)
            attrs (.getAttributes elem-builtin)]
        (is (= (Color. 0 90 158) (StyleConstants/getForeground attrs))))

      ;; "\"Hello, \"" is a string
      (let [idx (.indexOf code "\"Hello, \"")
            elem-str (.getCharacterElement doc idx)
            attrs (.getAttributes elem-str)]
        (is (= (Color. 34 134 58) (StyleConstants/getForeground attrs)))))))

(deftest setup-syntax-highlighting-test
  (testing "Setup syntax highlighting returns timer and manual highlight fn"
    (let [doc (DefaultStyledDocument.)
          pane (JTextPane. doc)
          res (syntax/setup-syntax-highlighting! pane {:delay-ms 50})]
      (is (instance? javax.swing.Timer (:timer res)))
      (is (fn? (:highlight-now! res)))
      (.insertString doc 0 "(def a 10)" nil)
      ((:highlight-now! res))
      (let [elem (.getCharacterElement doc 1)
            attrs (.getAttributes elem)]
        (is (= (Color. 0 0 205) (StyleConstants/getForeground attrs)))))))

(deftest compute-brackets-test
  (testing "Balanced nested brackets and depths"
    (let [code "(defn f [x] {:a (+ x 1)})"
          binfo (syntax/compute-brackets code)
          matches (:matches binfo)
          tokens (:bracket-tokens binfo)]
      ;; Total brackets: 8 (4 pairs)
      (is (= 8 (count tokens)))
      (is (empty? (:unmatched binfo)))
      ;; Matching indices:
      ;; 0 '(' matches 24 ')'
      (is (= 24 (get matches 0)))
      (is (= 0 (get matches 24)))
      ;; 8 '[' matches 10 ']'
      (is (= 10 (get matches 8)))
      (is (= 8 (get matches 10)))
      ;; 12 '{' matches 23 '}'
      (is (= 23 (get matches 12)))
      (is (= 12 (get matches 23)))
      ;; 16 '(' matches 22 ')'
      (is (= 22 (get matches 16)))
      (is (= 16 (get matches 22)))
      ;; Check depths
      ;; 0 is depth 0
      (is (= [0 1 0 :open] (first tokens)))
      ;; 8 is depth 1
      (is (= [8 9 1 :open] (nth tokens 1)))))

  (testing "Brackets inside comments, strings, regexes, and characters are ignored"
    (let [code "(defn f [x] ;; (ignored in comment) [ ] {\n  (str \"(ignored in string) [ ] {\" #\"[0-9]+\" \\( \\)) x)"
          binfo (syntax/compute-brackets code)
          tokens (:bracket-tokens binfo)]
      ;; Real code brackets:
      ;; 0 '(', 8 '[', 10 ']', 44 '(', 92 ')', 95 ')'
      (is (= 6 (count tokens)))
      (is (empty? (:unmatched binfo)))
      (is (= 95 (get (:matches binfo) 0)))
      (is (= 10 (get (:matches binfo) 8)))
      (is (= 92 (get (:matches binfo) 44)))))

  (testing "Unmatched brackets detection"
    (let [code "(let [x 1]))"
          binfo (syntax/compute-brackets code)]
      ;; Extra ')' at index 11
      (is (contains? (:unmatched binfo) 11))
      (let [extra-tok (last (:bracket-tokens binfo))]
        (is (= :unmatched (nth extra-tok 3)))))

    (let [code "(let [x 1)"
          binfo (syntax/compute-brackets code)]
      ;; Unclosed '[' at index 5 and unclosed '(' at index 0
      (is (contains? (:unmatched binfo) 0))
      (is (contains? (:unmatched binfo) 5))))

  (testing "Empty and nil inputs"
    (is (= {:bracket-tokens [] :matches {} :unmatched #{}}
           (syntax/compute-brackets "")))
    (is (= {:bracket-tokens [] :matches {} :unmatched #{}}
           (syntax/compute-brackets nil)))))

(deftest rainbow-parentheses-test
  (testing "Rainbow color definitions"
    (is (= 7 (count syntax/rainbow-colors)))
    (is (= 7 (count syntax/rainbow-styles)))
    ;; All 7 colors should be unique
    (is (= 7 (count (set syntax/rainbow-colors))))
    (doseq [style syntax/rainbow-styles]
      (is (true? (StyleConstants/isBold style)))))

  (testing "7 levels of rainbow colors applied to nested brackets"
    (let [code "([{[({[()]})]}])"  ;; 8 levels of nesting: depths 0..7
          doc (DefaultStyledDocument.)]
      (.insertString doc 0 code nil)
      (syntax/highlight-doc! doc)
      ;; Test each opening bracket 0..7
      (dotimes [depth 8]
        (let [elem (.getCharacterElement doc depth)
              attrs (.getAttributes elem)
              expected-color (nth syntax/rainbow-colors (mod depth 7))]
          (is (= expected-color (StyleConstants/getForeground attrs))
              (str "Bracket at depth " depth " should match rainbow color mod 7"))
          (is (true? (StyleConstants/isBold attrs)))))
      ;; Test each closing bracket 8..15 (matches corresponding depths)
      (let [binfo (syntax/compute-brackets code)
            matches (:matches binfo)]
        (dotimes [depth 8]
          (let [close-pos (get matches depth)
                elem (.getCharacterElement doc close-pos)
                attrs (.getAttributes elem)
                expected-color (nth syntax/rainbow-colors (mod depth 7))]
            (is (= expected-color (StyleConstants/getForeground attrs))
                (str "Closing bracket matching depth " depth " should match rainbow color mod 7"))
            (is (true? (StyleConstants/isBold attrs))))))))

  (testing "Unmatched bracket is styled with unmatched-bracket-style"
    (let [code "(foo))"
          doc (DefaultStyledDocument.)]
      (.insertString doc 0 code nil)
      (syntax/highlight-doc! doc)
      ;; Character at index 5 is the extra ')'
      (let [elem (.getCharacterElement doc 5)
            attrs (.getAttributes elem)]
        (is (= (Color. 220 0 0) (StyleConstants/getForeground attrs)))
        (is (true? (StyleConstants/isBold attrs)))))))


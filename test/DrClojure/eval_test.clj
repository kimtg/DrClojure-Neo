(ns DrClojure.eval-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [DrClojure.eval :as sut]))

(deftest test-format-error
  (testing "ArithmeticException formatting"
    (try
      (clojure.lang.Compiler/load (java.io.StringReader. "(/ 10 0)"))
      (catch Throwable t
        (let [err (sut/format-error t)]
          (is (.contains err "ArithmeticException"))
          (is (.contains err "Divide by zero"))
          (is (.contains err "Location: line 1"))))))

  (testing "Syntax error formatting"
    (try
      (clojure.lang.Compiler/load (java.io.StringReader. "(def 123)"))
      (catch Throwable t
        (let [err (sut/format-error t)]
          (is (.contains err "Location: line 1")))))))

(deftest test-repl-history
  (let [h0 (sut/create-history)]
    (is (empty? (:items h0)))
    (let [h1 (sut/history-add h0 "(+ 1 2)")
          h2 (sut/history-add h1 "(* 3 4)")]
      (is (= ["(+ 1 2)" "(* 3 4)"] (:items h2)))
      ;; Up arrow: get last command
      (let [[h3 text3] (sut/history-prev h2 "draft")]
        (is (= "(* 3 4)" text3))
        ;; Up arrow again: get previous command
        (let [[h4 text4] (sut/history-prev h3 text3)]
          (is (= "(+ 1 2)" text4))
          ;; Down arrow: get next command
          (let [[h5 text5] (sut/history-next h4)]
            (is (= "(* 3 4)" text5))
            ;; Down arrow again: return to draft
            (let [[_ text6] (sut/history-next h5)]
              (is (= "draft" text6)))))))))

(deftest test-async-eval-success
  (let [ctx (sut/make-eval-context)
        out-acc (atom "")
        status-acc (atom [])
        completed (promise)]
    (sut/eval-async ctx "(println \"Calculating...\") (+ 10 25)"
      {:on-output (fn [s] (swap! out-acc str s))
       :on-status-change (fn [st] (swap! status-acc conj st))
       :on-complete (fn [res] (deliver completed res))})
    (let [res (deref completed 3000 :timeout)]
      (is (not= :timeout res))
      (is (= :ok (:status res)))
      (is (= 35 (:value res)))
      (is (.contains @out-acc "Calculating..."))
      (is (.contains @status-acc :running))
      (is (.contains @status-acc :idle)))))

(deftest test-async-eval-stdin
  (let [ctx (sut/make-eval-context)
        out-acc (atom "")
        waiting-log (atom [])
        completed (promise)]
    (sut/eval-async ctx "(let [line (read-line)] (str \"echo: \" line))"
      {:on-output (fn [s] (swap! out-acc str s))
       :on-wait-input (fn [w?] (swap! waiting-log conj w?))
       :on-complete (fn [res] (deliver completed res))})
    (Thread/sleep 150)
    (is (sut/waiting-for-input? ctx))
    (is (some true? @waiting-log))
    ;; User provides stdin via GUI
    (sut/push-stdin! ctx "Greetings from GUI!")
    (let [res (deref completed 3000 :timeout)]
      (is (= :ok (:status res)))
      (is (= "echo: Greetings from GUI!" (:value res)))
      (is (false? (sut/waiting-for-input? ctx))))))

(deftest test-async-eval-stop
  (let [ctx (sut/make-eval-context)
        completed (promise)]
    (sut/eval-async ctx "(Thread/sleep 10000)"
      {:on-complete (fn [res] (deliver completed res))})
    (Thread/sleep 100)
    (is (sut/evaluating? ctx))
    (is (true? (sut/stop-eval! ctx)))
    (let [res (deref completed 2000 :timeout)]
      (is (not= :timeout res))
      (is (= :interrupted (:status res)))
      (is (false? (sut/evaluating? ctx))))))

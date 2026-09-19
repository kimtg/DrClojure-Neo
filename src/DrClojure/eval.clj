(ns DrClojure.eval
  "Evaluation engine for DrClojure with live stdout/stderr capture,
   GUI-integrated standard input (stdin / read-line), asynchronous execution,
   and beginner-friendly error formatting."
  (:require [clojure.main :as main]
            [clojure.string :as str])
  (:import (java.io Reader StringReader Writer PrintWriter)
           (java.util.concurrent LinkedBlockingQueue)
           (clojure.lang LineNumberingPushbackReader Compiler$CompilerException)))

(defn create-writer
  "Creates a java.io.Writer that forwards character chunks to (on-chunk str)."
  [on-chunk]
  (proxy [Writer] []
    (close [] nil)
    (flush [] nil)
    (write
      ([x]
       (cond
         (instance? (Class/forName "[C") x)
         (on-chunk (String. ^chars x))
         (string? x)
         (on-chunk x)
         (integer? x)
         (on-chunk (str (char x)))
         :else
         (on-chunk (str x))))
      ([x off len]
       (cond
         (instance? (Class/forName "[C") x)
         (on-chunk (String. ^chars x (int off) (int len)))
         (string? x)
         (on-chunk (subs ^String x (int off) (+ (int off) (int len))))
         :else
         (on-chunk (subs (str x) (int off) (+ (int off) (int len)))))))))

(defn create-stdin-reader
  "Creates a Reader connected to a LinkedBlockingQueue.
   When the stream needs data, it notifies on-wait-input and blocks until input arrives."
  [queue current-rdr-atom closed-atom on-wait-input]
  (proxy [Reader] []
    (close []
      (reset! closed-atom true)
      (.offer queue ::eof))
    (read
      ([]
       (let [buf (make-array Character/TYPE 1)
             n (.read ^Reader this buf 0 1)]
         (if (neg? n) -1 (int (aget ^chars buf 0)))))
      ([cbuf off len]
       (if @closed-atom
         -1
         (loop []
           (let [n (.read ^StringReader @current-rdr-atom cbuf off len)]
             (if (neg? n)
               (do
                 (when on-wait-input (on-wait-input true))
                 (let [item (try (.take queue) (catch InterruptedException _ ::eof))]
                   (when on-wait-input (on-wait-input false))
                   (if (or (identical? item ::eof) (nil? item) @closed-atom)
                     (do
                       (reset! closed-atom true)
                       -1)
                     (do
                       (reset! current-rdr-atom (StringReader. (str item)))
                       (recur)))))
               n))))))))

(defn find-root-cause
  "Traverses causes of a Throwable to find the initial cause."
  [^Throwable t]
  (loop [cause t]
    (if-let [next (.getCause cause)]
      (recur next)
      cause)))

(defn format-error
  "Formats a Throwable into a clean, friendly error message for beginners."
  [t]
  (let [root (find-root-cause t)
        data (or (ex-data t) (ex-data root))
        msg (or (.getMessage root) (.getMessage t) (str root))
        type-name (.getName (class root))
        line (or (:clojure.error/line data)
                 (try (.-line ^Compiler$CompilerException t) (catch Throwable _ nil)))
        column (or (:clojure.error/column data)
                   (:column data))]
    (str "Error [" type-name "]: " msg
         (when line
           (str "\nLocation: line " line (when column (str ", col " column)))))))

(defn format-value
  "Formats an evaluation result."
  [v]
  (pr-str v))

;; --- REPL Command History ---

(defn create-history []
  {:items []
   :index 0
   :draft ""})

(defn history-add
  "Adds command to history and resets pointer."
  [hist cmd]
  (let [trimmed (str/trim (or cmd ""))]
    (if (and (not (str/blank? trimmed))
             (not= (last (:items hist)) trimmed))
      (let [new-items (conj (:items hist) trimmed)]
        {:items new-items
         :index (count new-items)
         :draft ""})
      (assoc hist :index (count (:items hist)) :draft ""))))

(defn history-prev
  "Moves backward in command history. Returns [new-hist text]."
  [hist current-text]
  (let [items (:items hist)
        total (count items)]
    (if (zero? total)
      [hist current-text]
      (let [cur-idx (:index hist)
            new-draft (if (= cur-idx total) current-text (:draft hist))
            new-idx (max 0 (dec cur-idx))
            text (nth items new-idx)]
        [(assoc hist :index new-idx :draft new-draft) text]))))

(defn history-next
  "Moves forward in command history. Returns [new-hist text]."
  [hist]
  (let [items (:items hist)
        total (count items)]
    (if (zero? total)
      [hist ""]
      (let [cur-idx (:index hist)]
        (if (>= cur-idx total)
          [hist (:draft hist)]
          (let [new-idx (inc cur-idx)]
            (if (>= new-idx total)
              [(assoc hist :index total) (:draft hist)]
              [(assoc hist :index new-idx) (nth items new-idx)])))))))

;; --- Evaluation Context ---

(defn make-eval-context
  "Creates an evaluation context containing the stdin queue, history atom,
   and evaluation execution state atom."
  []
  (let [queue (LinkedBlockingQueue.)
        current-rdr (atom (StringReader. ""))
        closed? (atom false)
        waiting-input? (atom false)
        history (atom (create-history))
        running-state (atom {:status :idle
                             :future nil
                             :thread nil})]
    {:queue queue
     :current-rdr current-rdr
     :closed? closed?
     :waiting-input? waiting-input?
     :history history
     :running-state running-state}))

(defn push-stdin!
  "Pushes a line of input to the context's standard input queue."
  [{:keys [queue]} text]
  (.offer queue (str text "\n")))

(defn push-stdin-eof!
  "Pushes an EOF signal to the context's standard input queue."
  [{:keys [queue]}]
  (.offer queue ::eof))

(defn waiting-for-input?
  "Returns true if user code is currently blocked waiting for stdin."
  [{:keys [waiting-input?]}]
  @waiting-input?)

(defn evaluating?
  "Returns true if code is currently being evaluated."
  [{:keys [running-state]}]
  (= :running (:status @running-state)))

(defn stop-eval!
  "Interrupts any actively running evaluation and unblocks stdin."
  [{:keys [queue running-state closed? waiting-input?] :as ctx}]
  (let [state @running-state]
    (when (= :running (:status state))
      (when-let [^Thread th (:thread state)]
        (try (.interrupt th) (catch Throwable _ nil)))
      (when-let [fut (:future state)]
        (try (future-cancel fut) (catch Throwable _ nil)))
      (.offer queue ::eof)
      (reset! waiting-input? false)
      (when-let [complete-fn (:on-complete state)]
        (try (complete-fn {:status :interrupted}) (catch Throwable _ nil)))
      (reset! running-state {:status :idle :future nil :thread nil :on-complete nil})
      true)))

(defn eval-async
  "Asynchronously evaluates code in a background thread with dynamically bound
   *out*, *err*, and *in*.
   
   Options:
     :on-output (fn [str])          Called whenever stdout/stderr or eval prints text.
     :on-wait-input (fn [bool])     Called when code enters or leaves blocked stdin.
     :on-status-change (fn [status]) Status: :running, :idle, :waiting-input.
     :on-complete (fn [result])     Called when evaluation finishes.
     :ns-name symbol                Namespace to evaluate in (default 'user)."
  [{:keys [queue current-rdr closed? waiting-input? running-state] :as ctx}
   code
   {:keys [on-output on-wait-input on-status-change on-complete ns-name]
    :or {ns-name 'user}}]
  ;; If already running, do not overlap
  (if (evaluating? ctx)
    (do
      (when on-output (on-output "; [Warning: Evaluation already in progress. Press Stop to cancel.]\n"))
      nil)
    (let [completed-atom (atom false)
          _ (.clear queue)
          _ (reset! current-rdr (StringReader. ""))
          _ (reset! closed? false)
          _ (reset! waiting-input? false)
          safe-complete (fn [res]
                          (when (compare-and-set! completed-atom false true)
                            (when on-complete (on-complete res))))
          out-writer (create-writer (fn [chunk] (when on-output (on-output chunk))))
          print-writer (PrintWriter. out-writer true)
          in-callback (fn [waiting?]
                        (reset! waiting-input? waiting?)
                        (when on-wait-input (on-wait-input waiting?))
                        (when on-status-change
                          (on-status-change (if waiting? :waiting-input :running))))
          gui-in-reader (create-stdin-reader queue current-rdr closed? in-callback)
          pushback-reader (LineNumberingPushbackReader. gui-in-reader)
          fut (future
                (swap! running-state assoc :thread (Thread/currentThread))
                (when on-status-change (on-status-change :running))
                (let [target-ns (or (find-ns ns-name) (create-ns ns-name))
                      result (try
                               (binding [*out* print-writer
                                         *err* print-writer
                                         *in* pushback-reader
                                         *ns* target-ns]
                                 (in-ns ns-name)
                                 (clojure.core/refer-clojure)
                                 (try
                                   (clojure.core/require '[clojure.repl :refer [doc source apropos dir pst]])
                                   (clojure.core/require '[clojure.pprint :refer [pprint]])
                                   (catch Throwable _ nil))
                                 (let [rdr (LineNumberingPushbackReader. (StringReader. (str code "\n")))
                                       val (clojure.lang.Compiler/load rdr)]
                                   {:status :ok :value val}))
                               (catch InterruptedException _
                                 {:status :interrupted})
                               (catch Throwable t
                                 (if (or (instance? InterruptedException (find-root-cause t))
                                         (.isInterrupted (Thread/currentThread)))
                                   {:status :interrupted}
                                   {:status :error :error t})))]
                  (reset! waiting-input? false)
                  (reset! running-state {:status :idle :future nil :thread nil :on-complete nil})
                  (when on-status-change (on-status-change :idle))
                  (safe-complete result)
                  result))]
      (reset! running-state {:status :running
                             :future fut
                             :thread nil
                             :on-complete safe-complete})
      fut)))

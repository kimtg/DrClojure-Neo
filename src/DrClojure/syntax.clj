(ns DrClojure.syntax
  "Syntax highlighting for Clojure code in DrClojure.
   Provides lexing/tokenization and Swing DefaultStyledDocument styling
   using pure Clojure and standard Java Swing."
  (:require [clojure.string :as str])
  (:import (javax.swing Timer)
           (javax.swing.text DefaultStyledDocument SimpleAttributeSet StyleConstants JTextComponent)
           (javax.swing.event DocumentListener)
           (java.awt Color)
           (java.awt.event ActionListener)
           (java.util.regex Pattern Matcher)
           (java.util.concurrent Executors ThreadFactory ExecutorService)))

;; --- Token Definitions ---

(def special-forms
  #{"def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "definterface" "defonce"
    "let" "loop" "recur" "binding" "with-open" "with-local-vars" "with-redefs"
    "with-out-str" "with-in-str"
    "if" "if-not" "if-let" "when" "when-not" "when-let" "when-first"
    "when-some" "if-some" "cond" "condp" "case" "do"
    "fn" "fn*"
    "doseq" "dotimes" "for" "while"
    "try" "catch" "finally" "throw"
    "ns" "in-ns" "require" "use" "import" "refer"
    "quote" "var" "set!"})

(def core-builtins
  #{"println" "print" "prn" "pr" "printf" "read-line" "read-string" "slurp" "spit"
    "map" "filter" "reduce" "remove" "take" "drop" "take-while" "drop-while"
    "concat" "into" "assoc" "dissoc" "get" "get-in" "assoc-in" "update" "update-in"
    "conj" "disj" "contains?" "count" "empty?" "seq" "first" "rest" "next" "last"
    "nth" "peek" "pop" "cons" "vec" "set" "hash-map" "vector" "list"
    "keys" "vals" "select-keys" "zipmap" "merge" "merge-with" "sort" "sort-by"
    "group-by" "partition" "partition-by" "flatten" "distinct" "reverse"
    "interleave" "interpose" "keep"
    "nil?" "some?" "true?" "false?" "string?" "number?" "integer?" "float?"
    "boolean?" "keyword?" "symbol?" "fn?" "vector?" "map?" "set?" "seq?" "list?"
    "coll?" "pos?" "neg?" "zero?" "even?" "odd?"
    "+" "-" "*" "/" "inc" "dec" "max" "min" "quot" "rem" "mod" "abs"
    "=" "==" "not=" "<" "<=" ">" ">=" "not" "and" "or"
    "atom" "deref" "swap!" "reset!" "swap-vals!" "reset-vals!"
    "future" "promise" "deliver" "realized?" "delay" "force"
    "apply" "partial" "comp" "constantly" "identity" "juxt" "memoize" "complement"
    "doc" "source" "apropos" "dir" "find-doc" "format" "str"})

(def constants
  #{"true" "false" "nil"})

(def token-pattern
  (Pattern/compile
    (str
      "(;[^\n]*)"                                                  ;; Group 1: comment
      "|(#[?@]?\"(?>\\\\.|[^\"\\\\])*\"?)"                         ;; Group 2: regex literal
      "|(\"(?>\\\\.|[^\"\\\\])*\"?)"                               ;; Group 3: string literal
      "|(\\\\(?:newline|space|tab|backspace|formfeed|return|u[0-9a-fA-F]{4}|o[0-3]?[0-7]{1,2}|.))" ;; Group 4: character
      "|(::?[-_a-zA-Z0-9\\p{L}\\p{N}.!$%&*+/<=>?#]+)"             ;; Group 5: keyword
      "|([+-]?(?:0x[0-9a-fA-F]+|\\d+(?:/\\d+|\\.\\d+)?(?:[eE][+-]?\\d+)?[MN]?)(?=[\\s()\\[\\]{}\",]|$))" ;; Group 6: number
      "|([()\\[\\]{}])"                                            ;; Group 7: bracket / delimiter
      "|(\\^[-_a-zA-Z0-9\\p{L}\\p{N}.!$%&*+/<=>?#:]+)"            ;; Group 8: metadata tag
      "|([-_a-zA-Z\\p{L}.!$%&*+/<=>?][-_a-zA-Z0-9\\p{L}\\p{N}.!$%&*+/<=>?#]*)"))) ;; Group 9: symbol

(defn tokenize
  "Scans `text` and returns a vector of tuples `[token-type start end]`.
   Token types include:
   `:comment`, `:regex`, `:string`, `:char`, `:keyword`, `:number`,
   `:bracket`, `:metatag`, `:special-form`, `:builtin`, `:constant`, `:symbol`."
  [^String text]
  (if (or (nil? text) (zero? (.length text)))
    []
    (let [matcher (.matcher token-pattern text)
          results (transient [])]
      (while (.find matcher)
        (let [start (.start matcher)
              end (.end matcher)
              tok-type (cond
                         (.group matcher 1) :comment
                         (.group matcher 3) :string
                         (.group matcher 2) :regex
                         (.group matcher 4) :char
                         (.group matcher 5) :keyword
                         (.group matcher 6) :number
                         (.group matcher 7) :bracket
                         (.group matcher 8) :metatag
                         (.group matcher 9)
                         (let [sym (.group matcher 9)]
                           (cond
                             (constants sym) :constant
                             (special-forms sym) :special-form
                             (core-builtins sym) :builtin
                             :else :symbol))
                         :else :symbol)]
          (conj! results [tok-type start end])))
      (persistent! results))))

;; --- Style Definitions ---

(defn make-style
  "Constructs a SimpleAttributeSet with the specified foreground color,
   optional bold and italic flags."
  [^Color color & {:keys [bold italic]}]
  (let [attr (SimpleAttributeSet.)]
    (StyleConstants/setForeground attr color)
    (when bold (StyleConstants/setBold attr true))
    (when italic (StyleConstants/setItalic attr true))
    attr))

(def default-palette
  {:default      (make-style (Color. 36 41 46))              ;; Dark Charcoal
   :comment      (make-style (Color. 106 115 125) :italic true) ;; Muted Slate Gray
   :string       (make-style (Color. 34 134 58))             ;; Forest Green
   :regex        (make-style (Color. 179 29 40))             ;; Dark Maroon
   :char         (make-style (Color. 34 134 58))             ;; Forest Green
   :keyword      (make-style (Color. 111 66 193) :bold true) ;; Royal Purple
   :number       (make-style (Color. 0 92 197))              ;; Teal Blue
   :special-form (make-style (Color. 0 0 205) :bold true)    ;; Dark Blue
   :builtin      (make-style (Color. 0 90 158))              ;; Slate Blue
   :constant     (make-style (Color. 0 0 205) :bold true)    ;; Dark Blue
   :bracket      (make-style (Color. 68 77 86))              ;; Delimiter Gray
   :metatag      (make-style (Color. 88 96 105) :italic true) ;; Slate
   :symbol       (make-style (Color. 36 41 46))})            ;; Dark Charcoal

;; --- Rainbow Parentheses (7 Levels) & Bracket Stack ---

(def open->close {\( \), \[ \], \{ \}})
(def close->open {\) \(, \] \[, \} \{})

(def rainbow-colors
  [(Color. 192 104 0)    ;; Level 1: Amber / Warm Gold
   (Color. 0 122 204)    ;; Level 2: Royal Blue
   (Color. 123 31 162)   ;; Level 3: Violet / Purple
   (Color. 46 125 50)    ;; Level 4: Forest Green
   (Color. 198 40 40)    ;; Level 5: Crimson Red
   (Color. 0 131 143)    ;; Level 6: Teal
   (Color. 173 20 87)])  ;; Level 7: Magenta / Rose

(def rainbow-styles
  (mapv (fn [c] (make-style c :bold true)) rainbow-colors))

(def unmatched-bracket-style
  (make-style (Color. 220 0 0) :bold true))

(defn compute-brackets
  "Analyzes brackets in `text`, skipping comments, strings, regexes, and characters.
   Returns a map with:
   - `:bracket-tokens`: vector of `[start end depth type]` (:open, :close, :unmatched)
   - `:matches`: map of `pos -> match-pos`
   - `:unmatched`: set of unmatched bracket positions"
  [^String text]
  (if (or (nil? text) (empty? text))
    {:bracket-tokens [] :matches {} :unmatched #{}}
    (let [tokens (tokenize text)
          stack (java.util.ArrayDeque.)
          bracket-tokens (java.util.ArrayList.)
          matches (java.util.HashMap.)
          unmatched (java.util.HashSet.)]
      (doseq [[tok-type start end] tokens]
        (when (= tok-type :bracket)
          (let [ch (.charAt text start)]
            (cond
              (open->close ch)
              (let [depth (.size stack)]
                (.push stack [ch start depth])
                (.add bracket-tokens [(long start) (long end) depth :open]))

              (close->open ch)
              (let [expected-open (close->open ch)]
                (if (and (not (.isEmpty stack))
                         (= (first (.peek stack)) expected-open))
                  (let [[_ open-pos open-depth] (.pop stack)]
                    (.add bracket-tokens [(long start) (long end) open-depth :close])
                    (.put matches (long open-pos) (long start))
                    (.put matches (long start) (long open-pos)))
                  (do
                    (.add unmatched (long start))
                    (.add bracket-tokens [(long start) (long end) 0 :unmatched]))))))))
      (while (not (.isEmpty stack))
        (let [[_ open-pos _] (.pop stack)]
          (.add unmatched (long open-pos))))
      {:bracket-tokens (vec bracket-tokens)
       :matches (into {} matches)
       :unmatched (into #{} unmatched)})))

(defn highlight-doc!
  "Applies syntax highlighting styles across `doc`, including 7-level rainbow parentheses.
   Optionally accepts a custom `palette` map.
   Returns the computed `bracket-info` map."
  ([^DefaultStyledDocument doc]
   (highlight-doc! doc default-palette))
  ([^DefaultStyledDocument doc palette]
   (let [len (.getLength doc)]
     (if (zero? len)
       {:bracket-tokens [] :matches {} :unmatched #{}}
       (let [text (.getText doc 0 len)
             default-attr (:default palette)
             matcher (.matcher token-pattern text)
             binfo (compute-brackets text)
             matches (:matches binfo)
             unmatched (:unmatched binfo)
             rainbow-count (count rainbow-styles)]
         ;; 1. Reset document to default text style
         (.setCharacterAttributes doc 0 len default-attr true)
         ;; 2. Apply token-specific styles
         (while (.find matcher)
           (let [start (.start matcher)
                 end (.end matcher)
                 tok-len (- end start)
                 tok-type (cond
                            (.group matcher 1) :comment
                            (.group matcher 3) :string
                            (.group matcher 2) :regex
                            (.group matcher 4) :char
                            (.group matcher 5) :keyword
                            (.group matcher 6) :number
                            (.group matcher 8) :metatag
                            (.group matcher 9)
                            (let [sym (.group matcher 9)]
                              (cond
                                (constants sym) :constant
                                (special-forms sym) :special-form
                                (core-builtins sym) :builtin
                                :else nil))
                            :else nil)]
             (when-let [attr (and tok-type (get palette tok-type))]
               (.setCharacterAttributes doc start tok-len attr true))))
         ;; 3. Apply 7-level rainbow parentheses styles
         (doseq [[start end depth type] (:bracket-tokens binfo)]
           (let [attr (if (= type :unmatched)
                        unmatched-bracket-style
                        (nth rainbow-styles (mod depth rainbow-count)))]
             (.setCharacterAttributes doc (int start) (int (- end start)) attr true)))
         binfo)))))

(declare warm-buffer-cache-async!)

(defn setup-syntax-highlighting!
  "Attaches a live syntax highlighting listener to `pane`.
   Uses a debounced Swing Timer so that rapid typing remains smooth and responsive.
   Also triggers background caching of document symbols for instant autocomplete.
   Returns a map with:
   - `:timer` (javax.swing.Timer)
   - `:bracket-info` (atom holding latest bracket info)
   - `:highlight-now!` (fn [] ... to force immediate re-highlighting)."
  [^JTextComponent pane & [{:keys [delay-ms palette] :or {delay-ms 60 palette default-palette}}]]
  (let [doc ^DefaultStyledDocument (.getDocument pane)
        binfo-atom (atom {:bracket-tokens [] :matches {} :unmatched #{}})
        highlight-fn (fn []
                       (let [info (highlight-doc! doc palette)]
                         (reset! binfo-atom info)
                         (try
                           (let [txt (.getText doc 0 (.getLength doc))]
                             (warm-buffer-cache-async! txt))
                           (catch Exception _ nil))))
        timer (Timer. (int delay-ms)
                (proxy [ActionListener] []
                  (actionPerformed [e]
                    (highlight-fn))))]
    (.setRepeats timer false)
    (.addDocumentListener doc
      (proxy [DocumentListener] []
        (insertUpdate [e]
          (.restart timer))
        (removeUpdate [e]
          (.restart timer))
        (changedUpdate [e]
          ;; Attribute changes (from setCharacterAttributes) are ignored
          ;; to avoid infinite loops and unnecessary re-parsing.
          nil)))
    {:timer timer
     :bracket-info binfo-atom
     :highlight-now! highlight-fn}))

;; --- Symbol Analysis & Navigation ---

(def def-forms
  #{"def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defonce"
    "defprotocol" "defrecord" "deftype" "deftest" "defstruct" "definterface"})

(def fn-def-forms
  #{"defn" "defn-" "defmacro" "defmulti" "defmethod" "deftest" "fn" "fn*"})

(defn line-number-at-offset
  "Calculates 1-based line number for character offset in `text`."
  [^String text offset]
  (if (or (nil? text) (not (pos? offset)))
    1
    (inc (count (re-seq #"\n" (.substring text 0 (min (.length text) (int offset))))))))

(defn symbol-at-pos
  "Returns a map `{:symbol str, :start int, :end int, :type keyword}` for the symbol
   covering or immediately around `pos` in `text`.
   Returns nil if caret is not on or adjacent to a code symbol (e.g. inside comments, strings, or blank space)."
  [^String text pos]
  (when (and (string? text) (pos? (.length text)))
    (let [len (.length text)
          pos (min len (max 0 (int (or pos 0))))
          tokens (tokenize text)
          in-literal? (some (fn [[tok-type s e]]
                              (when (contains? #{:comment :string :regex :char} tok-type)
                                (if (= tok-type :comment)
                                  (<= s pos e)
                                  (and (<= s pos) (<= pos e)))))
                            tokens)]
      (when-not in-literal?
        (let [sym-tokens (filter #(contains? #{:symbol :special-form :builtin :constant} (first %)) tokens)]
          (when (seq sym-tokens)
            ;; 1. Direct match: pos is inside or at boundary of a symbol token
            (if-let [exact (or (first (filter (fn [[_ s e]] (and (<= s pos) (< pos e))) sym-tokens))
                               (first (filter (fn [[_ s e]] (<= s pos e)) sym-tokens)))]
              (let [[tok-type s e] exact]
                {:symbol (.substring text s e) :start s :end e :type tok-type})

              ;; 2. Proximity match: pos is on a delimiter/prefix/space immediately adjacent to a symbol
              (let [ch (when (< pos len) (.charAt text pos))
                    prev-ch (when (pos? pos) (.charAt text (dec pos)))
                    candidates
                    (keep (fn [[tok-type s e]]
                            (let [dist (cond
                                         (< pos s) (- s pos)
                                         (> pos e) (- pos e)
                                         :else 0)]
                              (when (<= dist 2)
                                (let [intervening (if (< pos s)
                                                    (.substring text pos s)
                                                    (.substring text e pos))]
                                  (when (and (not (re-find #"[\r\n]" intervening))
                                             (re-matches #"^[()\[\]{}@'~`^#, ]*$" intervening)
                                             (or (<= dist 1)
                                                 (re-find #"[()\[\]{}@'~`^#,]" intervening)))
                                    (let [score (cond
                                                  ;; Opening delimiters and reader prefixes bind forward to next symbol
                                                  (and (< pos s) (#{\( \[ \{ \@ \' \~ \^ \` \#} ch))
                                                  (- dist 0.5)

                                                  ;; Closing delimiters bind backward to preceding symbol
                                                  (and (> pos e) (or (#{\) \] \}} ch)
                                                                     (#{\) \] \}} prev-ch)))
                                                  (- dist 0.5)

                                                  :else (double dist))]
                                      {:tok [tok-type s e] :score score :dist dist}))))))
                          sym-tokens)]
                (when-let [best (first (sort-by :score candidates))]
                  (let [[tok-type s e] (:tok best)]
                    {:symbol (.substring text s e) :start s :end e :type tok-type}))))))))))

(defn find-symbol-occurrences
  "Returns a vector of `[start end]` positions for all occurrences of `sym-name`
   as a code symbol in `text` (strictly excluding comments, strings, regexes, keywords, etc.)."
  [^String text sym-name]
  (when (and (string? text) (not (str/blank? sym-name)))
    (let [tokens (tokenize text)]
      (vec
        (keep (fn [[tok-type s e]]
                (when (and (contains? #{:symbol :special-form :builtin :constant} tok-type)
                           (= (.substring text s e) sym-name))
                  [s e]))
              tokens)))))

(defn find-definition
  "Finds the definition position of `sym-name` in `text`.
   Checks top-level `def*` forms first, then local bindings (parameters / let / loop)
   prior to `pos` if provided.
   Returns `{:symbol str, :start int, :end int, :line int, :kind keyword}` or nil."
  [^String text sym-name & [pos]]
  (when (and (string? text) (not (str/blank? sym-name)))
    (let [tokens (vec (filter #(not= (first %) :comment) (tokenize text)))
          n (count tokens)]
      ;; 1. Search top-level def* forms
      (or
        (loop [i 0]
          (if (< i n)
            (let [[tok-type s _] (nth tokens i)]
              (if (and (= tok-type :bracket) (= (.charAt text s) \())
                (if (< (inc i) n)
                  (let [[next-type next-s next-e] (nth tokens (inc i))
                        sym-form (.substring text next-s next-e)]
                    (if (def-forms sym-form)
                      ;; Scan forward past metadata to find defined symbol
                      (let [def-target (loop [j (+ i 2)]
                                         (when (< j n)
                                           (let [[t-type ts te] (nth tokens j)]
                                             (cond
                                               (= t-type :metatag) (recur (inc j))
                                               (= t-type :symbol) (when (= (.substring text ts te) sym-name)
                                                                    {:symbol sym-name
                                                                     :start ts
                                                                     :end te
                                                                     :line (line-number-at-offset text ts)
                                                                     :kind :def})
                                               :else nil))))]
                        (if def-target
                          def-target
                          (recur (inc i))))
                      (recur (inc i))))
                  (recur (inc i)))
                (recur (inc i))))
            nil))
        ;; 2. Search local bindings if pos is provided
        (when (number? pos)
          (let [prior-tokens (filter (fn [[_ s _]] (< s pos)) tokens)]
            (when-let [target (last (filter (fn [[tok-type s e]]
                                              (and (= tok-type :symbol)
                                                   (= (.substring text s e) sym-name)))
                                            prior-tokens))]
              (let [[_ s e] target]
                 {:symbol sym-name
                  :start s
                  :end e
                  :line (line-number-at-offset text s)
                  :kind :local}))))))))

;; --- Smart Indentation, Search & Quick Documentation ---

(defn compute-smart-indent
  "Calculates the appropriate indentation spaces string for a new line created at `pos` in `text`.
   - Auto-indents (+2 spaces) when the line opened unclosed forms.
   - Auto-dedents (-2 spaces per net closed bracket) when the line closed forms.
   - Auto-dedents (-2 spaces per closing bracket) when inserting a newline immediately before closing brackets.
   - Preserves indentation when brackets on the line are balanced."
  [^String text pos]
  (if (or (nil? text) (not (pos? (.length text))) (not (pos? pos)))
    ""
    (let [pos (min (.length text) (max 0 (int pos)))
          line-start (let [idx (.lastIndexOf text "\n" (dec pos))]
                       (if (neg? idx) 0 (inc idx)))
          line-end (let [idx (.indexOf text "\n" pos)]
                     (if (neg? idx) (.length text) idx))
          line-prefix (.substring text line-start pos)
          line-suffix (.substring text pos line-end)
          base-indent (or (re-find #"^[ ]+" line-prefix) "")
          base-len (.length ^String base-indent)
          tokens (tokenize line-prefix)
          open-brackets (count (filter (fn [[tok-type s _]]
                                         (and (= tok-type :bracket)
                                              (open->close (.charAt line-prefix s))))
                                       tokens))
          close-brackets (count (filter (fn [[tok-type s _]]
                                          (and (= tok-type :bracket)
                                               (close->open (.charAt line-prefix s))))
                                        tokens))
          unclosed (- open-brackets close-brackets)
          suffix-tokens (tokenize line-suffix)
          leading-suffix-close (count (take-while (fn [[tok-type s _]]
                                                    (and (= tok-type :bracket)
                                                         (close->open (.charAt line-suffix s))))
                                                  suffix-tokens))]
      (cond
        ;; 1. Line-prefix closed more brackets than it opened: Auto-dedent
        (neg? unclosed)
        (let [dedent-spaces (* 2 (- unclosed))
              target (max 0 (- base-len dedent-spaces))]
          (apply str (repeat target " ")))

        ;; 2. Inserting immediately before closing bracket(s) and prefix didn't open forms: Auto-dedent
        (and (pos? leading-suffix-close) (<= unclosed 0))
        (let [dedent-spaces (* 2 leading-suffix-close)
              target (max 0 (- base-len dedent-spaces))]
          (apply str (repeat target " ")))

        ;; 3. Line-prefix opened unclosed brackets: Auto-indent (+2 spaces)
        (pos? unclosed)
        (str base-indent "  ")

        ;; 4. Balanced brackets on current line: Preserve existing indentation
        :else
        base-indent))))

(defn find-text-matches
  "Finds all occurrences of `query` in `text`.
   Returns vector of `[start end]` character offset pairs.
   Options:
     `:case-sensitive?` (boolean, default false)"
  [^String text ^String query & [{:keys [case-sensitive?]}]]
  (if (or (str/blank? text) (str/blank? query))
    []
    (let [flags (if case-sensitive? Pattern/LITERAL (bit-or Pattern/LITERAL Pattern/CASE_INSENSITIVE))
          pat (Pattern/compile query flags)
          matcher (.matcher pat text)
          results (transient [])]
      (while (.find matcher)
        (conj! results [(.start matcher) (.end matcher)]))
      (persistent! results))))

(defn extract-buffer-def-meta
  "Inspects `text` around `def-target` (from `find-definition`) to extract
   docstring and parameter list (arglists) if available.
   Returns a map with `:arglists` and `:doc`."
  [^String text def-target]
  (when (and (string? text) def-target (= (:kind def-target) :def))
    (try
      (let [target-start (:start def-target)
            tokens (vec (filter #(not= (first %) :comment) (tokenize text)))
            n (count tokens)
            bracket-info (compute-brackets text)
            matches (:matches bracket-info)
            sym-idx (loop [k 0]
                      (when (< k n)
                        (let [[_ s _] (nth tokens k)]
                          (if (= s target-start)
                            k
                            (recur (inc k))))))]
        (when sym-idx
          ;; Look backward from sym-idx to identify the defining form (e.g. defn, def, etc.)
          (let [defn-form-info (loop [k (dec sym-idx)]
                                 (when (>= k 0)
                                   (let [[tok s e] (nth tokens k)]
                                     (cond
                                       (= tok :metatag) (recur (dec k))
                                       (def-forms (.substring text s e))
                                       (when (> k 0)
                                         (let [[prev-tok ps _] (nth tokens (dec k))]
                                           (when (and (= prev-tok :bracket) (= (.charAt text ps) \())
                                             {:form (.substring text s e)
                                              :open-paren ps})))
                                       :else nil))))
                form-name (:form defn-form-info)
                form-end (when-let [op (:open-paren defn-form-info)] (get matches op))
                after-sym-tokens (take-while (fn [[_ s _]]
                                               (if form-end (< s form-end) true))
                                             (subvec tokens (inc sym-idx)))
                ;; Drop leading metadata tags if any after symbol
                meaningful-tokens (drop-while (fn [[t _ _]] (= t :metatag)) after-sym-tokens)]
            (when (seq meaningful-tokens)
              (let [[first-tok fs fe] (first meaningful-tokens)
                    [doc-str remaining-tokens] (if (= first-tok :string)
                                                 (let [raw (.substring text fs fe)
                                                       cleaned (if (and (.startsWith raw "\"")
                                                                        (.endsWith raw "\"")
                                                                        (>= (.length raw) 2))
                                                                 (subs raw 1 (dec (.length raw)))
                                                                 raw)]
                                                   [cleaned (rest meaningful-tokens)])
                                                 [nil meaningful-tokens])
                    rem-after-meta (drop-while (fn [[t _ _]] (= t :metatag)) remaining-tokens)]
                ;; Parameter vector extraction only for function/macro defining forms
                (if (and form-name (fn-def-forms form-name))
                  (if-let [[next-tok ns ne] (first rem-after-meta)]
                    (cond
                      ;; Single arity: [x y ...]
                      (and (= next-tok :bracket) (= (.charAt text ns) \[))
                      (let [close-pos (get matches ns)
                            args-vec (if close-pos
                                       (.substring text ns (inc close-pos))
                                       (.substring text ns ne))]
                        {:doc doc-str
                         :arglists (str "(" args-vec ")")})

                      ;; Multi arity: ([x] ...) ([x y] ...)
                      (and (= next-tok :bracket) (= (.charAt text ns) \())
                      (let [arities (loop [toks rem-after-meta
                                           acc []]
                                      (if (empty? toks)
                                        acc
                                        (let [[t s _] (first toks)]
                                          (if (and (= t :bracket) (= (.charAt text s) \())
                                            (let [branch-close (get matches s)
                                                  branch-toks (if branch-close
                                                                (take-while (fn [[_ bs _]] (< bs branch-close)) (rest toks))
                                                                (rest toks))
                                                  first-vec (first (filter (fn [[bt bs _]]
                                                                             (and (= bt :bracket) (= (.charAt text bs) \[)))
                                                                           branch-toks))]
                                              (if first-vec
                                                (let [[_ vs _] first-vec
                                                      vc (get matches vs)
                                                      v-str (if vc (.substring text vs (inc vc)) "[]")]
                                                  (recur (drop-while (fn [[_ bs _]] (if branch-close (<= bs branch-close) false)) toks)
                                                         (conj acc v-str)))
                                                (recur (rest toks) acc)))
                                            (recur (rest toks) acc)))))]
                        {:doc doc-str
                         :arglists (when (seq arities) (str "(" (str/join " " arities) ")"))})

                      :else
                      {:doc doc-str :arglists nil})
                    {:doc doc-str :arglists nil})
                  ;; Non-function def forms (e.g. def, defonce)
                  {:doc doc-str :arglists nil}))))))
      (catch Exception _ nil))))

(defn get-symbol-doc
  "Retrieves documentation and arglists for `sym-name`.
   Checks runtime environment first (resolving against clojure.core or loaded namespaces),
   then Clojure special forms (via clojure.repl/special-doc),
   then definition within `text` if provided.
   Returns a map with `:status` (:found, :buffer-def, :not-found) and metadata."
  [^String sym-name & [^String text pos]]
  (when-not (str/blank? sym-name)
    (let [sym (symbol sym-name)
          v (try (resolve sym) (catch Exception _ nil))]
      (cond
        ;; 1. Runtime Var resolved (built-ins, core functions, macros)
        v
        (let [m (meta v)
              ns-str (str (or (:ns m) "clojure.core"))
              name-str (str (:name m))
              arglists (when-let [args (:arglists m)] (str args))
              doc-str (:doc m)
              file-str (or (:file m) "unknown")
              line-num (:line m)
              macro? (boolean (:macro m))]
          {:status :found
           :symbol sym-name
           :ns ns-str
           :name name-str
           :arglists arglists
           :doc doc-str
           :macro? macro?
           :file file-str
           :line line-num})

        ;; 2. Clojure special forms (def, if, do, recur, try, quote, var, set!, etc.)
        (or (special-forms sym-name) (special-forms (str sym)))
        (let [spec-info (try
                          (require 'clojure.repl)
                          (when-let [spec-fn (ns-resolve 'clojure.repl 'special-doc)]
                            (@spec-fn sym))
                          (catch Exception _ nil))]
          (if spec-info
            (let [forms (:forms spec-info)
                  arglists (cond
                             (seq forms) (str/join " " (map str forms))
                             (:arglists spec-info) (str (:arglists spec-info))
                             :else nil)]
              {:status :found
               :symbol sym-name
               :name (str (or (:name spec-info) sym-name))
               :special-form true
               :arglists arglists
               :doc (:doc spec-info)})
            {:status :found
             :symbol sym-name
             :name sym-name
             :special-form true}))

        ;; 3. Buffer definition
        :else
        (if-let [def-target (and text (find-definition text sym-name pos))]
          (let [meta-info (extract-buffer-def-meta text def-target)]
            {:status :buffer-def
             :symbol sym-name
             :name sym-name
             :line (:line def-target)
             :kind (:kind def-target)
             :arglists (:arglists meta-info)
             :doc (:doc meta-info)})
          {:status :not-found
           :symbol sym-name})))))

(defn format-autocomplete-doc
  "Formats documentation metadata returned by `get-symbol-doc` into a clean,
   human-readable string for display in the autocomplete documentation preview."
  [doc-info & [{:keys [category symbol]}]]
  (if (or (nil? doc-info) (= (:status doc-info) :not-found))
    (let [sym (or symbol (:symbol doc-info) "")]
      (str sym "\n\n(No documentation found)"))
    (let [{:keys [status ns name arglists doc line kind special-form macro?]} doc-info
          sym-name (or name symbol (:symbol doc-info) "")
          header (cond
                   special-form (str sym-name "  [special form]")
                   (= category :special) (str sym-name "  [special form]")
                   macro? (str (if ns (str ns "/" sym-name) sym-name) "  [macro]")
                   (= status :buffer-def) (str sym-name "  [buffer def" (if line (str ", line " line) "") "]")
                   ns (str ns "/" sym-name)
                   :else sym-name)
          parts (transient [header])]
      (when (and arglists (not (str/blank? arglists)))
        (conj! parts (str "\n" arglists)))
      (conj! parts "\n----------------------------------------\n")
      (if (and doc (not (str/blank? doc)))
        (conj! parts (str/trim doc))
        (conj! parts "(No documentation string)"))
      (apply str (persistent! parts)))))

;; --- Background Worker & Autocomplete Candidate Caching ---

(defonce ^ExecutorService cache-executor
  (Executors/newSingleThreadExecutor
    (reify ThreadFactory
      (newThread [_ r]
        (doto (Thread. ^Runnable r "DrClojure-Autocomplete-Cache-Worker")
          (.setDaemon true)
          (.setPriority Thread/MIN_PRIORITY))))))

(defn- prune-cache [m max-size]
  (if (<= (count m) max-size)
    m
    (let [sorted-entries (sort-by (fn [[_ v]] (or (:timestamp v) 0)) m)
          to-remove (take (- (count m) max-size) sorted-entries)]
      (apply dissoc m (map first to-remove)))))

(defonce buffer-symbols-cache (atom {}))
(defonce docstring-cache (atom {}))

(defn get-cached-symbol-doc
  "Retrieves doc information map for `sym-name`, with bounded caching.
   Caches runtime/special-form doc lookups and buffer lookups."
  ([sym-name]
   (get-cached-symbol-doc sym-name nil nil))
  ([sym-name ^String text]
   (get-cached-symbol-doc sym-name text nil))
  ([sym-name ^String text pos]
   (let [k (if (and text (not (str/blank? text)))
             [(str sym-name) (hash text) pos]
             (str sym-name))]
     (if-let [hit (get @docstring-cache k)]
       hit
       (let [res (get-symbol-doc sym-name text pos)]
         (swap! docstring-cache (fn [m] (prune-cache (assoc m k res) 512)))
         res)))))

(def all-core-builtins
  "Cached set of all clojure.core public var names combined with syntax core-builtins."
  (delay
    (let [publics (try
                    (set (map name (keys (ns-publics 'clojure.core))))
                    (catch Exception _ #{}))]
      (into core-builtins publics))))

(def static-candidates
  "Precomputes all static candidate maps for special forms and clojure.core builtins,
   deduplicated with priority :special > :builtin, and bucketed by lowercase initial character."
  (delay
    (let [specials (map (fn [s] {:symbol s :category :special}) special-forms)
          builtins (map (fn [s] {:symbol s :category :builtin}) @all-core-builtins)
          deduped (reduce (fn [m c]
                            (let [sym (:symbol c)]
                              (if (contains? m sym) m (assoc m sym c))))
                          {}
                          (concat specials builtins))
          all-vec (vec (sort-by (fn [{:keys [symbol category]}]
                                  [(case category :special 0 :builtin 1 2)
                                   (str/lower-case symbol)
                                   symbol])
                                (vals deduped)))
          by-char (group-by (fn [{:keys [symbol]}]
                              (if (pos? (count symbol))
                                (Character/toLowerCase ^Character (.charAt ^String symbol 0))
                                nil))
                            all-vec)]
      {:all all-vec
       :by-char by-char})))

;; Pre-warm static candidates in background worker upon loading
(try
  (.submit ^ExecutorService cache-executor ^Runnable (fn [] @static-candidates))
  (catch Exception _ nil))

(defn- find-span-at-pos
  "Binary search for span [sym start end] containing `pos` (start <= pos <= end).
   Returns the symbol string, or nil if none."
  [^clojure.lang.PersistentVector spans ^long pos]
  (let [n (count spans)]
    (loop [low 0
           high (dec n)]
      (when (<= low high)
        (let [mid (quot (+ low high) 2)
              [sym s e] (nth spans mid)]
          (cond
            (< pos (long s)) (recur low (dec mid))
            (> pos (long e)) (recur (inc mid) high)
            :else sym))))))

(defn extract-text-symbols
  "Parses `text` in a single pass over non-comment tokens to extract:
   - `:defs`: set of symbols defined via top-level `def-forms`
   - `:spans`: vector of [sym-str start end] for all symbol tokens
   - `:all-tokens`: set of all symbol strings across the text
   - `:token-counts`: map of symbol to frequency in text"
  [^String text]
  (if (or (nil? text) (str/blank? text))
    {:defs #{} :spans [] :all-tokens #{} :token-counts {}}
    (let [raw-tokens (tokenize text)
          tokens (filterv #(not= (first %) :comment) raw-tokens)
          n (count tokens)]
      (loop [i 0
             defs (transient #{})
             spans (transient [])
             all-tokens (transient #{})
             token-counts (transient {})]
        (if (< i n)
          (let [[tok-type s e] (nth tokens i)]
            (cond
              (= tok-type :bracket)
              (if (and (= (.charAt text s) \() (< (inc i) n))
                (let [[_ next-s next-e] (nth tokens (inc i))
                      sym-form (.substring text next-s next-e)]
                  (if (def-forms sym-form)
                    (let [found (loop [j (+ i 2)]
                                  (when (< j n)
                                    (let [[t-type ts te] (nth tokens j)]
                                      (cond
                                        (= t-type :metatag) (recur (inc j))
                                        (#{:symbol :special-form :builtin :constant} t-type) (.substring text ts te)
                                        :else nil))))]
                      (recur (inc i)
                             (if found (conj! defs found) defs)
                             spans
                             all-tokens
                             token-counts))
                    (recur (inc i) defs spans all-tokens token-counts)))
                (recur (inc i) defs spans all-tokens token-counts))

              (= tok-type :symbol)
              (let [sym-str (.substring text s e)
                    cur-cnt (get token-counts sym-str 0)]
                (recur (inc i)
                       defs
                       (conj! spans [sym-str s e])
                       (conj! all-tokens sym-str)
                       (assoc! token-counts sym-str (inc cur-cnt))))

              :else
              (recur (inc i) defs spans all-tokens token-counts)))
          {:defs (persistent! defs)
           :spans (persistent! spans)
           :all-tokens (persistent! all-tokens)
           :token-counts (persistent! token-counts)})))))

(defn get-cached-or-compute-buffer-symbols
  "Returns `{:defs #{...} :spans [...] :all-tokens #{...}}` for `text`.
   Checks `buffer-symbols-cache` first (O(1)). If missing, computes synchronously,
   stores into cache, and returns it."
  [^String text]
  (if (or (nil? text) (str/blank? text))
    {:defs #{} :spans [] :all-tokens #{} :token-counts {}}
    (let [h (hash text)
          now (System/currentTimeMillis)]
      (if-let [entry (get @buffer-symbols-cache h)]
        entry
        (let [computed (assoc (extract-text-symbols text) :timestamp now)]
          (swap! buffer-symbols-cache (fn [m] (prune-cache (assoc m h computed) 32)))
          computed)))))

(defn warm-buffer-cache-async!
  "Asynchronously computes and caches symbols for `text` in `cache-executor` background thread.
   Returns a java.util.concurrent.Future (or nil if text is empty or already cached)."
  [^String text]
  (when (and (string? text) (not (str/blank? text)))
    (let [h (hash text)]
      (when-not (contains? @buffer-symbols-cache h)
        (try
          (.submit ^ExecutorService cache-executor
            ^Runnable (fn []
                        (get-cached-or-compute-buffer-symbols text)))
          (catch Exception _ nil))))))

(defn find-buffer-definitions
  "Extracts all symbols defined in `text` via top-level definition forms
   (defn, def, defmacro, defmulti, defmethod, defonce, defprotocol, defrecord, deftype, etc.).
   Returns a set of symbol names as strings."
  [^String text]
  (:defs (get-cached-or-compute-buffer-symbols text)))

(defn symbol-char?
  "Returns true if `ch` is a valid character inside a Clojure symbol/identifier."
  [ch]
  (and (some? ch)
       (not (Character/isWhitespace ^Character ch))
       (not (contains? #{\( \) \[ \] \{ \} \" \' \` \~ \@ \^ \\ \, \; \:} ch))))

(defn get-symbol-prefix-at-pos
  "Extracts the symbol prefix immediately preceding `pos` in `text`, along with the
   full symbol bounds covering the caret (for replacement).
   Returns `{:prefix str, :start int, :end int, :word-end int}`.
   - `:start` is where the prefix starts.
   - `:end` is `pos` (where the prefix ends).
   - `:word-end` is the end offset of the symbol word following `pos` (if any)."
  [^String text pos]
  (if (or (nil? text) (zero? (.length text)))
    {:prefix "" :start 0 :end 0 :word-end 0}
    (let [len (.length text)
          pos (min len (max 0 (int (or pos 0))))
          start (loop [i (dec pos)]
                  (if (and (>= i 0) (symbol-char? (.charAt text i)))
                    (recur (dec i))
                    (inc i)))
          prefix (.substring text start pos)
          word-end (loop [j pos]
                     (if (and (< j len) (symbol-char? (.charAt text j)))
                       (recur (inc j))
                       j))]
      {:prefix prefix :start start :end pos :word-end word-end})))

(defn get-autocomplete-candidates
  "Returns a sorted vector of candidate maps:
   `{:symbol str, :category (:special | :builtin | :user)}`
   whose symbols start with `prefix`.
   If `text` is provided, user-defined symbols and document symbols from `text`
   are included under the `:user` category.
   If `pos` is provided, the token covering `pos` (the active word being typed)
   is excluded from document symbols.
   If `extra-context` is provided (string or zero-arg fn), user definitions and
   tokens from it are also included."
  ([^String prefix]
   (get-autocomplete-candidates prefix nil nil nil))
  ([^String prefix ^String text]
   (get-autocomplete-candidates prefix text nil nil))
  ([^String prefix ^String text pos]
   (get-autocomplete-candidates prefix text pos nil))
  ([^String prefix ^String text pos extra-context]
   (let [extra-text (if (fn? extra-context) (extra-context) extra-context)
         prefix (or prefix "")
         prefix-lower (str/lower-case prefix)
         pos-int (when (number? pos) (int pos))
         has-prefix? (not (str/blank? prefix))

         ;; Buffer and extra context symbol resolution via background/LRU cache
         text-data (when (and text (not (str/blank? text)))
                     (get-cached-or-compute-buffer-symbols text))
         extra-data (when (and (string? extra-text) (not (str/blank? extra-text)))
                      (get-cached-or-compute-buffer-symbols extra-text))

         ;; User definitions
         user-defs (into (set (or (:defs text-data) #{}))
                         (or (:defs extra-data) #{}))

         ;; Document tokens (excluding active word at pos and prefix itself)
         tokens-text (if text-data
                       (let [all-tokens (:all-tokens text-data)]
                         (if (and pos-int (seq (:spans text-data)))
                           (if-let [sym-at-pos (find-span-at-pos (:spans text-data) (long pos-int))]
                             (if (<= (get (:token-counts text-data) sym-at-pos 1) 1)
                               (disj all-tokens sym-at-pos prefix)
                               (disj all-tokens prefix))
                             (disj all-tokens prefix))
                           (disj all-tokens prefix)))
                       #{})
         tokens-extra (if extra-data
                        (disj (or (:all-tokens extra-data) #{}) prefix)
                        #{})
         user-doc-syms (into tokens-text tokens-extra)

         ;; Runtime namespace definitions
         runtime-user (try
                        (let [curr-ns (or *ns* (find-ns 'user))]
                          (if curr-ns
                            (set (map name (keys (ns-publics curr-ns))))
                            #{}))
                        (catch Exception _ #{}))

         ;; Combine user symbols
         all-core @all-core-builtins
         all-user (into (into (set user-defs) runtime-user)
                        (remove #(or (contains? special-forms %)
                                     (contains? all-core %))
                                user-doc-syms))

         ;; Filter user candidates by prefix
         matching-user (reduce (fn [acc sym]
                                 (if (or (not has-prefix?)
                                         (str/starts-with? (str/lower-case sym) prefix-lower))
                                   (conj! acc {:symbol sym :category :user})
                                   acc))
                               (transient [])
                               all-user)
         matching-user-vec (persistent! matching-user)
         user-syms-set (set (map :symbol matching-user-vec))

         ;; Static candidate lookup via initial-char bucket
         static-pool (if has-prefix?
                       (let [first-ch (Character/toLowerCase ^Character (.charAt prefix 0))]
                         (get (:by-char @static-candidates) first-ch []))
                       (:all @static-candidates))

         ;; Filter static candidates by prefix and exclude any symbol overridden by user
         matching-static (reduce (fn [acc {:keys [symbol] :as cand}]
                                   (if (and (or (not has-prefix?)
                                                (str/starts-with? (str/lower-case symbol) prefix-lower))
                                            (not (contains? user-syms-set symbol)))
                                     (conj! acc cand)
                                     acc))
                                 (transient [])
                                 static-pool)
         combined (concat matching-user-vec (persistent! matching-static))]

     (if (not has-prefix?)
       (if (empty? matching-user-vec)
         (:all @static-candidates)
         (let [sorted-user (vec (sort-by (fn [{:keys [symbol]}]
                                           [(str/lower-case symbol) symbol])
                                         matching-user-vec))]
           (into sorted-user
                 (remove #(contains? user-syms-set (:symbol %))
                         (:all @static-candidates)))))
       (vec
         (sort-by (fn [{:keys [symbol category]}]
                    [(if (str/starts-with? symbol prefix) 0 1)
                     (case category :user 0 :special 1 :builtin 2 3)
                     (str/lower-case symbol)
                     symbol])
                  combined))))))

;; --- Clojure Code Formatter ---

(def body-indent-forms
  #{"def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "definterface" "defonce"
    "let" "loop" "recur" "binding" "with-open" "with-local-vars" "with-redefs"
    "with-out-str" "with-in-str" "with-precision"
    "if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-first" "when-some"
    "cond" "condp" "case" "do"
    "fn" "fn*" "doseq" "dotimes" "for" "while"
    "try" "catch" "finally" "throw" "locking"
    "ns" "testing" "deftest" "is" "are" "comment"
    "extend-protocol" "extend-type" "reify" "proxy"
    "alt!" "alts!" "go" "go-loop"})

(defn- body-form? [op]
  (when op
    (or (contains? body-indent-forms op)
        (str/starts-with? op "def")
        (str/starts-with? op "with-")
        (str/starts-with? op "when-")
        (str/starts-with? op "if-"))))

(defn compute-line-indentations
  "Calculates the target indentation (in spaces) for each line of `text`.
   Returns a vector of integers or `:verbatim` (for lines within multiline strings)."
  [^String text]
  (if (or (nil? text) (zero? (.length text)))
    []
    (let [lines (str/split text #"\n" -1)
          n (count lines)
          line-starts (loop [ls lines, off 0, acc []]
                        (if (empty? ls)
                          acc
                          (recur (rest ls) (+ off (count (first ls)) 1) (conj acc off))))
          tokens (tokenize text)
          multiline-string-lines
          (set (for [[tok-type s e] tokens
                     :when (or (= tok-type :string) (= tok-type :regex))
                     line-idx (range n)
                     :let [ls (nth line-starts line-idx)]
                     :when (< s ls e)]
                 line-idx))]
      (loop [line-idx 0
             tok-idx 0
             stack []
             deltas []
             indents []]
        (if (>= line-idx n)
          indents
          (let [line (nth lines line-idx)
                ls (nth line-starts line-idx)
                first-non-ws (let [len (.length line)]
                               (loop [col 0]
                                 (cond
                                   (>= col len) nil
                                   (not (Character/isWhitespace (.charAt line col))) col
                                   :else (recur (inc col)))))
                target-offset (if first-non-ws (+ ls first-non-ws) ls)
                ;; Advance tok-idx processing all tokens with end <= target-offset
                [new-tok-idx new-stack]
                (loop [ti tok-idx, st stack]
                  (if (or (>= ti (count tokens))
                          (> (nth (nth tokens ti) 2) target-offset))
                    [ti st]
                    (let [[tok-type s e] (nth tokens ti)
                          tok-line (loop [l 0]
                                     (if (or (>= l (dec n))
                                             (< s (nth line-starts (inc l))))
                                       l
                                       (recur (inc l))))
                          tok-delta (if (< tok-line line-idx)
                                      (nth deltas tok-line 0)
                                      0)
                          tok-col (+ (- s (nth line-starts tok-line)) tok-delta)
                          tok-str (.substring text s e)]
                      (cond
                        (= tok-type :bracket)
                        (let [ch (.charAt tok-str 0)]
                          (cond
                            (#{\( \[ \{} ch)
                            (recur (inc ti)
                                   (conj st {:type (case ch \( :list \[ :vector \{ :map)
                                             :open-char ch
                                             :close-char (case ch \( \) \[ \] \{ \})
                                             :line tok-line
                                             :col tok-col
                                             :operator nil
                                             :first-arg-col nil
                                             :first-child-col nil}))
                            (#{\) \] \}} ch)
                            (let [match-idx (first (keep-indexed
                                                     (fn [i frame] (when (= (:close-char frame) ch) i))
                                                     (reverse st)))]
                              (recur (inc ti)
                                     (if match-idx
                                       (subvec st 0 (- (count st) (inc match-idx)))
                                       st)))
                            :else (recur (inc ti) st)))

                        (not= tok-type :comment)
                        ;; Code token: operator or argument
                        (if (empty? st)
                          (recur (inc ti) st)
                          (let [top (peek st)]
                            (if (= (:type top) :list)
                              (if (nil? (:operator top))
                                (recur (inc ti) (conj (pop st) (assoc top :operator tok-str)))
                                (if (and (nil? (:first-arg-col top)) (= (:line top) tok-line))
                                  (recur (inc ti) (conj (pop st) (assoc top :first-arg-col tok-col)))
                                  (recur (inc ti) st)))
                              ;; vector or map
                              (if (and (nil? (:first-child-col top)) (= (:line top) tok-line))
                                (recur (inc ti) (conj (pop st) (assoc top :first-child-col tok-col)))
                                (recur (inc ti) st)))))

                        :else
                        (recur (inc ti) st)))))]
            ;; Calculate indentation for line-idx
            (let [line-indent
                  (cond
                    (multiline-string-lines line-idx) :verbatim
                    (nil? first-non-ws) 0
                    (empty? new-stack) 0
                    :else
                    (let [first-char (.charAt line first-non-ws)
                          top (peek new-stack)]
                      (if (and (= first-char (:close-char top))
                               (not= line-idx (:line top)))
                        (:col top)
                        (case (:type top)
                          :vector (if (:first-child-col top)
                                    (:first-child-col top)
                                    (+ (:col top) 2))
                          :map (if (:first-child-col top)
                                 (:first-child-col top)
                                 (+ (:col top) 2))
                          :list (cond
                                  (body-form? (:operator top))
                                  (+ (:col top) 2)

                                  (contains? #{"->" "->>" "as->" "cond->" "cond->>" "some->" "some->>"}
                                             (:operator top))
                                  (if (:first-arg-col top)
                                    (:first-arg-col top)
                                    (+ (:col top) 2))

                                  :else
                                  (if (:first-arg-col top)
                                    (:first-arg-col top)
                                    (+ (:col top) 2)))))))
                  old-indent (or first-non-ws 0)
                  delta (if (= line-indent :verbatim) 0 (- line-indent old-indent))]
              (recur (inc line-idx)
                     new-tok-idx
                     new-stack
                     (conj deltas delta)
                     (conj indents line-indent)))))))))

(defn format-code
  "Formats Clojure `text` by re-indenting lines according to syntactic nesting
   and cleaning trailing whitespace. Preserves multiline strings and empty lines."
  [^String text]
  (if (str/blank? text)
    text
    (let [lines (str/split text #"\n" -1)
          indents (compute-line-indentations text)
          formatted-lines (mapv (fn [line indent]
                                  (cond
                                    (= indent :verbatim) line
                                    (str/blank? line) ""
                                    :else
                                    (let [trimmed (str/triml line)
                                          clean (str/replace trimmed #"[ \t]+$" "")]
                                      (str (apply str (repeat indent " ")) clean))))
                                lines
                                indents)]
      (str/join "\n" formatted-lines))))

(defn format-selection-text
  "Formats only the lines spanned by `start-offset` to `end-offset` in `text`.
   Lines outside the range are preserved exactly as-is."
  [^String text start-offset end-offset]
  (if (or (str/blank? text) (>= start-offset end-offset))
    text
    (let [lines (str/split text #"\n" -1)
          indents (compute-line-indentations text)
          line-starts (loop [ls lines, off 0, acc []]
                        (if (empty? ls)
                          acc
                          (recur (rest ls) (+ off (count (first ls)) 1) (conj acc off))))
          n (count lines)
          offset->line (fn [pos]
                         (loop [i 0]
                           (if (or (>= i (dec n))
                                   (< pos (nth line-starts (inc i))))
                             i
                             (recur (inc i)))))
          start-line (offset->line start-offset)
          end-line (offset->line (max start-offset (dec end-offset)))
          formatted-lines (map-indexed
                            (fn [idx line]
                              (if (<= start-line idx end-line)
                                (let [indent (nth indents idx)]
                                  (cond
                                    (= indent :verbatim) line
                                    (str/blank? line) ""
                                    :else
                                    (let [trimmed (str/triml line)
                                          clean (str/replace trimmed #"[ \t]+$" "")]
                                      (str (apply str (repeat indent " ")) clean))))
                                line))
                            lines)]
      (str/join "\n" formatted-lines))))

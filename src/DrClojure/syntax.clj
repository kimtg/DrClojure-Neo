(ns DrClojure.syntax
  "Syntax highlighting for Clojure code in DrClojure.
   Provides lexing/tokenization and Swing DefaultStyledDocument styling
   using pure Clojure and standard Java Swing."
  (:import (javax.swing Timer)
           (javax.swing.text DefaultStyledDocument SimpleAttributeSet StyleConstants JTextComponent)
           (javax.swing.event DocumentListener)
           (java.awt Color)
           (java.awt.event ActionListener)
           (java.util.regex Pattern Matcher)))

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
      "|(::?[a-zA-Z0-9_.!$%&*+\\-/<=>?#]+)"                        ;; Group 5: keyword
      "|([+-]?(?:0x[0-9a-fA-F]+|\\d+(?:/\\d+|\\.\\d+)?(?:[eE][+-]?\\d+)?[MN]?)(?=[\\s()\\[\\]{}\",]|$))" ;; Group 6: number
      "|([()\\[\\]{}])"                                            ;; Group 7: bracket / delimiter
      "|(\\^[a-zA-Z0-9_.!$%&*+\\-/<=>?#:]+)"                       ;; Group 8: metadata tag
      "|([a-zA-Z0-9_.!$%&*+\\-/<=>?#]+)")))                        ;; Group 9: symbol

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

(defn setup-syntax-highlighting!
  "Attaches a live syntax highlighting listener to `pane`.
   Uses a debounced Swing Timer so that rapid typing remains smooth and responsive.
   Returns a map with:
   - `:timer` (javax.swing.Timer)
   - `:bracket-info` (atom holding latest bracket info)
   - `:highlight-now!` (fn [] ... to force immediate re-highlighting)."
  [^JTextComponent pane & [{:keys [delay-ms palette] :or {delay-ms 60 palette default-palette}}]]
  (let [doc ^DefaultStyledDocument (.getDocument pane)
        binfo-atom (atom {:bracket-tokens [] :matches {} :unmatched #{}})
        highlight-fn (fn []
                       (let [info (highlight-doc! doc palette)]
                         (reset! binfo-atom info)))
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


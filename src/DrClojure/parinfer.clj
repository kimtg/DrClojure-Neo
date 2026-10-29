(ns DrClojure.parinfer
  "Implementation of Shaun Lebron's Parinfer algorithm for Clojure.
   Supports:
   - Smart Mode: Automatically decides whether to adjust indentation or parentheses based on user edits.
   - Indent Mode: Infers parenthesis structure from indentation.
   - Paren Mode: Infers indentation from parentheses."
  (:require [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Constants

(def ^:const UINT_NULL -999)

(def ^:const INDENT_MODE :indent-mode)
(def ^:const PAREN_MODE :paren-mode)

(def ^:const BACKSLASH "\\")
(def ^:const BLANK_SPACE " ")
(def ^:const DOUBLE_SPACE "  ")
(def ^:const DOUBLE_QUOTE "\"")
(def ^:const NEWLINE "\n")
(def ^:const TAB "\t")

(def line-ending-regex #"\r?\n")

(def match-paren
  {"{" "}"
   "}" "{"
   "[" "]"
   "]" "["
   "(" ")"
   ")" "("})

;; Possible Errors
(def ^:const ERROR_QUOTE_DANGER "quote-danger")
(def ^:const ERROR_EOL_BACKSLASH "eol-backslash")
(def ^:const ERROR_UNCLOSED_QUOTE "unclosed-quote")
(def ^:const ERROR_UNCLOSED_PAREN "unclosed-paren")
(def ^:const ERROR_UNMATCHED_CLOSE_PAREN "unmatched-close-paren")
(def ^:const ERROR_UNMATCHED_OPEN_PAREN "unmatched-open-paren")
(def ^:const ERROR_LEADING_CLOSE_PAREN "leading-close-paren")
(def ^:const ERROR_UNHANDLED "unhandled")

(def error-messages
  {ERROR_QUOTE_DANGER "Quotes must balanced inside comment blocks."
   ERROR_EOL_BACKSLASH "Line cannot end in a hanging backslash."
   ERROR_UNCLOSED_QUOTE "String is missing a closing quote."
   ERROR_UNCLOSED_PAREN "Unclosed open-paren."
   ERROR_UNMATCHED_CLOSE_PAREN "Unmatched close-paren."
   ERROR_UNMATCHED_OPEN_PAREN "Unmatched open-paren."
   ERROR_LEADING_CLOSE_PAREN "Line cannot lead with a close-paren."
   ERROR_UNHANDLED "Unhandled error."})

;; -----------------------------------------------------------------------------
;; Stack Helpers (using java.util.ArrayList)

(defn- stack-empty? [^java.util.ArrayList s]
  (.isEmpty s))

(defn- peek-stack [^java.util.ArrayList arr ^long idx-from-back]
  (let [max-idx (dec (.size arr))]
    (when (<= idx-from-back max-idx)
      (.get arr (- max-idx idx-from-back)))))

(defn- stack-pop! [^java.util.ArrayList s]
  (when-not (.isEmpty s)
    (.remove s (dec (.size s)))))

(defn- stack-push! [^java.util.ArrayList s itm]
  (.add s itm)
  nil)

(defn- array-slice [^java.util.ArrayList arr from-idx to-idx]
  (let [size (.size arr)
        f (int (max 0 (min (int from-idx) size)))
        t (int (max f (min (int to-idx) size)))]
    (java.util.ArrayList. (.subList arr f t))))

;; -----------------------------------------------------------------------------
;; String Helpers

(defn- js-substring
  ([^String s from]
   (let [len (.length s)
         f (int (max 0 (min (int from) len)))]
     (.substring s f len)))
  ([^String s from to]
   (let [len (.length s)
         f (int (max 0 (min (int from) len)))
         t (int (max 0 (min (int to) len)))]
     (if (<= f t)
       (.substring s f t)
       (.substring s t f)))))

(defn- replace-within-string [^String orig start-idx end-idx ^String replace-txt]
  (let [head (js-substring orig 0 start-idx)
        tail (js-substring orig end-idx)]
    (str head replace-txt tail)))

(defn- repeat-string [^String text ^long n]
  (if (<= n 0)
    ""
    (let [sb (StringBuilder.)]
      (dotimes [_ n] (.append sb text))
      (.toString sb))))

(defn- get-line-ending [^String text]
  (if (and text (.contains text "\r"))
    "\r\n"
    "\n"))

;; -----------------------------------------------------------------------------
;; Options & Changes Structure

(defn- transform-change [change]
  (when change
    (let [x (long (or (:x change) 0))
          line-no (long (or (:line-no change) (:lineNo change) 0))
          old-text (str (or (:old-text change) (:oldText change) ""))
          new-text (str (or (:new-text change) (:newText change) ""))
          new-lines (str/split new-text line-ending-regex -1)
          old-lines (str/split old-text line-ending-regex -1)
          last-old-line (last old-lines)
          last-old-line-len (count last-old-line)
          last-new-line (last new-lines)
          last-new-line-len (count last-new-line)
          carry-over-old-x (if (= (count old-lines) 1) x 0)
          old-end-x (+ carry-over-old-x last-old-line-len)
          carry-over-new-x (if (= (count new-lines) 1) x 0)
          new-end-x (+ carry-over-new-x last-new-line-len)
          new-end-line-no (+ line-no (dec (count new-lines)))]
      {:x x
       :line-no line-no
       :old-text old-text
       :new-text new-text
       :old-end-x old-end-x
       :new-end-x new-end-x
       :new-end-line-no new-end-line-no
       :lookup-line-no new-end-line-no
       :lookup-x new-end-x})))

(defn- transform-changes [changes]
  (if (or (nil? changes) (empty? changes))
    nil
    (let [lines (java.util.HashMap.)]
      (doseq [c changes]
        (when-let [change (transform-change c)]
          (let [line-no (long (:lookup-line-no change))
                x (long (:lookup-x change))
                line-map (or (.get lines line-no)
                             (let [m (java.util.HashMap.)]
                               (.put lines line-no m)
                               m))]
            (.put ^java.util.HashMap line-map x change))))
      lines)))

(defn- parse-options [options]
  (let [opts (or options {})
        comment-chars (or (:comment-chars opts) (:commentChars opts))
        open-paren-chars (or (:open-paren-chars opts) (:openParenChars opts))
        close-paren-chars (or (:close-paren-chars opts) (:closeParenChars opts))
        to-set-str (fn [v default-val]
                     (cond
                       (nil? v) default-val
                       (coll? v) (set (map str v))
                       :else #{(str v)}))]
    {:changes (or (:changes opts) nil)
     :comment-chars (to-set-str comment-chars #{";"})
     :open-paren-chars (to-set-str open-paren-chars #{"(" "[" "{"})
     :close-paren-chars (to-set-str close-paren-chars #{")" "]" "}"})
     :cursor-line (or (:cursor-line opts) (:cursorLine opts) UINT_NULL)
     :cursor-x (or (:cursor-x opts) (:cursorX opts) UINT_NULL)
     :force-balance (boolean (or (:force-balance opts) (:forceBalance opts)))
     :partial-result (boolean (or (:partial-result opts) (:partialResult opts)))
     :prev-cursor-line (or (:prev-cursor-line opts) (:prevCursorLine opts) UINT_NULL)
     :prev-cursor-x (or (:prev-cursor-x opts) (:prevCursorX opts) UINT_NULL)
     :return-parens (boolean (or (:return-parens opts) (:returnParens opts)))
     :selection-start-line (or (:selection-start-line opts) (:selectionStartLine opts) UINT_NULL)}))

;; -----------------------------------------------------------------------------
;; Result Initialization

(defn- initial-paren-trail []
  (doto (java.util.HashMap.)
    (.put :line-no UINT_NULL)
    (.put :start-x UINT_NULL)
    (.put :end-x UINT_NULL)
    (.put :openers (java.util.ArrayList.))
    (.put :clamped (doto (java.util.HashMap.)
                     (.put :start-x UINT_NULL)
                     (.put :end-x UINT_NULL)
                     (.put :openers (java.util.ArrayList.))))))

(defn- get-initial-result [^String text options mode smart]
  (let [parsed-opts (parse-options options)
        safe-text (or text "")
        input-lines (into-array String (str/split safe-text line-ending-regex -1))
        result (java.util.HashMap.)]
    (doto result
      (.put :mode mode)
      (.put :smart (boolean smart))
      (.put :orig-text safe-text)
      (.put :orig-cursor-x (long (:cursor-x parsed-opts)))
      (.put :orig-cursor-line (long (:cursor-line parsed-opts)))
      (.put :input-lines input-lines)
      (.put :input-line-no (long -1))
      (.put :input-x (long -1))
      (.put :lines (java.util.ArrayList.))
      (.put :line-no (long -1))
      (.put :ch "")
      (.put :x (long 0))
      (.put :indent-x UINT_NULL)
      (.put :paren-stack (java.util.ArrayList.))
      (.put :tab-stops (java.util.ArrayList.))
      (.put :paren-trail (initial-paren-trail))
      (.put :paren-trails (java.util.ArrayList.))
      (.put :return-parens (:return-parens parsed-opts))
      (.put :parens (java.util.ArrayList.))
      (.put :cursor-x (long (:cursor-x parsed-opts)))
      (.put :cursor-line (long (:cursor-line parsed-opts)))
      (.put :prev-cursor-x (long (:prev-cursor-x parsed-opts)))
      (.put :prev-cursor-line (long (:prev-cursor-line parsed-opts)))
      (.put :comment-chars (:comment-chars parsed-opts))
      (.put :open-paren-chars (:open-paren-chars parsed-opts))
      (.put :close-paren-chars (:close-paren-chars parsed-opts))
      (.put :selection-start-line (long (:selection-start-line parsed-opts)))
      (.put :changes (transform-changes (:changes parsed-opts)))
      (.put :is-in-code true)
      (.put :is-escaping false)
      (.put :is-escaped false)
      (.put :is-in-str false)
      (.put :is-in-comment false)
      (.put :comment-x UINT_NULL)
      (.put :quote-danger false)
      (.put :tracking-indent false)
      (.put :skip-char false)
      (.put :success false)
      (.put :partial-result (:partial-result parsed-opts))
      (.put :force-balance (:force-balance parsed-opts))
      (.put :max-indent UINT_NULL)
      (.put :indent-delta (long 0))
      (.put :tracking-arg-tab-stop nil)
      (.put :error (doto (java.util.HashMap.)
                     (.put :name nil)
                     (.put :message nil)
                     (.put :line-no nil)
                     (.put :x nil)
                     (.put :extra nil)))
      (.put :error-pos-cache (java.util.HashMap.)))))

;; -----------------------------------------------------------------------------
;; Error Handling

(defn- cache-error-pos! [^java.util.HashMap result error-name]
  (let [e (doto (java.util.HashMap.)
            (.put :line-no (long (.get result :line-no)))
            (.put :x (long (.get result :x)))
            (.put :input-line-no (long (.get result :input-line-no)))
            (.put :input-x (long (.get result :input-x))))
        cache ^java.util.HashMap (.get result :error-pos-cache)]
    (.put cache error-name e)
    e))

(defn- create-error [^java.util.HashMap result error-name]
  (let [cache ^java.util.HashMap (.get result :error-pos-cache)
        cached-pos ^java.util.HashMap (when cache (.get cache error-name))
        partial? (boolean (.get result :partial-result))
        key-line (if partial? :line-no :input-line-no)
        key-x (if partial? :x :input-x)
        line-no (if cached-pos (.get cached-pos key-line) (.get result key-line))
        x (if cached-pos (.get cached-pos key-x) (.get result key-x))
        opener ^java.util.HashMap (peek-stack (.get result :paren-stack) 0)
        err (doto (java.util.HashMap.)
              (.put :parinfer-error true)
              (.put :name error-name)
              (.put :message (get error-messages error-name))
              (.put :line-no line-no)
              (.put :x x))]
    (cond
      (= error-name ERROR_UNMATCHED_CLOSE_PAREN)
      (let [cached-open ^java.util.HashMap (when cache (.get cache ERROR_UNMATCHED_OPEN_PAREN))]
        (when (or cached-open opener)
          (let [line-no2 (if cached-open (.get cached-open key-line) (.get opener key-line))
                x2 (if cached-open (.get cached-open key-x) (.get opener key-x))]
            (.put err :extra {:name ERROR_UNMATCHED_OPEN_PAREN
                              :line-no line-no2
                              :x x2}))))

      (= error-name ERROR_UNCLOSED_PAREN)
      (when opener
        (.put err :line-no (.get opener key-line))
        (.put err :x (.get opener key-x))))
    (ex-info "ParinferError" {:error err})))

(defn- exit-to-paren-mode [reason]
  (ex-info "ExitToParenMode" {:exit-to-paren-mode true :reason reason}))

;; -----------------------------------------------------------------------------
;; Line Operations & Cursors

(defn- cursor-affected? [cursor-x start end]
  (let [cx (long cursor-x)
        s (long start)
        e (long end)]
    (if (and (= cx s) (= cx e))
      (= cx 0)
      (>= cx e))))

(defn- shift-cursor-on-edit! [^java.util.HashMap result line-no start end ^String replace-txt]
  (let [l-no (long line-no)
        s (long start)
        e (long end)
        old-length (- e s)
        new-length (.length replace-txt)
        dx (- new-length old-length)
        cursor-line (long (.get result :cursor-line))
        cursor-x (long (.get result :cursor-x))]
    (when (and (not= dx 0)
               (= cursor-line l-no)
               (not= cursor-x UINT_NULL)
               (cursor-affected? cursor-x s e))
      (.put result :cursor-x (+ cursor-x dx)))))

(defn- replace-within-line! [^java.util.HashMap result line-no start-idx end-idx ^String replace-txt]
  (let [l-no (long line-no)
        s (long start-idx)
        e (long end-idx)
        lines ^java.util.ArrayList (.get result :lines)
        line ^String (.get lines (int l-no))
        new-line (replace-within-string line s e replace-txt)]
    (.set lines (int l-no) new-line)
    (shift-cursor-on-edit! result l-no s e replace-txt)))

(defn- insert-within-line! [^java.util.HashMap result line-no idx ^String insert-txt]
  (replace-within-line! result line-no idx idx insert-txt))

(defn- init-line! [^java.util.HashMap result]
  (.put result :x (long 0))
  (.put result :line-no (inc (long (.get result :line-no))))
  (.put result :indent-x UINT_NULL)
  (.put result :comment-x UINT_NULL)
  (.put result :indent-delta (long 0))
  (let [cache ^java.util.HashMap (.get result :error-pos-cache)]
    (.remove cache ERROR_UNMATCHED_CLOSE_PAREN)
    (.remove cache ERROR_UNMATCHED_OPEN_PAREN)
    (.remove cache ERROR_LEADING_CLOSE_PAREN))
  (.put result :tracking-arg-tab-stop nil)
  (.put result :tracking-indent (not (boolean (.get result :is-in-str)))))

(defn- commit-char! [^java.util.HashMap result ^String orig-ch]
  (let [ch ^String (.get result :ch)
        orig-len (.length orig-ch)
        ch-len (.length ch)]
    (when-not (= orig-ch ch)
      (replace-within-line! result (long (.get result :line-no)) (long (.get result :x)) (+ (long (.get result :x)) orig-len) ch)
      (.put result :indent-delta (- (long (.get result :indent-delta)) (- orig-len ch-len))))
    (.put result :x (+ (long (.get result :x)) ch-len))))

(defn- clamp-val [^long val ^long min-n ^long max-n]
  (let [v (if (not= min-n UINT_NULL) (max min-n val) val)]
    (if (not= max-n UINT_NULL) (min max-n v) v)))

;; -----------------------------------------------------------------------------
;; Character Properties

(defn- open-paren? [^String ch open-paren-chars]
  (contains? open-paren-chars ch))

(defn- close-paren? [^String ch close-paren-chars]
  (contains? close-paren-chars ch))

(defn- valid-close-paren? [^java.util.ArrayList paren-stack ^String ch]
  (if (stack-empty? paren-stack)
    false
    (let [opener ^java.util.HashMap (peek-stack paren-stack 0)]
      (= (get match-paren ch) (.get opener :ch)))))

(defn- whitespace? [^java.util.HashMap result]
  (let [ch ^String (.get result :ch)]
    (and (not (boolean (.get result :is-escaped)))
         (or (= ch BLANK_SPACE) (= ch DOUBLE_SPACE)))))

(defn- closable? [^java.util.HashMap result]
  (let [ch ^String (.get result :ch)
        close-chars (.get result :close-paren-chars)
        closer? (and (close-paren? ch close-chars)
                     (not (boolean (.get result :is-escaped))))]
    (and (boolean (.get result :is-in-code))
         (not (whitespace? result))
         (not= ch "")
         (not closer?))))

(defn- comment-char? [^String ch comment-chars]
  (contains? comment-chars ch))

;; -----------------------------------------------------------------------------
;; Cursor Holding & Argument Tab Stops

(defn- check-cursor-holding [^java.util.HashMap result]
  (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
        opener ^java.util.HashMap (peek-stack paren-stack 0)
        parent ^java.util.HashMap (peek-stack paren-stack 1)
        hold-min-x (if parent (inc (long (.get parent :x))) 0)
        hold-max-x (long (.get opener :x))
        cursor-line (long (.get result :cursor-line))
        cursor-x (long (.get result :cursor-x))
        holding (and (= cursor-line (long (.get opener :line-no)))
                     (<= hold-min-x cursor-x hold-max-x))
        changes (.get result :changes)
        prev-cursor-line (long (.get result :prev-cursor-line))
        prev-cursor-x (long (.get result :prev-cursor-x))
        should-check-prev? (and (nil? changes) (not= prev-cursor-line UINT_NULL))]
    (when should-check-prev?
      (let [prev-holding (and (= prev-cursor-line (long (.get opener :line-no)))
                              (<= hold-min-x prev-cursor-x hold-max-x))]
        (when (and prev-holding (not holding))
          (throw (exit-to-paren-mode "releaseCursorHold")))))
    holding))

(defn- track-arg-tab-stop! [^java.util.HashMap result state]
  (cond
    (= state "space")
    (when (and (boolean (.get result :is-in-code)) (whitespace? result))
      (.put result :tracking-arg-tab-stop "arg"))

    (= state "arg")
    (when-not (whitespace? result)
      (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
            opener ^java.util.HashMap (peek-stack paren-stack 0)]
        (when opener
          (.put opener :arg-x (long (.get result :x))))
        (.put result :tracking-arg-tab-stop nil)))))

;; -----------------------------------------------------------------------------
;; Paren Trail Core Functions

(defn- reset-paren-trail! [^java.util.HashMap result line-no x]
  (let [trail ^java.util.HashMap (.get result :paren-trail)]
    (.put trail :line-no (long line-no))
    (.put trail :start-x (long x))
    (.put trail :end-x (long x))
    (.put trail :openers (java.util.ArrayList.))
    (let [clamped ^java.util.HashMap (.get trail :clamped)]
      (.put clamped :start-x UINT_NULL)
      (.put clamped :end-x UINT_NULL)
      (.put clamped :openers (java.util.ArrayList.)))))

(defn- remember-paren-trail! [^java.util.HashMap result]
  (let [trail ^java.util.HashMap (.get result :paren-trail)
        clamped ^java.util.HashMap (.get trail :clamped)
        openers ^java.util.ArrayList (.get trail :openers)
        clamped-openers ^java.util.ArrayList (.get clamped :openers)
        has-openers? (or (not (.isEmpty clamped-openers)) (not (.isEmpty openers)))]
    (when has-openers?
      (let [is-clamped (not= (.get clamped :start-x) UINT_NULL)
            all-clamped (.isEmpty openers)
            start-x (if is-clamped (.get clamped :start-x) (.get trail :start-x))
            end-x (if all-clamped (.get clamped :end-x) (.get trail :end-x))
            short-trail {:line-no (long (.get trail :line-no))
                         :start-x (long start-x)
                         :end-x (long end-x)}
            paren-trails ^java.util.ArrayList (.get result :paren-trails)]
        (stack-push! paren-trails short-trail)))))

(defn- update-remembered-paren-trail! [^java.util.HashMap result]
  (let [paren-trails ^java.util.ArrayList (.get result :paren-trails)
        trail-last (peek-stack paren-trails 0)
        curr-trail ^java.util.HashMap (.get result :paren-trail)]
    (if (or (nil? trail-last) (not= (:line-no trail-last) (.get curr-trail :line-no)))
      (remember-paren-trail! result)
      (let [idx (dec (.size paren-trails))]
        (.set paren-trails idx (assoc trail-last :end-x (long (.get curr-trail :end-x))))))))

(defn- on-open-paren! [^java.util.HashMap result]
  (when (boolean (.get result :is-in-code))
    (let [opener (doto (java.util.HashMap.)
                   (.put :input-line-no (long (.get result :input-line-no)))
                   (.put :input-x (long (.get result :input-x)))
                   (.put :line-no (long (.get result :line-no)))
                   (.put :x (long (.get result :x)))
                   (.put :ch ^String (.get result :ch))
                   (.put :indent-delta (long (.get result :indent-delta)))
                   (.put :max-child-indent UINT_NULL))]
      (when (boolean (.get result :return-parens))
        (.put opener :children (java.util.ArrayList.))
        (.put opener :closer (doto (java.util.HashMap.)
                               (.put :line-no UINT_NULL)
                               (.put :x UINT_NULL)
                               (.put :ch "")))
        (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
              parent1 ^java.util.HashMap (peek-stack paren-stack 0)
              parent2 (if parent1 (.get parent1 :children) (.get result :parens))]
          (stack-push! parent2 opener)))
      (stack-push! (.get result :paren-stack) opener)
      (.put result :tracking-arg-tab-stop "space"))))

(defn- set-closer! [^java.util.HashMap opener line-no x ch]
  (when-let [closer ^java.util.HashMap (.get opener :closer)]
    (.put closer :line-no (long line-no))
    (.put closer :x (long x))
    (.put closer :ch (str ch))))

(defn- on-matched-close-paren! [^java.util.HashMap result]
  (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
        opener ^java.util.HashMap (peek-stack paren-stack 0)
        trail ^java.util.HashMap (.get result :paren-trail)]
    (when (boolean (.get result :return-parens))
      (set-closer! opener (.get result :line-no) (.get result :x) (.get result :ch)))
    (.put trail :end-x (inc (long (.get result :x))))
    (stack-push! (.get trail :openers) opener)

    (when (and (= (.get result :mode) INDENT_MODE)
               (boolean (.get result :smart))
               (check-cursor-holding result))
      (let [orig-start-x (.get trail :start-x)
            orig-end-x (.get trail :end-x)
            orig-openers (.get trail :openers)]
        (reset-paren-trail! result (.get result :line-no) (inc (long (.get result :x))))
        (let [clamped ^java.util.HashMap (.get trail :clamped)]
          (.put clamped :start-x orig-start-x)
          (.put clamped :end-x orig-end-x)
          (.put clamped :openers orig-openers))))
    (stack-pop! paren-stack)
    (.put result :tracking-arg-tab-stop nil)))

(defn- on-unmatched-close-paren! [^java.util.HashMap result]
  (if (= (.get result :mode) PAREN_MODE)
    (let [trail ^java.util.HashMap (.get result :paren-trail)
          in-leading? (and (= (.get trail :line-no) (.get result :line-no))
                           (= (.get trail :start-x) (.get result :indent-x)))
          can-remove? (and (boolean (.get result :smart)) in-leading?)]
      (when-not can-remove?
        (throw (create-error result ERROR_UNMATCHED_CLOSE_PAREN))))
    (when (and (= (.get result :mode) INDENT_MODE)
               (not (.containsKey ^java.util.HashMap (.get result :error-pos-cache) ERROR_UNMATCHED_CLOSE_PAREN)))
      (cache-error-pos! result ERROR_UNMATCHED_CLOSE_PAREN)
      (when-let [opener ^java.util.HashMap (peek-stack (.get result :paren-stack) 0)]
        (let [e ^java.util.HashMap (cache-error-pos! result ERROR_UNMATCHED_OPEN_PAREN)]
          (.put e :input-line-no (.get opener :input-line-no))
          (.put e :input-x (.get opener :input-x))))))
  (.put result :ch ""))

(defn- on-close-paren! [^java.util.HashMap result]
  (when (boolean (.get result :is-in-code))
    (if (valid-close-paren? (.get result :paren-stack) (.get result :ch))
      (on-matched-close-paren! result)
      (on-unmatched-close-paren! result))))

(defn- on-tab! [^java.util.HashMap result]
  (when (boolean (.get result :is-in-code))
    (.put result :ch DOUBLE_SPACE)))

(defn- on-comment-char! [^java.util.HashMap result]
  (when (boolean (.get result :is-in-code))
    (.put result :is-in-comment true)
    (.put result :comment-x (long (.get result :x)))
    (.put result :tracking-arg-tab-stop nil)))

(defn- on-newline! [^java.util.HashMap result]
  (.put result :is-in-comment false)
  (.put result :ch ""))

(defn- on-quote! [^java.util.HashMap result]
  (cond
    (boolean (.get result :is-in-str))
    (.put result :is-in-str false)

    (boolean (.get result :is-in-comment))
    (do
      (.put result :quote-danger (not (boolean (.get result :quote-danger))))
      (when (boolean (.get result :quote-danger))
        (cache-error-pos! result ERROR_QUOTE_DANGER)))

    :else
    (do
      (.put result :is-in-str true)
      (cache-error-pos! result ERROR_UNCLOSED_QUOTE))))

(defn- on-backslash! [^java.util.HashMap result]
  (.put result :is-escaping true))

(defn- after-backslash! [^java.util.HashMap result]
  (.put result :is-escaping false)
  (.put result :is-escaped true)
  (when (= (.get result :ch) NEWLINE)
    (if (boolean (.get result :is-in-code))
      (throw (create-error result ERROR_EOL_BACKSLASH))
      (on-newline! result))))

(defn- on-char! [^java.util.HashMap result]
  (let [ch ^String (.get result :ch)]
    (.put result :is-escaped false)
    (cond
      (boolean (.get result :is-escaping)) (after-backslash! result)
      (open-paren? ch (.get result :open-paren-chars)) (on-open-paren! result)
      (close-paren? ch (.get result :close-paren-chars)) (on-close-paren! result)
      (= ch DOUBLE_QUOTE) (on-quote! result)
      (comment-char? ch (.get result :comment-chars)) (on-comment-char! result)
      (= ch BACKSLASH) (on-backslash! result)
      (= ch TAB) (on-tab! result)
      (= ch NEWLINE) (on-newline! result)))

  (let [ch ^String (.get result :ch)]
    (.put result :is-in-code (and (not (boolean (.get result :is-in-comment)))
                                  (not (boolean (.get result :is-in-str)))))
    (when (closable? result)
      (reset-paren-trail! result (.get result :line-no) (+ (long (.get result :x)) (.length ch))))

    (when-let [state (.get result :tracking-arg-tab-stop)]
      (track-arg-tab-stop! result state))))

;; -----------------------------------------------------------------------------
;; Cursor Predicates & Changes

(defn- cursor-left-of? [cursor-x cursor-line x line-no]
  (let [cx (long cursor-x)
        cl (long cursor-line)
        vx (long x)
        ln (long line-no)]
    (and (= cl ln)
         (not= vx UINT_NULL)
         (not= cx UINT_NULL)
         (<= cx vx))))

(defn- cursor-right-of? [cursor-x cursor-line x line-no]
  (let [cx (long cursor-x)
        cl (long cursor-line)
        vx (long x)
        ln (long line-no)]
    (and (= cl ln)
         (not= vx UINT_NULL)
         (not= cx UINT_NULL)
         (> cx vx))))

(defn- handle-change-delta! [^java.util.HashMap result]
  (when-let [changes ^java.util.HashMap (.get result :changes)]
    (when (or (boolean (.get result :smart))
              (= (.get result :mode) PAREN_MODE))
      (when-let [line ^java.util.HashMap (.get changes (.get result :input-line-no))]
        (when-let [change (.get line (.get result :input-x))]
          (.put result :indent-delta (+ (long (.get result :indent-delta))
                                        (- (long (:new-end-x change)) (long (:old-end-x change))))))))))

;; -----------------------------------------------------------------------------
;; Clamping and Stack Manipulation

(defn- clamp-paren-trail-to-cursor! [^java.util.HashMap result]
  (let [trail ^java.util.HashMap (.get result :paren-trail)
        start-x (long (.get trail :start-x))
        end-x (long (.get trail :end-x))
        cursor-x (long (.get result :cursor-x))
        cursor-line (long (.get result :cursor-line))
        clamping? (and (cursor-right-of? cursor-x cursor-line start-x (long (.get result :line-no)))
                       (not (cursor-right-of? cursor-x cursor-line (long (.get result :comment-x)) (long (.get result :line-no)))))]
    (when clamping?
      (let [new-start-x (max start-x cursor-x)
            new-end-x (max end-x cursor-x)
            lines ^java.util.ArrayList (.get result :lines)
            line ^String (.get lines (int (long (.get result :line-no))))
            close-chars (.get result :close-paren-chars)
            remove-count (loop [i start-x, cnt 0]
                           (if (< i new-start-x)
                             (let [ch (str (.charAt line (int i)))]
                               (recur (inc i) (if (close-paren? ch close-chars) (inc cnt) cnt)))
                             cnt))
            openers ^java.util.ArrayList (.get trail :openers)
            openers-len (.size openers)
            clamped ^java.util.HashMap (.get trail :clamped)]
        (.put clamped :openers (array-slice openers 0 remove-count))
        (.put clamped :start-x start-x)
        (.put clamped :end-x end-x)

        (.put trail :openers (array-slice openers remove-count openers-len))
        (.put trail :start-x new-start-x)
        (.put trail :end-x new-end-x)))))

(defn- pop-paren-trail! [^java.util.HashMap result]
  (let [trail ^java.util.HashMap (.get result :paren-trail)
        start-x (long (.get trail :start-x))
        end-x (long (.get trail :end-x))]
    (when-not (= start-x end-x)
      (let [openers ^java.util.ArrayList (.get trail :openers)
            paren-stack ^java.util.ArrayList (.get result :paren-stack)]
        (while (not (stack-empty? openers))
          (let [itm (stack-pop! openers)]
            (stack-push! paren-stack itm)))))))

;; -----------------------------------------------------------------------------
;; Smart Mode Parent Opener Resolution

(defn- get-parent-opener-index [^java.util.HashMap result ^long indent-x]
  (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
        len (.size paren-stack)]
    (loop [i 0]
      (if (< i len)
        (let [opener ^java.util.HashMap (peek-stack paren-stack i)
              curr-outside (< (long (.get opener :x)) indent-x)
              prev-indent-x (- indent-x (long (.get result :indent-delta)))
              prev-outside (< (- (long (.get opener :x)) (long (.get opener :indent-delta))) prev-indent-x)
              is-parent (cond
                          (and prev-outside curr-outside) true
                          (and (not prev-outside) (not curr-outside)) false

                          ;; Possible fragmentation
                          (and prev-outside (not curr-outside))
                          (cond
                            (= (long (.get result :indent-delta)) 0) true
                            (= (long (.get opener :indent-delta)) 0) false
                            :else false)

                          ;; Possible adoption
                          (and (not prev-outside) curr-outside)
                          (let [next-opener ^java.util.HashMap (peek-stack paren-stack (inc i))
                                parent?
                                (cond
                                  (and next-opener (<= (long (.get next-opener :indent-delta)) (long (.get opener :indent-delta))))
                                  (> (+ indent-x (long (.get next-opener :indent-delta))) (long (.get opener :x)))

                                  (and next-opener (> (long (.get next-opener :indent-delta)) (long (.get opener :indent-delta))))
                                  true

                                  (> (long (.get result :indent-delta)) (long (.get opener :indent-delta)))
                                  true

                                  :else false)]
                            (when parent?
                              (.put opener :indent-delta (long 0)))
                            parent?)

                          :else false)]
          (if is-parent
            i
            (recur (inc i))))
        len))))

(defn- correct-paren-trail! [^java.util.HashMap result ^long indent-x]
  (let [opener-idx (get-parent-opener-index result indent-x)
        trail ^java.util.HashMap (.get result :paren-trail)
        paren-stack ^java.util.ArrayList (.get result :paren-stack)
        openers ^java.util.ArrayList (.get trail :openers)
        sb (StringBuilder.)]
    (dotimes [i opener-idx]
      (let [opener ^java.util.HashMap (stack-pop! paren-stack)
            close-ch (get match-paren (.get opener :ch))]
        (stack-push! openers opener)
        (.append sb close-ch)
        (when (boolean (.get result :return-parens))
          (set-closer! opener (.get trail :line-no) (+ (long (.get trail :start-x)) i) close-ch))))
    (when (not= (.get trail :line-no) UINT_NULL)
      (let [parens (.toString sb)]
        (replace-within-line! result (long (.get trail :line-no)) (long (.get trail :start-x)) (long (.get trail :end-x)) parens)
        (.put trail :end-x (+ (long (.get trail :start-x)) (.length parens)))
        (remember-paren-trail! result)))))

(defn- clean-paren-trail! [^java.util.HashMap result]
  (let [trail ^java.util.HashMap (.get result :paren-trail)
        start-x (long (.get trail :start-x))
        end-x (long (.get trail :end-x))]
    (when-not (or (= start-x end-x)
                  (not= (.get result :line-no) (.get trail :line-no)))
      (let [lines ^java.util.ArrayList (.get result :lines)
            line ^String (.get lines (int (long (.get result :line-no))))
            close-chars (.get result :close-paren-chars)
            sb (StringBuilder.)
            space-count (loop [i start-x, sc 0]
                          (if (< i end-x)
                            (let [ch (str (.charAt line (int i)))]
                              (if (close-paren? ch close-chars)
                                (do (.append sb ch) (recur (inc i) sc))
                                (recur (inc i) (inc sc))))
                            sc))]
        (when (pos? space-count)
          (replace-within-line! result (long (.get result :line-no)) start-x end-x (.toString sb))
          (.put trail :end-x (- end-x space-count)))))))

(defn- set-max-indent! [^java.util.HashMap result ^java.util.HashMap opener]
  (when opener
    (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
          parent ^java.util.HashMap (peek-stack paren-stack 0)]
      (if parent
        (.put parent :max-child-indent (.get opener :x))
        (.put result :max-indent (.get opener :x))))))

(defn- append-paren-trail! [^java.util.HashMap result]
  (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
        opener ^java.util.HashMap (stack-pop! paren-stack)
        close-ch (get match-paren (.get opener :ch))
        trail ^java.util.HashMap (.get result :paren-trail)]
    (when (boolean (.get result :return-parens))
      (set-closer! opener (.get trail :line-no) (.get trail :end-x) close-ch))
    (set-max-indent! result opener)
    (insert-within-line! result (long (.get trail :line-no)) (long (.get trail :end-x)) close-ch)
    (.put trail :end-x (inc (long (.get trail :end-x))))
    (stack-push! (.get trail :openers) opener)
    (update-remembered-paren-trail! result)))

(defn- finish-new-paren-trail! [^java.util.HashMap result]
  (cond
    (boolean (.get result :is-in-str))
    (.put result :paren-trail (initial-paren-trail))

    (= (.get result :mode) INDENT_MODE)
    (do
      (clamp-paren-trail-to-cursor! result)
      (pop-paren-trail! result))

    (= (.get result :mode) PAREN_MODE)
    (do
      (set-max-indent! result (peek-stack (.get ^java.util.HashMap (.get result :paren-trail) :openers) 0))
      (when-not (= (.get result :line-no) (.get result :cursor-line))
        (clean-paren-trail! result))
      (remember-paren-trail! result))))

;; -----------------------------------------------------------------------------
;; Indentation Functions

(defn- add-indent! [^java.util.HashMap result ^long delta]
  (let [orig-indent (long (.get result :x))
        new-indent (+ orig-indent delta)
        indent-str (repeat-string BLANK_SPACE new-indent)]
    (replace-within-line! result (long (.get result :line-no)) 0 orig-indent indent-str)
    (.put result :x new-indent)
    (.put result :indent-x new-indent)
    (.put result :indent-delta (+ (long (.get result :indent-delta)) delta))))

(defn- should-add-opener-indent? [^java.util.HashMap result ^java.util.HashMap opener]
  (not= (.get opener :indent-delta) (.get result :indent-delta)))

(defn- correct-indent! [^java.util.HashMap result]
  (let [orig-indent (long (.get result :x))
        opener ^java.util.HashMap (peek-stack (.get result :paren-stack) 0)
        min-indent (if opener (inc (long (.get opener :x))) 0)
        max-indent (if opener (long (.get opener :max-child-indent)) (long (.get result :max-indent)))
        new-indent-raw (if (and opener (should-add-opener-indent? result opener))
                         (+ orig-indent (long (.get opener :indent-delta)))
                         orig-indent)
        clamped (clamp-val new-indent-raw min-indent max-indent)]
    (when-not (= clamped orig-indent)
      (add-indent! result (- clamped orig-indent)))))

(defn- on-indent! [^java.util.HashMap result]
  (.put result :indent-x (.get result :x))
  (.put result :tracking-indent false)
  (when (boolean (.get result :quote-danger))
    (throw (create-error result ERROR_QUOTE_DANGER)))
  (cond
    (= (.get result :mode) INDENT_MODE)
    (do
      (correct-paren-trail! result (long (.get result :x)))
      (when-let [opener ^java.util.HashMap (peek-stack (.get result :paren-stack) 0)]
        (when (should-add-opener-indent? result opener)
          (add-indent! result (long (.get opener :indent-delta))))))

    (= (.get result :mode) PAREN_MODE)
    (correct-indent! result)))

(defn- on-leading-close-paren! [^java.util.HashMap result]
  (when (= (.get result :mode) INDENT_MODE)
    (when-not (boolean (.get result :force-balance))
      (if (boolean (.get result :smart))
        (throw (exit-to-paren-mode "leadingCloseParen"))
        (when-not (.containsKey ^java.util.HashMap (.get result :error-pos-cache) ERROR_LEADING_CLOSE_PAREN)
          (cache-error-pos! result ERROR_LEADING_CLOSE_PAREN))))
    (.put result :skip-char true))

  (when (= (.get result :mode) PAREN_MODE)
    (cond
      (not (valid-close-paren? (.get result :paren-stack) (.get result :ch)))
      (if (boolean (.get result :smart))
        (.put result :skip-char true)
        (throw (create-error result ERROR_UNMATCHED_CLOSE_PAREN)))

      (cursor-left-of? (long (.get result :cursor-x)) (long (.get result :cursor-line))
                       (long (.get result :x)) (long (.get result :line-no)))
      (do
        (reset-paren-trail! result (.get result :line-no) (.get result :x))
        (on-indent! result))

      :else
      (do
        (append-paren-trail! result)
        (.put result :skip-char true)))))

(defn- on-comment-line! [^java.util.HashMap result]
  (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
        trail ^java.util.HashMap (.get result :paren-trail)
        openers ^java.util.ArrayList (.get trail :openers)
        trail-len (.size openers)]
    (when (= (.get result :mode) PAREN_MODE)
      (dotimes [i trail-len]
        (stack-push! paren-stack (peek-stack openers i))))
    (let [opener-idx (get-parent-opener-index result (long (.get result :x)))
          opener ^java.util.HashMap (peek-stack paren-stack opener-idx)]
      (when opener
        (when (should-add-opener-indent? result opener)
          (add-indent! result (long (.get opener :indent-delta))))))
    (when (= (.get result :mode) PAREN_MODE)
      (dotimes [_ trail-len]
        (stack-pop! paren-stack)))))

(defn- check-indent! [^java.util.HashMap result]
  (let [ch ^String (.get result :ch)]
    (cond
      (close-paren? ch (.get result :close-paren-chars))
      (on-leading-close-paren! result)

      (comment-char? ch (.get result :comment-chars))
      (do
        (on-comment-line! result)
        (.put result :tracking-indent false))

      (and (not= ch NEWLINE) (not= ch BLANK_SPACE) (not= ch TAB))
      (on-indent! result))))

;; -----------------------------------------------------------------------------
;; Tab Stops Calculation

(defn- set-tab-stops! [^java.util.HashMap result]
  (let [tab-stop-line (if (not= (.get result :selection-start-line) UINT_NULL)
                        (long (.get result :selection-start-line))
                        (long (.get result :cursor-line)))]
    (when (= tab-stop-line (long (.get result :line-no)))
      (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)
            tab-stops ^java.util.ArrayList (.get result :tab-stops)]
        (doseq [opener paren-stack]
          (let [ts (doto (java.util.HashMap.)
                     (.put :ch (.get ^java.util.HashMap opener :ch))
                     (.put :x (.get ^java.util.HashMap opener :x))
                     (.put :line-no (.get ^java.util.HashMap opener :line-no)))]
            (when (and (.containsKey ^java.util.HashMap opener :arg-x)
                       (>= (long (.get ^java.util.HashMap opener :arg-x)) 0))
              (.put ts :arg-x (.get ^java.util.HashMap opener :arg-x)))
            (stack-push! tab-stops ts)))

        (when (= (.get result :mode) PAREN_MODE)
          (let [trail-openers ^java.util.ArrayList (.get ^java.util.HashMap (.get result :paren-trail) :openers)
                n (.size trail-openers)]
            (loop [i (dec n)]
              (when (>= i 0)
                (let [opener ^java.util.HashMap (.get trail-openers i)
                      ts (doto (java.util.HashMap.)
                           (.put :ch (.get opener :ch))
                           (.put :x (.get opener :x))
                           (.put :line-no (.get opener :line-no)))]
                  (when (and (.containsKey opener :arg-x) (>= (long (.get opener :arg-x)) 0))
                    (.put ts :arg-x (.get opener :arg-x)))
                  (stack-push! tab-stops ts)
                  (recur (dec i)))))))

        ;; Remove argX if it falls to the right of the next stop
        (let [ts-len (.size tab-stops)]
          (loop [i 1]
            (when (< i ts-len)
              (let [curr-x (long (.get ^java.util.HashMap (.get tab-stops i) :x))
                    prev-ts ^java.util.HashMap (.get tab-stops (dec i))]
                (when (.containsKey prev-ts :arg-x)
                  (let [prev-arg-x (long (.get prev-ts :arg-x))]
                    (when (>= prev-arg-x curr-x)
                      (.remove prev-ts :arg-x)))))
              (recur (inc i)))))))))

;; -----------------------------------------------------------------------------
;; High-level Text Processing

(defn- process-char! [^java.util.HashMap result ^String ch]
  (let [orig-ch ch]
    (.put result :ch ch)
    (.put result :skip-char false)

    (handle-change-delta! result)

    (when (boolean (.get result :tracking-indent))
      (check-indent! result))

    (if (boolean (.get result :skip-char))
      (.put result :ch "")
      (on-char! result))

    (commit-char! result orig-ch)))

(defn- process-line! [^java.util.HashMap result ^long line-no]
  (init-line! result)
  (let [input-lines ^"[Ljava.lang.String;" (.get result :input-lines)
        line (aget input-lines (int line-no))
        lines ^java.util.ArrayList (.get result :lines)]
    (stack-push! lines line)
    (set-tab-stops! result)
    (let [line-len (.length line)]
      (loop [x 0]
        (when (< x line-len)
          (.put result :input-x (long x))
          (let [ch (str (.charAt line x))]
            (process-char! result ch))
          (recur (inc x)))))
    (process-char! result NEWLINE)

    (when-not (boolean (.get result :force-balance))
      (let [cache ^java.util.HashMap (.get result :error-pos-cache)
            unmatched-pos ^java.util.HashMap (.get cache ERROR_UNMATCHED_CLOSE_PAREN)
            trail ^java.util.HashMap (.get result :paren-trail)]
        (when (and unmatched-pos (< (long (.get unmatched-pos :x)) (long (.get trail :start-x))))
          (throw (create-error result ERROR_UNMATCHED_CLOSE_PAREN))))
      (let [cache ^java.util.HashMap (.get result :error-pos-cache)
            leading-pos ^java.util.HashMap (.get cache ERROR_LEADING_CLOSE_PAREN)
            trail ^java.util.HashMap (.get result :paren-trail)]
        (when (and leading-pos (= (long (.get trail :line-no)) (long (.get result :line-no))))
          (throw (create-error result ERROR_LEADING_CLOSE_PAREN)))))

    (when (= (long (.get result :line-no)) (long (.get ^java.util.HashMap (.get result :paren-trail) :line-no)))
      (finish-new-paren-trail! result))))

(defn- finalize-result! [^java.util.HashMap result]
  (when (boolean (.get result :quote-danger))
    (throw (create-error result ERROR_QUOTE_DANGER)))
  (when (boolean (.get result :is-in-str))
    (throw (create-error result ERROR_UNCLOSED_QUOTE)))
  (let [paren-stack ^java.util.ArrayList (.get result :paren-stack)]
    (when (not (.isEmpty paren-stack))
      (when (= (.get result :mode) PAREN_MODE)
        (throw (create-error result ERROR_UNCLOSED_PAREN)))))
  (when (= (.get result :mode) INDENT_MODE)
    (init-line! result)
    (on-indent! result))
  (.put result :success true))

(defn- process-text [^String text options mode smart]
  (let [result (get-initial-result text options mode smart)]
    (try
      (let [input-lines ^"[Ljava.lang.String;" (.get result :input-lines)
            n (alength input-lines)]
        (loop [i 0]
          (when (< i n)
            (.put result :input-line-no (long i))
            (process-line! result (long i))
            (recur (inc i))))
        (finalize-result! result)
        result)
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (if (:exit-to-paren-mode d)
            (process-text text options PAREN_MODE smart)
            (do
              (if-let [err (:error d)]
                (do
                  (.put result :success false)
                  (.remove ^java.util.HashMap err :parinfer-error)
                  (.put result :error err))
                (throw e))
              result))))
      (catch Throwable t
        (.put result :success false)
        (let [err (doto (java.util.HashMap.)
                    (.put :name ERROR_UNHANDLED)
                    (.put :message (.getMessage t)))]
          (.put result :error err))
        result))))

;; -----------------------------------------------------------------------------
;; Public Result Format

(defn- map-to-clj [m]
  (when m
    (into {} (for [[k v] m]
               [k (cond
                    (instance? java.util.Map v) (map-to-clj v)
                    (instance? java.util.List v) (mapv #(if (instance? java.util.Map %) (map-to-clj %) %) v)
                    :else v)]))))

(defn- public-result [^java.util.HashMap result]
  (let [line-ending (get-line-ending (.get result :orig-text))
        success? (boolean (.get result :success))
        final-map (java.util.HashMap.)]
    (if success?
      (let [lines ^java.util.ArrayList (.get result :lines)]
        (.put final-map :text (str/join line-ending lines))
        (.put final-map :cursor-x (.get result :cursor-x))
        (.put final-map :cursor-line (.get result :cursor-line))
        (.put final-map :success true)
        (.put final-map :tab-stops (mapv map-to-clj (.get result :tab-stops)))
        (.put final-map :paren-trails (vec (.get result :paren-trails)))
        (when (boolean (.get result :return-parens))
          (.put final-map :parens (mapv map-to-clj (.get result :parens)))))
      (do
        (.put final-map :success false)
        (.put final-map :error (map-to-clj (.get result :error)))
        (if (boolean (.get result :partial-result))
          (let [lines ^java.util.ArrayList (.get result :lines)]
            (.put final-map :text (str/join line-ending lines))
            (.put final-map :cursor-x (.get result :cursor-x))
            (.put final-map :cursor-line (.get result :cursor-line))
            (.put final-map :paren-trails (vec (.get result :paren-trails))))
          (do
            (.put final-map :text (.get result :orig-text))
            (.put final-map :cursor-x (.get result :orig-cursor-x))
            (.put final-map :cursor-line (.get result :orig-cursor-line))
            (.put final-map :paren-trails nil)))
        (when (and (boolean (.get result :partial-result)) (boolean (.get result :return-parens)))
          (.put final-map :parens (mapv map-to-clj (.get result :parens))))))

    (when (= (.get final-map :cursor-x) UINT_NULL) (.remove final-map :cursor-x))
    (when (= (.get final-map :cursor-line) UINT_NULL) (.remove final-map :cursor-line))
    (when (empty? (.get final-map :tab-stops)) (.remove final-map :tab-stops))

    (let [res (into {} final-map)]
      ;; Provide both kebab-case and camelCase keys for seamless interoperability
      (cond-> res
        (contains? res :cursor-x) (assoc :cursorX (:cursor-x res))
        (contains? res :cursor-line) (assoc :cursorLine (:cursor-line res))
        (contains? res :tab-stops) (assoc :tabStops (:tab-stops res))
        (contains? res :paren-trails) (assoc :parenTrails (:paren-trails res))))))

;; -----------------------------------------------------------------------------
;; Public API

(defn indent-mode
  "Runs Parinfer in Indent Mode.
   Infers parenthesis balance and positions from line indentation."
  ([text]
   (indent-mode text nil))
  ([text options]
   (public-result (process-text text options INDENT_MODE false))))

(defn paren-mode
  "Runs Parinfer in Paren Mode.
   Infers code indentation from existing parenthesis structure."
  ([text]
   (paren-mode text nil))
  ([text options]
   (public-result (process-text text options PAREN_MODE false))))

(defn smart-mode
  "Runs Parinfer in Smart Mode.
   Preserves structure while you type, automatically deciding whether to update
   indentation or parentheses based on your edits."
  ([text]
   (smart-mode text nil))
  ([text options]
   (let [smart (nil? (or (:selection-start-line options)
                         (:selectionStartLine options)))]
     (public-result (process-text text options INDENT_MODE smart)))))

;; Aliases matching JS camelCase API
(def indentMode indent-mode)
(def parenMode paren-mode)
(def smartMode smart-mode)

;; -----------------------------------------------------------------------------
;; Editor Integration Helpers

(defn offset->line-col
  "Converts a 0-based character offset in `text` to `[line-no x]` (0-based)."
  [^String text ^long offset]
  (if (or (nil? text) (not (pos? (.length text))))
    [0 0]
    (let [safe-off (int (max 0 (min (int offset) (.length text))))
          prefix (.substring text 0 safe-off)
          line-no (count (re-seq #"\n" prefix))
          last-nl (.lastIndexOf prefix "\n")
          x (if (neg? last-nl) safe-off (- safe-off (inc last-nl)))]
      [line-no x])))

(defn line-col->offset
  "Converts 0-based `[line-no x]` in `text` to character offset."
  [^String text ^long line-no ^long x]
  (if (or (nil? text) (not (pos? (.length text))))
    0
    (let [lines (str/split text #"\r?\n" -1)
          target-line (int (max 0 (min (int line-no) (dec (count lines)))))]
      (loop [l 0, off 0]
        (if (= l target-line)
          (let [line-len (.length ^String (nth lines l))]
            (+ off (int (max 0 (min (int x) line-len)))))
          (let [line-len (.length ^String (nth lines l))
                ending-len (if (and (< (+ off line-len) (.length text))
                                    (= \return (.charAt text (+ off line-len))))
                             2
                             1)]
            (recur (inc l) (+ off line-len ending-len))))))))

(defn compute-text-change
  "Computes change map `{:line-no :x :old-text :new-text}` between `old-text` and `new-text`."
  [^String old-text ^String new-text]
  (let [old-s (or old-text "")
        new-s (or new-text "")]
    (when (not= old-s new-s)
      (let [old-len (.length old-s)
            new-len (.length new-s)
            max-prefix (min old-len new-len)
            prefix-len (loop [i 0]
                         (if (and (< i max-prefix)
                                  (= (.charAt old-s i) (.charAt new-s i)))
                           (recur (inc i))
                           i))
            max-suffix (- (min old-len new-len) prefix-len)
            suffix-len (loop [i 0]
                         (if (and (< i max-suffix)
                                  (= (.charAt old-s (- old-len 1 i))
                                     (.charAt new-s (- new-len 1 i))))
                           (recur (inc i))
                           i))
            old-change (.substring old-s prefix-len (- old-len suffix-len))
            new-change (.substring new-s prefix-len (- new-len suffix-len))
            prefix-str (.substring old-s 0 prefix-len)
            line-no (count (re-seq #"\n" prefix-str))
            last-nl (.lastIndexOf prefix-str "\n")
            x (if (neg? last-nl) prefix-len (- prefix-len (inc last-nl)))]
        {:line-no line-no
         :lineNo line-no
         :x x
         :old-text old-change
         :oldText old-change
         :new-text new-change
         :newText new-change}))))

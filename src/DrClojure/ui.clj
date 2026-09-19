(ns DrClojure.ui
  "DrClojure Swing User Interface.
   Implements a DrRacket-inspired dual-pane layout with:
   - Definitions editor (top) with line numbers, bracket matching, undo/redo, 2-space tabs.
   - Interactions console (bottom) with live stdout, GUI stdin, REPL history, and Stop button.
   - Clojure cheatsheet, toolbar, status bar, and unsaved changes confirmation."
  (:require [clojure.string :as str]
            [DrClojure.eval :as eval]
            [DrClojure.syntax :as syntax])
  (:import (javax.swing JFrame JPanel JSplitPane JScrollPane JTextArea JTextField JTextPane
                        JButton JLabel JMenuBar JMenu JMenuItem KeyStroke
                        JFileChooser JOptionPane JToolBar BorderFactory Box
                        SwingUtilities UIManager JDialog JViewport)
           (javax.swing.event DocumentListener CaretListener UndoableEditListener DocumentEvent$EventType)
           (javax.swing.text DefaultHighlighter$DefaultHighlightPainter JTextComponent
                             DefaultStyledDocument AbstractDocument$DefaultDocumentEvent)
           (java.awt BorderLayout FlowLayout Dimension Font Color Insets
                     KeyboardFocusManager Toolkit Desktop Desktop$Action)
           (java.net URI)
           (java.awt.event ActionEvent ActionListener KeyEvent KeyAdapter
                           WindowAdapter WindowEvent)))

(def app-name "DrClojure")
(def app-version "0.3.0")
(def cheatsheet-url "https://clojure.org/api/cheatsheet")

(defn open-browser! [^String url]
  (try
    (if (and (Desktop/isDesktopSupported)
             (.. Desktop getDesktop (isSupported Desktop$Action/BROWSE)))
      (.. Desktop getDesktop (browse (URI. url)))
      (let [os (str/lower-case (System/getProperty "os.name" ""))]
        (cond
          (str/includes? os "win")
          (.. Runtime getRuntime (exec (into-array String ["rundll32" "url.dll,FileProtocolHandler" url])))
          (str/includes? os "mac")
          (.. Runtime getRuntime (exec (into-array String ["open" url])))
          :else
          (.. Runtime getRuntime (exec (into-array String ["xdg-open" url]))))))
    (catch Exception ex
      (JOptionPane/showMessageDialog nil
        (str "Failed to open web browser:\n" (.getMessage ex) "\n\nPlease visit:\n" url)
        "Error Opening Link" JOptionPane/ERROR_MESSAGE))))

(defn open-cheatsheet! []
  (open-browser! cheatsheet-url))

;; --- Bracket Matching ---

(defn find-matching-bracket
  "Finds the matching bracket position for the bracket at `pos` in `text`.
   Uses lexical tokenization so brackets inside comments, strings, regexes,
   and character literals are completely ignored. Returns nil if unmatched."
  [^String text pos]
  (when (and (string? text) (<= 0 pos (dec (.length text))))
    (get (:matches (syntax/compute-brackets text)) (long pos))))

(defn setup-bracket-matching!
  "Highlights matching parentheses/brackets at the caret position.
   Uses syntax tokenization so brackets inside strings/comments are ignored.
   Matched pairs are highlighted with warm amber, and unmatched brackets
   are highlighted with soft red."
  ([^JTextComponent editor]
   (setup-bracket-matching! editor nil))
  ([^JTextComponent editor bracket-info-atom]
   (let [match-painter (DefaultHighlighter$DefaultHighlightPainter. (Color. 255 220 100))
         unmatched-painter (DefaultHighlighter$DefaultHighlightPainter. (Color. 255 180 180))
         tag1 (atom nil)
         tag2 (atom nil)
         hl (.getHighlighter editor)
         clear-highlights! (fn []
                             (when-let [t @tag1] (.removeHighlight hl t) (reset! tag1 nil))
                             (when-let [t @tag2] (.removeHighlight hl t) (reset! tag2 nil)))]
     (.addCaretListener editor
       (proxy [CaretListener] []
         (caretUpdate [e]
           (clear-highlights!)
           (let [caret (.getCaretPosition editor)
                 doc-text (.getText editor)
                 len (.length doc-text)]
             (when (pos? len)
               (let [binfo (or (and bracket-info-atom @bracket-info-atom)
                               (syntax/compute-brackets doc-text))
                     matches (:matches binfo)
                     unmatched (:unmatched binfo)
                     is-bracket? (fn [idx]
                                   (let [l (long idx)]
                                     (or (contains? matches l) (contains? unmatched l))))
                     check-pos (cond
                                 (and (> caret 0) (is-bracket? (dec caret))) (long (dec caret))
                                 (and (< caret len) (is-bracket? caret)) (long caret)
                                 :else nil)]
                 (when-let [pos check-pos]
                   (if-let [match-pos (get matches pos)]
                     (do
                       (reset! tag1 (.addHighlight hl (int pos) (inc (int pos)) match-painter))
                       (reset! tag2 (.addHighlight hl (int match-pos) (inc (int match-pos)) match-painter)))
                     (when (contains? unmatched pos)
                       (reset! tag1 (.addHighlight hl (int pos) (inc (int pos)) unmatched-painter))))))))))))))

;; --- Editor Helpers: Tabs, Undo, Line Numbers ---

(defn update-line-numbers! [^JTextComponent editor ^JTextArea line-numbers]
  (let [root (.. editor getDocument getDefaultRootElement)
        count (.getElementCount root)
        sb (StringBuilder.)]
    (dotimes [i count]
      (.append sb (format "%4d \n" (inc i))))
    (when-not (= (.getText line-numbers) (str sb))
      (.setText line-numbers (str sb)))))

(defn setup-tab-key! [^JTextComponent editor]
  (.setFocusTraversalKeys editor KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS java.util.Collections/EMPTY_SET)
  (.setFocusTraversalKeys editor KeyboardFocusManager/BACKWARD_TRAVERSAL_KEYS java.util.Collections/EMPTY_SET)
  (let [im (.getInputMap editor)
        am (.getActionMap editor)]
    (.put im (KeyStroke/getKeyStroke "TAB") "insert-indent")
    (.put am "insert-indent"
      (proxy [javax.swing.AbstractAction] []
        (actionPerformed [e]
          (let [start (.getSelectionStart editor)
                end (.getSelectionEnd editor)]
            (if (= start end)
              (.replaceSelection editor "  ")
              (let [selected (.getSelectedText editor)
                    indented (str/replace selected #"(?m)^" "  ")]
                (.replaceSelection editor indented)
                (.setSelectionStart editor start)
                (.setSelectionEnd editor (+ start (count indented)))))))))))

(defn setup-undo! [^JTextComponent editor]
  (let [undo-mgr (javax.swing.undo.UndoManager.)
        im (.getInputMap editor)
        am (.getActionMap editor)]
    (.. editor getDocument
        (addUndoableEditListener
          (reify UndoableEditListener
            (undoableEditHappened [_ e]
              (let [edit (.getEdit e)]
                ;; Filter out style-only attribute changes from undo history
                (when-not (and (instance? AbstractDocument$DefaultDocumentEvent edit)
                               (= (.getType ^AbstractDocument$DefaultDocumentEvent edit)
                                  DocumentEvent$EventType/CHANGE))
                  (.addEdit undo-mgr edit)))))))
    (.put im (KeyStroke/getKeyStroke "control Z") "undo")
    (.put am "undo"
      (proxy [javax.swing.AbstractAction] []
        (actionPerformed [e]
          (when (.canUndo undo-mgr) (.undo undo-mgr)))))
    (.put im (KeyStroke/getKeyStroke "control Y") "redo")
    (.put im (KeyStroke/getKeyStroke "control shift Z") "redo")
    (.put am "redo"
      (proxy [javax.swing.AbstractAction] []
        (actionPerformed [e]
          (when (.canRedo undo-mgr) (.redo undo-mgr)))))
    undo-mgr))

;; --- Clojure Cheatsheet Dialog ---

(def cheatsheet-data
  [["Basics"
    [["(def name val)" "Defines a global variable in the current namespace." "(def x 10)"]
     ["(defn name [params] body)" "Defines a function." "(defn square [n]\n  (* n n))"]
     ["(let [bindings] body)" "Evaluates body with local variables." "(let [a 5\n      b 10]\n  (+ a b))"]
     ["(fn [params] body)" "Anonymous function (lambda)." "((fn [x] (* x 2)) 21)"]]]
   ["Console I/O & Stdin"
    [["(println ...)" "Prints text followed by a newline to console." "(println \"Hello, DrClojure!\")"]
     ["(read-line)" "Reads a line of input from GUI stdin." "(println \"What is your name?\")\n(let [name (read-line)]\n  (println (str \"Nice to meet you, \" name \"!\")))"]
     ["(prn ...)" "Prints Clojure data structure for reading." "(prn {:name \"Alice\" :scores [95 88 92]})"]]]
   ["Control Flow"
    [["(if test then else)" "Conditional branch." "(if (> 10 5)\n  \"greater\"\n  \"smaller\")"]
     ["(when test body...)" "Executes body if test is true, returns nil otherwise." "(when true\n  (println \"Running\")\n  :ok)"]
     ["(cond & clauses)" "Multi-branch condition." "(let [score 85]\n  (cond\n    (>= score 90) \"A\"\n    (>= score 80) \"B\"\n    :else \"C\"))"]
     ["(dotimes [i n] body)" "Repeats body n times with index 0 to n-1." "(dotimes [i 5]\n  (println \"Count:\" i))"]]]
   ["Collections & Sequences"
    [["(map f coll)" "Applies f to each element of coll." "(map inc [1 2 3 4 5])"]
     ["(filter pred coll)" "Filters elements satisfying predicate." "(filter even? (range 10))"]
     ["(reduce f init coll)" "Reduces collection to a single value." "(reduce + 0 [1 2 3 4 5])"]
     ["(assoc map key val)" "Adds or updates key-value pair in map." "(assoc {:a 1} :b 2)"]
     ["(get map key [default])" "Looks up value by key in map." "(get {:name \"Alice\"} :name)"]]]
   ["State (Atoms)"
    [["(atom init)" "Creates an atomic mutable reference." "(def count (atom 0))"]
     ["@atom / (deref atom)" "Reads the current value of atom." "@count"]
     ["(swap! atom f & args)" "Atomically updates atom by applying f." "(swap! count inc)"]
     ["(reset! atom new-val)" "Sets atom to a new value unconditionally." "(reset! count 0)"]]]
   ["REPL Helpers"
    [["(doc fn-name)" "Prints documentation for a function." "(doc map)"]
     ["(source fn-name)" "Prints source code of a function." "(source identity)"]
     ["(apropos \"pattern\")" "Searches symbols matching pattern." "(apropos \"sort\")"]]]])

(defn show-cheatsheet-dialog! [^JFrame parent-frame ^JTextComponent editor]
  (let [dialog (JDialog. parent-frame "Clojure Cheatsheet & Quick Reference" false)
        panel (JPanel. (BorderLayout. 8 8))
        text-area (JTextArea. 24 60)
        _ (do (.setEditable text-area false)
              (.setFont text-area (Font. "Consolas" Font/PLAIN 13)))
        sb (StringBuilder.)
        _ (doseq [[category items] cheatsheet-data]
            (.append sb (str "\n;; === " category " ===\n\n"))
            (doseq [[syntax desc example] items]
              (.append sb (str ";; " syntax "  ;  " desc "\n"))
              (.append sb (str ";; Example:\n" example "\n\n"))))
        _ (.setText text-area (str sb))
        _ (.setCaretPosition text-area 0)
        scroll (JScrollPane. text-area)
        btn-panel (JPanel. (FlowLayout. FlowLayout/RIGHT))
        btn-insert (JButton. "Insert Example at Cursor")
        btn-close (JButton. "Close")]
    (.addActionListener btn-insert
      (proxy [ActionListener] []
        (actionPerformed [e]
          (let [sel (.getSelectedText text-area)]
            (if (not (str/blank? sel))
              (do
                (.replaceSelection editor sel)
                (.requestFocus editor))
              (JOptionPane/showMessageDialog dialog
                "Select an example code snippet from the text above, then click Insert."
                "Tip" JOptionPane/INFORMATION_MESSAGE))))))
    (.addActionListener btn-close
      (proxy [ActionListener] []
        (actionPerformed [e] (.dispose dialog))))
    (.add btn-panel btn-insert)
    (.add btn-panel btn-close)
    (.setBorder panel (BorderFactory/createEmptyBorder 10 10 10 10))
    (.add panel scroll BorderLayout/CENTER)
    (.add panel btn-panel BorderLayout/SOUTH)
    (.setContentPane dialog panel)
    (.pack dialog)
    (.setLocationRelativeTo dialog parent-frame)
    (.setVisible dialog true)))

;; --- Main UI Builder ---

(defn create-ide [initial-file]
  (let [init-content (if (and (not (str/blank? initial-file)) (.exists (java.io.File. initial-file)))
                       (try (slurp initial-file) (catch Exception _ ""))
                       "")
        frame (JFrame. (str "Untitled - " app-name))
        eval-ctx (eval/make-eval-context)
        cur-file (atom (or initial-file ""))
        saved-content (atom init-content)
        dirty? (atom false)
        font-size (atom 14)

        ;; Components: Editor (JTextPane with horizontal scroll support) & Line numbers
        doc (DefaultStyledDocument.)
        editor (proxy [JTextPane] [doc]
                 (getScrollableTracksViewportWidth []
                   (let [parent (.getParent this)]
                     (if (instance? JViewport parent)
                       (>= (.getWidth parent) (.. this getUI (getPreferredSize this) width))
                       true))))
        _ (do (.setMargin editor (Insets. 2 4 2 4))
              (.setBackground editor Color/WHITE)
              (.setCaretColor editor (Color. 30 30 30))
              (when-not (empty? init-content)
                (.setText editor init-content)))
        line-numbers (JTextArea. 16 5)
        _ (do (.setEditable line-numbers false)
              (.setFocusable line-numbers false)
              (.setBackground line-numbers (Color. 242 242 242))
              (.setForeground line-numbers (Color. 130 130 130))
              (.setBorder line-numbers (BorderFactory/createEmptyBorder 2 4 2 6)))
        editor-scroll (JScrollPane. editor)
        _ (.setRowHeaderView editor-scroll line-numbers)
        syntax-controller (syntax/setup-syntax-highlighting! editor {:delay-ms 60})
        highlight-now! (:highlight-now! syntax-controller)
        bracket-info (:bracket-info syntax-controller)

        ;; Components: Output & REPL input
        output-area (JTextArea. 10 80)
        _ (do (.setEditable output-area false)
              (.setBackground output-area (Color. 250 250 250))
              (.setForeground output-area (Color. 30 30 30)))
        output-scroll (JScrollPane. output-area)
        prompt-label (JLabel. " > ")
        _ (.setFont prompt-label (Font. "Consolas" Font/BOLD 14))
        input-field (JTextField.)
        prompt-panel (JPanel. (BorderLayout. 4 0))
        _ (do (.add prompt-panel prompt-label BorderLayout/WEST)
              (.add prompt-panel input-field BorderLayout/CENTER))
        interactions-panel (JPanel. (BorderLayout. 0 4))
        _ (do (.add interactions-panel output-scroll BorderLayout/CENTER)
              (.add interactions-panel prompt-panel BorderLayout/SOUTH))

        ;; Split Pane: Top = Definitions, Bottom = Interactions
        split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT editor-scroll interactions-panel)
        _ (do (.setResizeWeight split-pane 0.6)
              (.setDividerLocation split-pane 380))

        ;; Status Bar
        status-panel (JPanel. (BorderLayout.))
        status-caret (JLabel. " Line 1, Col 1 ")
        status-state (JLabel. " ● Ready ")
        _ (do (.setForeground status-state (Color. 0 130 0))
              (.setFont status-state (Font. "Segoe UI" Font/BOLD 12))
              (.setBorder status-panel (BorderFactory/createEmptyBorder 3 6 3 6))
              (.add status-panel status-caret BorderLayout/WEST)
              (.add status-panel status-state BorderLayout/EAST))

        ;; Toolbar Buttons
        btn-run (JButton. "▶ Run (Ctrl+Enter)")
        btn-stop (JButton. "⏹ Stop")
        _ (.setEnabled btn-stop false)
        btn-clear (JButton. "Clear Output (Ctrl+L)")
        btn-cheatsheet (JButton. "Cheatsheet (F1)")
        btn-font-plus (JButton. "A+")
        btn-font-minus (JButton. "A-")

        ;; File Chooser
        fc (JFileChooser.)
        _ (.setFileFilter fc (javax.swing.filechooser.FileNameExtensionFilter. "Clojure files (*.clj, *.cljc, *.edn)" (into-array ["clj" "cljc" "edn"])))

        undo-mgr (setup-undo! editor)]

    ;; --- Setup Fonts ---
    (letfn [(apply-font! [sz]
              (let [f (Font. "Consolas" Font/PLAIN sz)
                    fb (Font. "Consolas" Font/BOLD sz)]
                (.setFont editor f)
                (.setFont line-numbers f)
                (.setFont output-area f)
                (.setFont input-field f)
                (.setFont prompt-label fb)
                (update-line-numbers! editor line-numbers)))]
      (apply-font! @font-size)

      ;; --- Title and Filename Updater ---
      (letfn [(update-title! []
                (let [fname (if (empty? @cur-file) "Untitled" @cur-file)
                      short-name (if (empty? @cur-file) "Untitled" (last (str/split @cur-file #"[/\\]")))
                      star (if @dirty? "* " "")]
                  (.setTitle frame (str star short-name " - " app-name " (" fname ")"))))

              (update-dirty! []
                (let [is-dirty? (not= (.getText editor) @saved-content)]
                  (when (not= @dirty? is-dirty?)
                    (reset! dirty? is-dirty?)
                    (update-title!))))

              (append-output! [text]
                (SwingUtilities/invokeLater
                  (fn []
                    (.append output-area text)
                    (.setCaretPosition output-area (.. output-area getDocument getLength)))))]

        ;; Startup Banner
        (letfn [(print-banner! []
                  (let [c-ver (clojure-version)
                        j-ver (System/getProperty "java.version")]
                    (.setText output-area "")
                    (append-output!
                      (str "============================================================\n"
                           " " app-name " " app-version "  |  Clojure " c-ver "  |  Java " j-ver "\n"
                           "============================================================\n"
                           " Definitions (Top): Type Clojure code and press [Run] or Ctrl+Enter\n"
                           " Interactions (Bottom): REPL expressions & GUI standard input\n"
                           " Stdin Console: Use (read-line) in code; input is prompted below\n"
                           " Cheatsheet: Click [Cheatsheet] or press F1 to open Clojure Cheatsheet in browser\n"
                           "------------------------------------------------------------\n\n"))))]
          (print-banner!)

          ;; Status Updater
          (letfn [(set-status! [st]
                    (SwingUtilities/invokeLater
                      (fn []
                        (case st
                          :running
                          (do (.setText status-state " ● Running... ")
                              (.setForeground status-state (Color. 200 100 0))
                              (.setEnabled btn-run false)
                              (.setEnabled btn-stop true))
                          :waiting-input
                          (do (.setText status-state " ● Waiting for input... ")
                              (.setForeground status-state (Color. 0 100 220))
                              (.setText prompt-label " [stdin] > ")
                              (.setForeground prompt-label (Color. 0 100 220))
                              (.requestFocusInWindow input-field))
                          :idle
                          (do (.setText status-state " ● Ready ")
                              (.setForeground status-state (Color. 0 130 0))
                              (.setText prompt-label " > ")
                              (.setForeground prompt-label (Color. 0 0 0))
                              (.setEnabled btn-run true)
                              (.setEnabled btn-stop false))))))]

            ;; --- Evaluation Logic ---
            (letfn [(run-code-string! [code-str]
                      (eval/eval-async eval-ctx code-str
                        {:on-output append-output!
                         :on-status-change set-status!
                         :on-complete (fn [res]
                                        (case (:status res)
                                          :ok
                                          (append-output! (str "=> " (eval/format-value (:value res)) "\n"))
                                          :error
                                          (append-output! (str (eval/format-error (:error res)) "\n"))
                                          :interrupted
                                          (append-output! "; [Evaluation stopped by user]\n")
                                          nil))}))

                    (run-definitions! []
                      (let [code (.getText editor)]
                        (append-output! "\n; --- Running Definitions ---\n")
                        (run-code-string! code)))

                    (run-selection! []
                      (let [sel (.getSelectedText editor)
                            code (if (not (str/blank? sel))
                                   sel
                                   (let [caret (.getCaretPosition editor)
                                         root (.. editor getDocument getDefaultRootElement)
                                         line-idx (.getElementIndex root caret)
                                         line-elem (.getElement root line-idx)
                                         start (.getStartOffset line-elem)
                                         end (.getEndOffset line-elem)]
                                     (.getText editor start (- end start))))]
                        (when-not (str/blank? code)
                          (append-output! (str "\n; --- Running Selection ---\n" code "\n"))
                          (run-code-string! code))))

                    (stop-current-eval! []
                      (eval/stop-eval! eval-ctx)
                      (set-status! :idle))]

              ;; --- Input Field Action (REPL & Stdin) ---
              (.addActionListener input-field
                (proxy [ActionListener] []
                  (actionPerformed [e]
                    (let [text (.getText input-field)]
                      (.setText input-field "")
                      (if (eval/waiting-for-input? eval-ctx)
                        ;; Process GUI stdin input
                        (do
                          (append-output! (str text "\n"))
                          (eval/push-stdin! eval-ctx text))
                        ;; Process REPL expression
                        (if (eval/evaluating? eval-ctx)
                          (append-output! "; [Warning: Code is currently running. Click Stop to cancel.]\n")
                          (when-not (str/blank? text)
                            (swap! (:history eval-ctx) eval/history-add text)
                            (append-output! (str "> " text "\n"))
                            (run-code-string! text))))))))

              ;; History navigation on Up / Down arrow
              (.addKeyListener input-field
                (proxy [KeyAdapter] []
                  (keyPressed [e]
                    (cond
                      (= (.getKeyCode e) KeyEvent/VK_UP)
                      (do
                        (.consume e)
                        (let [[new-hist display-text] (eval/history-prev @(:history eval-ctx) (.getText input-field))]
                          (reset! (:history eval-ctx) new-hist)
                          (.setText input-field display-text)))

                      (= (.getKeyCode e) KeyEvent/VK_DOWN)
                      (do
                        (.consume e)
                        (let [[new-hist display-text] (eval/history-next @(:history eval-ctx))]
                          (reset! (:history eval-ctx) new-hist)
                          (.setText input-field display-text)))))))

              ;; Wire toolbar buttons
              (.addActionListener btn-run (proxy [ActionListener] [] (actionPerformed [e] (run-definitions!))))
              (.addActionListener btn-stop (proxy [ActionListener] [] (actionPerformed [e] (stop-current-eval!))))
              (.addActionListener btn-clear (proxy [ActionListener] [] (actionPerformed [e] (print-banner!))))
              (.addActionListener btn-cheatsheet (proxy [ActionListener] [] (actionPerformed [e] (open-cheatsheet!))))
              (.addActionListener btn-font-plus
                (proxy [ActionListener] []
                  (actionPerformed [e]
                    (swap! font-size #(min 32 (+ % 2)))
                    (apply-font! @font-size))))
              (.addActionListener btn-font-minus
                (proxy [ActionListener] []
                  (actionPerformed [e]
                    (swap! font-size #(max 10 (- % 2)))
                    (apply-font! @font-size))))

              ;; Setup Editor Listeners
              (setup-bracket-matching! editor bracket-info)
              (setup-tab-key! editor)

              (.. editor getDocument (addDocumentListener
                (proxy [DocumentListener] []
                  (insertUpdate [e]
                    (update-dirty!)
                    (update-line-numbers! editor line-numbers))
                  (removeUpdate [e]
                    (update-dirty!)
                    (update-line-numbers! editor line-numbers))
                  (changedUpdate [e]
                    (update-line-numbers! editor line-numbers)))))

              (.addCaretListener editor
                (proxy [CaretListener] []
                  (caretUpdate [e]
                    (let [pos (.getCaretPosition editor)
                          root (.. editor getDocument getDefaultRootElement)
                          line (inc (.getElementIndex root pos))
                          start (.getStartOffset (.getElement root (dec line)))
                          col (inc (- pos start))]
                      (.setText status-caret (format " Line %d, Col %d " line col))))))

              ;; --- File Management & Dirty Checking ---
              (letfn [(save-file-to! [file-path]
                        (try
                          (let [text (.getText editor)]
                            (spit file-path text)
                            (reset! cur-file file-path)
                            (reset! saved-content text)
                            (reset! dirty? false)
                            (update-title!)
                            true)
                          (catch Exception ex
                            (JOptionPane/showMessageDialog frame
                              (str "Failed to save file:\n" (.getMessage ex))
                              "Error" JOptionPane/ERROR_MESSAGE)
                            false)))

                      (file-save-as! []
                        (when (= (.showSaveDialog fc frame) JFileChooser/APPROVE_OPTION)
                          (let [f (.getSelectedFile fc)
                                  path (let [p (.getCanonicalPath f)]
                                         (if (str/includes? p ".") p (str p ".clj")))]
                            (save-file-to! path))))

                      (file-save! []
                        (if (empty? @cur-file)
                          (file-save-as!)
                          (save-file-to! @cur-file)))

                      (prompt-save-if-dirty! []
                        (if @dirty?
                          (let [resp (JOptionPane/showConfirmDialog frame
                                       (str "Save changes to " (if (empty? @cur-file) "Untitled" @cur-file) "?")
                                       app-name JOptionPane/YES_NO_CANCEL_OPTION)]
                            (cond
                              (= resp JOptionPane/YES_OPTION) (if (file-save!) :proceed :cancel)
                              (= resp JOptionPane/NO_OPTION) :proceed
                              :else :cancel))
                          :proceed))

                      (file-new! []
                        (when (= (prompt-save-if-dirty!) :proceed)
                          (reset! cur-file "")
                          (reset! saved-content "")
                          (reset! dirty? false)
                          (.setText editor "")
                          (highlight-now!)
                          (update-title!)))

                      (file-open! []
                        (when (= (prompt-save-if-dirty!) :proceed)
                          (when (= (.showOpenDialog fc frame) JFileChooser/APPROVE_OPTION)
                            (let [f (.getSelectedFile fc)
                                  path (.getCanonicalPath f)]
                              (try
                                (let [content (slurp path)]
                                  (reset! cur-file path)
                                  (reset! saved-content content)
                                  (reset! dirty? false)
                                  (.setText editor content)
                                  (.setCaretPosition editor 0)
                                  (highlight-now!)
                                  (update-title!))
                                (catch Exception ex
                                  (JOptionPane/showMessageDialog frame
                                    (str "Failed to open file:\n" (.getMessage ex))
                                    "Error" JOptionPane/ERROR_MESSAGE)))))))

                      (file-exit! []
                        (when (= (prompt-save-if-dirty!) :proceed)
                          (.dispose frame)
                          (System/exit 0)))]

                ;; Highlight initial file if loaded
                (when-not (empty? init-content)
                  (highlight-now!))

                (update-title!)
                (update-line-numbers! editor line-numbers)

                ;; --- Menu Bar ---
                (let [menu-bar (JMenuBar.)
                      ;; File Menu
                      menu-file (JMenu. "File")
                      _ (.setMnemonic menu-file (int \F))
                      item-new (JMenuItem. "New" (int \N))
                      item-open (JMenuItem. "Open..." (int \O))
                      item-save (JMenuItem. "Save" (int \S))
                      item-save-as (JMenuItem. "Save As..." (int \A))
                      item-exit (JMenuItem. "Exit" (int \X))
                      _ (do (.setAccelerator item-new (KeyStroke/getKeyStroke "control N"))
                            (.setAccelerator item-open (KeyStroke/getKeyStroke "control O"))
                            (.setAccelerator item-save (KeyStroke/getKeyStroke "control S"))
                            (.setAccelerator item-save-as (KeyStroke/getKeyStroke "control shift S"))
                            (.addActionListener item-new (proxy [ActionListener] [] (actionPerformed [e] (file-new!))))
                            (.addActionListener item-open (proxy [ActionListener] [] (actionPerformed [e] (file-open!))))
                            (.addActionListener item-save (proxy [ActionListener] [] (actionPerformed [e] (file-save!))))
                            (.addActionListener item-save-as (proxy [ActionListener] [] (actionPerformed [e] (file-save-as!))))
                            (.addActionListener item-exit (proxy [ActionListener] [] (actionPerformed [e] (file-exit!))))
                            (.add menu-file item-new)
                            (.add menu-file item-open)
                            (.add menu-file item-save)
                            (.add menu-file item-save-as)
                            (.addSeparator menu-file)
                            (.add menu-file item-exit))

                      ;; Edit Menu
                      menu-edit (JMenu. "Edit")
                      _ (.setMnemonic menu-edit (int \E))
                      item-undo (JMenuItem. "Undo")
                      item-redo (JMenuItem. "Redo")
                      item-clear (JMenuItem. "Clear Output")
                      _ (do (.setAccelerator item-undo (KeyStroke/getKeyStroke "control Z"))
                            (.setAccelerator item-redo (KeyStroke/getKeyStroke "control Y"))
                            (.setAccelerator item-clear (KeyStroke/getKeyStroke "control L"))
                            (.addActionListener item-undo (proxy [ActionListener] [] (actionPerformed [e] (when (.canUndo undo-mgr) (.undo undo-mgr)))))
                            (.addActionListener item-redo (proxy [ActionListener] [] (actionPerformed [e] (when (.canRedo undo-mgr) (.redo undo-mgr)))))
                            (.addActionListener item-clear (proxy [ActionListener] [] (actionPerformed [e] (print-banner!))))
                            (.add menu-edit item-undo)
                            (.add menu-edit item-redo)
                            (.addSeparator menu-edit)
                            (.add menu-edit item-clear))

                      ;; Run Menu
                      menu-run (JMenu. "Run")
                      _ (.setMnemonic menu-run (int \R))
                      item-run-def (JMenuItem. "Run Definitions")
                      item-run-sel (JMenuItem. "Run Selection / Current Form")
                      item-stop (JMenuItem. "Stop Evaluation")
                      _ (do (.setAccelerator item-run-def (KeyStroke/getKeyStroke "control ENTER"))
                            (.setAccelerator item-run-sel (KeyStroke/getKeyStroke "control E"))
                            (.setAccelerator item-stop (KeyStroke/getKeyStroke "ESCAPE"))
                            (.addActionListener item-run-def (proxy [ActionListener] [] (actionPerformed [e] (run-definitions!))))
                            (.addActionListener item-run-sel (proxy [ActionListener] [] (actionPerformed [e] (run-selection!))))
                            (.addActionListener item-stop (proxy [ActionListener] [] (actionPerformed [e] (stop-current-eval!))))
                            (.add menu-run item-run-def)
                            (.add menu-run item-run-sel)
                            (.add menu-run item-stop))

                      ;; View Menu
                      menu-view (JMenu. "View")
                      _ (.setMnemonic menu-view (int \V))
                      item-zoom-in (JMenuItem. "Zoom In")
                      item-zoom-out (JMenuItem. "Zoom Out")
                      item-zoom-reset (JMenuItem. "Reset Zoom")
                      _ (do (.setAccelerator item-zoom-in (KeyStroke/getKeyStroke "control EQUALS"))
                            (.setAccelerator item-zoom-out (KeyStroke/getKeyStroke "control MINUS"))
                            (.setAccelerator item-zoom-reset (KeyStroke/getKeyStroke "control 0"))
                            (.addActionListener item-zoom-in (proxy [ActionListener] [] (actionPerformed [e] (swap! font-size #(min 32 (+ % 2))) (apply-font! @font-size))))
                            (.addActionListener item-zoom-out (proxy [ActionListener] [] (actionPerformed [e] (swap! font-size #(max 10 (- % 2))) (apply-font! @font-size))))
                            (.addActionListener item-zoom-reset (proxy [ActionListener] [] (actionPerformed [e] (reset! font-size 14) (apply-font! @font-size))))
                            (.add menu-view item-zoom-in)
                            (.add menu-view item-zoom-out)
                            (.add menu-view item-zoom-reset))

                      ;; Help Menu
                      menu-help (JMenu. "Help")
                      _ (.setMnemonic menu-help (int \H))
                      item-cheat (JMenuItem. "Clojure Cheatsheet")
                      item-cheat-dialog (JMenuItem. "Cheatsheet Examples (Dialog)")
                      item-about (JMenuItem. "About DrClojure")
                      _ (do (.setAccelerator item-cheat (KeyStroke/getKeyStroke "F1"))
                            (.addActionListener item-cheat (proxy [ActionListener] [] (actionPerformed [e] (open-cheatsheet!))))
                            (.addActionListener item-cheat-dialog (proxy [ActionListener] [] (actionPerformed [e] (show-cheatsheet-dialog! frame editor))))
                            (.addActionListener item-about
                                (proxy [ActionListener] []
                                  (actionPerformed [e]
                                    (JOptionPane/showMessageDialog frame
                                      (str app-name " version " app-version "\n\n"
                                           "A beginner-friendly Clojure IDE inspired by DrRacket.\n"
                                           "Written in Clojure.\n\n"
                                           "Running on Clojure " (clojure-version) "\n"
                                           "Java " (System/getProperty "java.version") "\n\n"
                                           "Distributed under Eclipse Public License.")
                                      (str "About " app-name) JOptionPane/INFORMATION_MESSAGE))))
                            (.add menu-help item-cheat)
                            (.add menu-help item-cheat-dialog)
                            (.add menu-help item-about))]

                  (.add menu-bar menu-file)
                  (.add menu-bar menu-edit)
                  (.add menu-bar menu-run)
                  (.add menu-bar menu-view)
                  (.add menu-bar menu-help)
                  (.setJMenuBar frame menu-bar))

                ;; --- Toolbar ---
                (let [toolbar (JToolBar.)
                      _ (.setFloatable toolbar false)]
                  (.add toolbar btn-run)
                  (.add toolbar btn-stop)
                  (.addSeparator toolbar)
                  (.add toolbar btn-clear)
                  (.addSeparator toolbar)
                  (.add toolbar btn-font-plus)
                  (.add toolbar btn-font-minus)
                  (.add toolbar (Box/createHorizontalGlue))
                  (.add toolbar btn-cheatsheet)

                  ;; --- Main Layout ---
                  (let [content-pane (JPanel. (BorderLayout.))]
                    (.add content-pane toolbar BorderLayout/NORTH)
                    (.add content-pane split-pane BorderLayout/CENTER)
                    (.add content-pane status-panel BorderLayout/SOUTH)
                    (.setContentPane frame content-pane)

                    ;; Window closing handler
                    (.setDefaultCloseOperation frame JFrame/DO_NOTHING_ON_CLOSE)
                    (.addWindowListener frame
                      (proxy [WindowAdapter] []
                        (windowClosing [e]
                          (file-exit!))))

                    ;; Layout frame
                    (.setPreferredSize frame (Dimension. 950 720))
                    (.pack frame)
                    (.setLocationRelativeTo frame nil)
                    (.requestFocusInWindow editor)
                    frame))))))))))

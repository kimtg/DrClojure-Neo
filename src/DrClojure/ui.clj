(ns DrClojure.ui
  "DrClojure Swing User Interface.
   Implements a DrRacket-inspired dual-pane layout with:
   - Definitions editor (top) with line numbers, bracket matching, undo/redo, 2-space tabs.
   - Unified Interactions console (bottom) integrating output display and REPL/stdin prompt.
   - Clojure cheatsheet, toolbar, status bar, and multi-window support."
  (:require [clojure.string :as str]
            [DrClojure.eval :as eval]
            [DrClojure.syntax :as syntax])
  (:import (javax.swing JFrame JPanel JSplitPane JScrollPane JTextArea JTextField JTextPane
                        JButton JLabel JMenuBar JMenu JMenuItem JPopupMenu KeyStroke
                        JFileChooser JOptionPane JToolBar BorderFactory Box
                        SwingUtilities UIManager JDialog JViewport JComponent JCheckBox AbstractAction
                        JList DefaultListModel DefaultListCellRenderer ListSelectionModel ScrollPaneConstants)
           (javax.swing.event DocumentListener CaretListener UndoableEditListener DocumentEvent$EventType ListSelectionListener PopupMenuListener)
           (javax.swing.text DefaultHighlighter$DefaultHighlightPainter JTextComponent
                             DefaultStyledDocument AbstractDocument$DefaultDocumentEvent
                             SimpleAttributeSet StyleConstants)
           (java.awt BorderLayout FlowLayout GridLayout Dimension Font Color Insets
                     KeyboardFocusManager Toolkit Desktop Desktop$Action GraphicsEnvironment)
           (java.net URI)
           (java.awt.event ActionEvent ActionListener KeyEvent KeyAdapter
                           MouseAdapter MouseEvent WindowAdapter WindowEvent)
           (java.util.concurrent Executors ThreadFactory ExecutorService)
           (java.util.concurrent.atomic AtomicLong)))

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
  "Finds the matching bracket position for the bracket at `pos` in `text`."
  [^String text pos]
  (when (and (string? text) (<= 0 pos (dec (.length text))))
    (get (:matches (syntax/compute-brackets text)) (long pos))))

(defn setup-bracket-matching!
  "Highlights matching parentheses/brackets at the caret position."
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
                 d (.getDocument editor)
                 doc-text (.getText d 0 (.getLength d))
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

(defn indent-selection!
  "Indents the current line (or selected block of lines) by prepending 2 spaces."
  [^JTextComponent editor]
  (let [doc (.getDocument editor)
        root (.getDefaultRootElement doc)
        sel-start (.getSelectionStart editor)
        sel-end (.getSelectionEnd editor)]
    (if (= sel-start sel-end)
      (.replaceSelection editor "  ")
      (let [start-line (.getElementIndex root sel-start)
            end-line (.getElementIndex root (if (and (> sel-end sel-start)
                                                    (= sel-end (.getStartOffset (.getElement root (.getElementIndex root sel-end)))))
                                              (dec sel-end)
                                              sel-end))
            lines (range end-line (dec start-line) -1)
            line-count (inc (- end-line start-line))]
        (doseq [line-idx lines]
          (let [elem (.getElement root line-idx)
                line-start (.getStartOffset elem)]
            (.insertString doc line-start "  " nil)))
        (let [new-start (+ sel-start 2)
              new-end (+ sel-end (* 2 line-count))]
          (.setSelectionStart editor new-start)
          (.setSelectionEnd editor new-end))))))

(defn unindent-selection!
  "Unindents the current line (or selected block of lines) by removing up to 2 leading spaces."
  [^JTextComponent editor]
  (let [doc (.getDocument editor)
        root (.getDefaultRootElement doc)
        sel-start (.getSelectionStart editor)
        sel-end (.getSelectionEnd editor)]
    (if (= sel-start sel-end)
      (let [line-idx (.getElementIndex root sel-start)
            elem (.getElement root line-idx)
            line-start (.getStartOffset elem)
            line-end (.getEndOffset elem)
            line-text (.getText doc line-start (- line-end line-start))]
        (when-let [m (re-find #"^(?:[ ]{1,2}|\t)" line-text)]
          (let [n (count m)]
            (.remove doc line-start n)
            (.setCaretPosition editor (max line-start (- sel-start n))))))
      (let [start-line (.getElementIndex root sel-start)
            end-line (.getElementIndex root (if (and (> sel-end sel-start)
                                                    (= sel-end (.getStartOffset (.getElement root (.getElementIndex root sel-end)))))
                                              (dec sel-end)
                                              sel-end))
            lines (range end-line (dec start-line) -1)
            first-line-diff (atom 0)
            total-diff (atom 0)]
        (doseq [line-idx lines]
          (let [elem (.getElement root line-idx)
                line-start (.getStartOffset elem)
                line-end (.getEndOffset elem)
                line-text (.getText doc line-start (- line-end line-start))]
            (when-let [m (re-find #"^(?:[ ]{1,2}|\t)" line-text)]
              (let [n (count m)]
                (.remove doc line-start n)
                (swap! total-diff #(+ % n))
                (when (= line-idx start-line)
                  (reset! first-line-diff n))))))
        (let [new-start (max 0 (- sel-start @first-line-diff))
              new-end (max new-start (- sel-end @total-diff))]
          (.setSelectionStart editor new-start)
          (.setSelectionEnd editor new-end))))))

(defn toggle-comment!
  "Comments or uncomments the current line or selected lines."
  [^JTextComponent editor]
  (let [doc (.getDocument editor)
        root (.getDefaultRootElement doc)
        sel-start (.getSelectionStart editor)
        sel-end (.getSelectionEnd editor)
        start-line (.getElementIndex root sel-start)
        end-line (.getElementIndex root (if (and (> sel-end sel-start)
                                                (= sel-end (.getStartOffset (.getElement root (.getElementIndex root sel-end)))))
                                          (dec sel-end)
                                          sel-end))
        lines (range start-line (inc end-line))
        non-empty-lines (filter (fn [idx]
                                  (let [elem (.getElement root idx)
                                        s (.getStartOffset elem)
                                        e (.getEndOffset elem)
                                        t (.getText doc s (- e s))]
                                    (not (str/blank? t))))
                                lines)
        all-commented? (and (seq non-empty-lines)
                            (every? (fn [line-idx]
                                      (let [elem (.getElement root line-idx)
                                            s (.getStartOffset elem)
                                            e (.getEndOffset elem)
                                            t (.getText doc s (- e s))]
                                        (boolean (re-find #"^[ ]*;+" t))))
                                    non-empty-lines))]
    (if all-commented?
      (doseq [line-idx (reverse lines)]
        (let [elem (.getElement root line-idx)
              s (.getStartOffset elem)
              e (.getEndOffset elem)
              t (.getText doc s (- e s))]
          (when-let [m (re-find #"^([ ]*)(;+[ ]?)" t)]
            (let [leading-spaces (count (nth m 1))
                  comment-chars (count (nth m 2))]
              (.remove doc (+ s leading-spaces) comment-chars)))))
      (doseq [line-idx (reverse lines)]
        (let [elem (.getElement root line-idx)
              s (.getStartOffset elem)
              e (.getEndOffset elem)
              t (.getText doc s (- e s))]
          (when-not (str/blank? t)
            (.insertString doc s "; " nil)))))))

(defn format-all!
  ([^JTextComponent editor] (format-all! editor nil))
  ([^JTextComponent editor status-fn]
   (let [old-text (.getText editor)
         new-text (syntax/format-code old-text)]
     (if (= old-text new-text)
       (when status-fn (status-fn " Already formatted "))
       (let [caret (.getCaretPosition editor)
             max-len (.length new-text)]
         (.setText editor new-text)
         (.setCaretPosition editor (min max-len caret))
         (when status-fn (status-fn " Formatted entire document ")))))))

(defn format-selection!
  ([^JTextComponent editor] (format-selection! editor nil))
  ([^JTextComponent editor status-fn]
   (let [old-text (.getText editor)
         sel-start (.getSelectionStart editor)
         sel-end (.getSelectionEnd editor)]
     (if (not= sel-start sel-end)
       (let [new-text (syntax/format-selection-text old-text sel-start sel-end)]
         (if (= old-text new-text)
           (when status-fn (status-fn " Selection already formatted "))
           (let [caret (.getCaretPosition editor)
                 max-len (.length new-text)]
             (.setText editor new-text)
             (.setCaretPosition editor (min max-len caret))
             (when status-fn (status-fn " Formatted selection ")))))
       (let [pos (.getCaretPosition editor)
             doc-len (.length old-text)
             line-start (let [idx (.lastIndexOf old-text "\n" (max 0 (dec pos)))]
                          (if (neg? idx) 0 (inc idx)))
             line-end (let [idx (.indexOf old-text "\n" pos)]
                        (if (neg? idx) doc-len idx))
             new-text (syntax/format-selection-text old-text line-start line-end)]
         (if (= old-text new-text)
           (when status-fn (status-fn " Line already formatted "))
           (let [max-len (.length new-text)]
             (.setText editor new-text)
             (.setCaretPosition editor (min max-len pos))
             (when status-fn (status-fn " Formatted current line ")))))))))

(def open->matching-close
  {\( \), \[ \], \{ \}, \" \"})

(def close-delimiters
  #{\) \] \} \"})

(defn handle-smart-enter!
  [^JTextComponent editor]
  (let [doc (.getDocument editor)
        sel-start (.getSelectionStart editor)
        sel-end (.getSelectionEnd editor)]
    (when (> sel-end sel-start)
      (.remove doc sel-start (- sel-end sel-start))
      (.setCaretPosition editor sel-start))
    (let [pos (.getCaretPosition editor)
          text (.getText doc 0 (.getLength doc))
          line-start (let [idx (.lastIndexOf text "\n" (dec pos))]
                       (if (neg? idx) 0 (inc idx)))
          line-prefix (.substring text line-start pos)
          prev-ch (when (pos? pos) (.charAt text (dec pos)))
          next-ch (when (< pos (.length text)) (.charAt text pos))
          between-pair? (and prev-ch next-ch
                             (contains? #{\( \[ \{} prev-ch)
                             (= (open->matching-close prev-ch) next-ch))]
      (if between-pair?
        (let [base-indent (or (re-find #"^[ ]+" line-prefix) "")
              inner-indent (str base-indent "  ")
              close-indent base-indent
              insert-str (str "\n" inner-indent "\n" close-indent)]
          (.insertString doc pos insert-str nil)
          (.setCaretPosition editor (+ pos 1 (count inner-indent))))
        (let [indent (syntax/compute-smart-indent text pos)
              insert-str (str "\n" indent)]
          (.insertString doc pos insert-str nil)
          (.setCaretPosition editor (+ pos (count insert-str))))))))

(defn setup-auto-brackets!
  [^JTextComponent editor]
  (.addKeyListener editor
    (proxy [KeyAdapter] []
      (keyTyped [^KeyEvent e]
        (when-not (or (.isControlDown e) (.isAltDown e) (.isMetaDown e))
          (let [ch (.getKeyChar e)]
            (cond
              (contains? open->matching-close ch)
              (let [doc (.getDocument editor)
                    sel-start (.getSelectionStart editor)
                    sel-end (.getSelectionEnd editor)
                    close-ch (open->matching-close ch)]
                (if (> sel-end sel-start)
                  (let [sel-text (.getSelectedText editor)
                        wrapped (str ch sel-text close-ch)]
                    (.consume e)
                    (.remove doc sel-start (- sel-end sel-start))
                    (.insertString doc sel-start wrapped nil)
                    (.setSelectionStart editor sel-start)
                    (.setSelectionEnd editor (+ sel-start (count wrapped))))
                  (let [caret (.getCaretPosition editor)
                        doc-len (.getLength doc)
                        next-ch (when (< caret doc-len) (.charAt (.getText doc caret 1) 0))]
                    (if (and (= ch \") (= next-ch \"))
                      (do
                        (.consume e)
                        (.setCaretPosition editor (inc caret)))
                      (do
                        (.consume e)
                        (.insertString doc caret (str ch close-ch) nil)
                        (.setCaretPosition editor (inc caret)))))))

              (contains? close-delimiters ch)
              (let [sel-start (.getSelectionStart editor)
                    sel-end (.getSelectionEnd editor)]
                (when (= sel-start sel-end)
                  (let [doc (.getDocument editor)
                        caret (.getCaretPosition editor)
                        doc-len (.getLength doc)]
                    (when (< caret doc-len)
                      (let [next-ch (.charAt (.getText doc caret 1) 0)]
                        (when (= ch next-ch)
                          (.consume e)
                          (.setCaretPosition editor (inc caret))))))))

              :else nil))))

      (keyPressed [^KeyEvent e]
        (when-not (or (.isControlDown e) (.isAltDown e) (.isMetaDown e))
          (when (= (.getKeyCode e) KeyEvent/VK_BACK_SPACE)
            (let [sel-start (.getSelectionStart editor)
                  sel-end (.getSelectionEnd editor)]
              (when (= sel-start sel-end)
                (let [caret (.getCaretPosition editor)
                      doc (.getDocument editor)
                      doc-len (.getLength doc)]
                  (when (and (pos? caret) (< caret doc-len))
                    (let [prev-ch (.charAt (.getText doc (dec caret) 1) 0)
                          next-ch (.charAt (.getText doc caret 1) 0)]
                      (when (= (open->matching-close prev-ch) next-ch)
                        (.consume e)
                        (.remove doc (dec caret) 2)
                        (.setCaretPosition editor (dec caret))))))))))))))

(defn setup-editor-keys!
  [^JTextComponent editor toggle-comment-fn!]
  (.setFocusTraversalKeys editor KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS java.util.Collections/EMPTY_SET)
  (.setFocusTraversalKeys editor KeyboardFocusManager/BACKWARD_TRAVERSAL_KEYS java.util.Collections/EMPTY_SET)
  (let [im (.getInputMap editor)
        am (.getActionMap editor)]
    (.put im (KeyStroke/getKeyStroke "TAB") "block-indent")
    (.put am "block-indent"
      (proxy [AbstractAction] []
        (actionPerformed [e]
          (indent-selection! editor))))
    (.put im (KeyStroke/getKeyStroke "shift TAB") "block-unindent")
    (.put im (KeyStroke/getKeyStroke KeyEvent/VK_TAB KeyEvent/SHIFT_DOWN_MASK) "block-unindent")
    (.put am "block-unindent"
      (proxy [AbstractAction] []
        (actionPerformed [e]
          (unindent-selection! editor))))

    (.put im (KeyStroke/getKeyStroke "ENTER") "smart-enter")
    (.put am "smart-enter"
      (proxy [AbstractAction] []
        (actionPerformed [e]
          (handle-smart-enter! editor))))

    (.put im (KeyStroke/getKeyStroke "control SLASH") "toggle-comment")
    (.put im (KeyStroke/getKeyStroke KeyEvent/VK_SLASH KeyEvent/CTRL_DOWN_MASK) "toggle-comment")
    (.put im (KeyStroke/getKeyStroke "control SEMICOLON") "toggle-comment")
    (.put im (KeyStroke/getKeyStroke KeyEvent/VK_SEMICOLON KeyEvent/CTRL_DOWN_MASK) "toggle-comment")
    (.put am "toggle-comment"
      (proxy [AbstractAction] []
        (actionPerformed [e]
          (toggle-comment-fn!))))))

(defn jump-to-definition!
  [^JFrame frame ^JTextComponent editor & [_set-status!]]
  (let [doc (.getDocument editor)
        text (.getText doc 0 (.getLength doc))
        sel (.getSelectedText editor)
        caret (.getCaretPosition editor)
        sym-info (if (and (not (str/blank? sel)) (re-matches #"^[-_a-zA-Z0-9\p{L}\p{N}.!$%&*+/<=>?#]+$" (str/trim sel)))
                   {:symbol (str/trim sel) :start (.getSelectionStart editor) :end (.getSelectionEnd editor)}
                   (syntax/symbol-at-pos text caret))]
    (if-not sym-info
      (.. Toolkit getDefaultToolkit beep)
      (let [sym-name (:symbol sym-info)
            def-target (syntax/find-definition text sym-name caret)]
        (if def-target
          (let [start-pos (:start def-target)
                line-num (:line def-target)]
            (.setCaretPosition editor (int start-pos))
            (.setSelectionStart editor (int start-pos))
            (.setSelectionEnd editor (int start-pos))
            (try
              (if-let [rect (.modelToView2D editor (int start-pos))]
                (.scrollRectToVisible editor (.getBounds rect))
                (let [root (.. editor getDocument getDefaultRootElement)
                      line-idx (dec line-num)
                      elem (.getElement root line-idx)]
                  (.scrollRectToVisible editor (java.awt.Rectangle. 0 (int (* line-idx 18)) 1 1))))
              (catch Exception _ nil))
            (.requestFocusInWindow editor))
          (let [sym (symbol sym-name)
                v (try (resolve sym) (catch Exception _ nil))]
            (if v
              (let [m (meta v)
                    ns-str (str (or (:ns m) "clojure.core"))
                    name-str (str (:name m))
                    file-str (or (:file m) "unknown")
                    line-num (:line m)
                    arglists (when-let [args (:arglists m)] (str args))
                    doc-str (:doc m)
                    msg-sb (StringBuilder.)]
                (.append msg-sb (str "Symbol: " ns-str "/" name-str "\n"))
                (when file-str (.append msg-sb (str "Defined in: " file-str (when line-num (str ":" line-num)) "\n")))
                (when arglists (.append msg-sb (str "Arglists: " arglists "\n\n")))
                (when doc-str (.append msg-sb (str "Documentation:\n" doc-str "\n")))
                (JOptionPane/showMessageDialog frame (str msg-sb) (str "Definition: " ns-str "/" name-str) JOptionPane/INFORMATION_MESSAGE))
              (JOptionPane/showMessageDialog frame
                (str "Could not find definition for '" sym-name "' in current file or loaded namespaces.")
                "Jump to Definition"
                JOptionPane/INFORMATION_MESSAGE))))))))

(defn rename-symbol-dialog!
  [^JFrame frame ^JTextComponent editor highlight-now! update-title! set-status!]
  (let [doc (.getDocument editor)
        text (.getText doc 0 (.getLength doc))
        sel (.getSelectedText editor)
        caret (.getCaretPosition editor)
        sym-info (if (and (not (str/blank? sel)) (re-matches #"^[-_a-zA-Z0-9\p{L}\p{N}.!$%&*+/<=>?#]+$" (str/trim sel)))
                   {:symbol (str/trim sel) :start (.getSelectionStart editor) :end (.getSelectionEnd editor)}
                   (syntax/symbol-at-pos text caret))]
    (if-not sym-info
      (JOptionPane/showMessageDialog frame
        "Place the cursor on a symbol or select a symbol to rename."
        "Rename Symbol"
        JOptionPane/INFORMATION_MESSAGE)
      (let [old-sym (:symbol sym-info)
            input (JOptionPane/showInputDialog frame
                    (str "Rename symbol '" old-sym "' to:")
                    "Rename Symbol (Refactor)"
                    JOptionPane/QUESTION_MESSAGE
                    nil
                    nil
                    old-sym)]
        (when (some? input)
          (let [new-sym (str/trim (str input))]
            (cond
              (= new-sym old-sym)
              (set-status! "Symbol name unchanged.")

              (or (empty? new-sym)
                  (not (re-matches #"^[-_a-zA-Z0-9\p{L}\p{N}.!$%&*+/<=>?#]+$" new-sym)))
              (JOptionPane/showMessageDialog frame
                (str "Invalid Clojure symbol name: \"" new-sym "\"")
                "Rename Error"
                JOptionPane/ERROR_MESSAGE)

              :else
              (let [doc-text (.getText doc 0 (.getLength doc))
                    occs (syntax/find-symbol-occurrences doc-text old-sym)]
                (if (empty? occs)
                  (set-status! (str "No occurrences of '" old-sym "' found to rename."))
                  (do
                    (doseq [[s e] (reverse occs)]
                      (.remove doc (int s) (int (- e s)))
                      (.insertString doc (int s) new-sym nil))
                    (highlight-now!)
                    (update-title!)
                    (let [n (count occs)
                          msg (str "Renamed " n " occurrence" (when (> n 1) "s") " of '" old-sym "' to '" new-sym "'.")]
                      (set-status! msg)
                      (JOptionPane/showMessageDialog frame msg "Rename Complete" JOptionPane/INFORMATION_MESSAGE))))))))))))

;; --- Quick Documentation ---

(defn show-doc-dialog!
  [^JFrame frame ^String content ^String title]
  (let [dialog (JDialog. frame title false)
        text-area (JTextArea. 16 56)
        _ (do (.setText text-area content)
              (.setEditable text-area false)
              (.setCaretPosition text-area 0)
              (.setFont text-area (Font. Font/MONOSPACED Font/PLAIN 13))
              (.setBackground text-area (Color. 250 250 250))
              (.setBorder text-area (BorderFactory/createEmptyBorder 8 10 8 10)))
        scroll (JScrollPane. text-area)
        btn-close (JButton. "Close")
        _ (.addActionListener btn-close
            (proxy [ActionListener] []
              (actionPerformed [e]
                (.dispose dialog))))
        btn-panel (JPanel. (FlowLayout. FlowLayout/RIGHT))
        _ (.add btn-panel btn-close)
        content-pane (.getContentPane dialog)]
    (.setLayout content-pane (BorderLayout. 0 4))
    (.add content-pane scroll BorderLayout/CENTER)
    (.add content-pane btn-panel BorderLayout/SOUTH)
    (.. dialog getRootPane (setDefaultButton btn-close))
    (.. dialog getRootPane (getInputMap JComponent/WHEN_IN_FOCUSED_WINDOW)
        (put (KeyStroke/getKeyStroke "ESCAPE") "close-dialog"))
    (.. dialog getRootPane (getActionMap)
        (put "close-dialog"
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (.dispose dialog)))))
    (.pack dialog)
    (when frame (.setLocationRelativeTo dialog frame))
    (.setVisible dialog true)))

(defn show-quick-doc!
  [^JFrame frame ^JTextComponent editor & [_set-status!]]
  (let [doc (.getDocument editor)
        text (.getText doc 0 (.getLength doc))
        caret (.getCaretPosition editor)
        sel (.getSelectedText editor)
        sym-info (if (and (not (str/blank? sel)) (re-matches #"^[-_a-zA-Z0-9\p{L}\p{N}.!$%&*+/<=>?#]+$" (str/trim sel)))
                   {:symbol (str/trim sel)}
                   (syntax/symbol-at-pos text caret))]
    (if-not sym-info
      (JOptionPane/showMessageDialog frame
        "No symbol at cursor to inspect documentation."
        "Quick Documentation"
        JOptionPane/INFORMATION_MESSAGE)
      (let [sym-name (:symbol sym-info)
            doc-info (syntax/get-symbol-doc sym-name text caret)]
        (case (:status doc-info)
          :found
          (let [msg-sb (StringBuilder.)]
            (.append msg-sb (str "Symbol: " (:ns doc-info) "/" (:name doc-info) "\n"))
            (when (:macro? doc-info)
              (.append msg-sb "Type: Macro\n"))
            (when-let [args (:arglists doc-info)]
              (.append msg-sb (str "Arglists: " args "\n\n")))
            (if-let [d (:doc doc-info)]
              (.append msg-sb (str "Documentation:\n" d "\n\n"))
              (.append msg-sb "Documentation: (Not documented)\n\n"))
            (when-let [f (:file doc-info)]
              (.append msg-sb (str "Source: " f (when-let [l (:line doc-info)] (str ":" l)))))
            (show-doc-dialog! frame (str msg-sb) (str "Doc: " (:ns doc-info) "/" (:name doc-info))))

          :buffer-def
          (let [msg (str "Symbol: " sym-name "\n\n"
                         "Defined locally in this buffer at line " (:line doc-info) " (" (name (:kind doc-info)) ").")]
            (show-doc-dialog! frame msg (str "Definition: " sym-name)))

          :not-found
          (JOptionPane/showMessageDialog frame
            (str "No documentation found for symbol '" sym-name "'.")
            "Quick Documentation"
            JOptionPane/INFORMATION_MESSAGE))))))

;; --- In-Editor Find & Replace Panel ---

(defn create-find-replace-panel
  [^JTextComponent editor highlight-now! update-title! set-status!]
  (let [panel (JPanel. (BorderLayout. 4 2))
        _ (.setBorder panel (BorderFactory/createCompoundBorder
                              (BorderFactory/createMatteBorder 1 0 0 0 (Color. 210 210 210))
                              (BorderFactory/createEmptyBorder 4 6 4 6)))
        _ (.setBackground panel (Color. 248 248 248))

        find-field (JTextField. 16)
        replace-field (JTextField. 16)
        _ (do (.setFont find-field (Font. Font/MONOSPACED Font/PLAIN 12))
              (.setFont replace-field (Font. Font/MONOSPACED Font/PLAIN 12)))
        match-label (JLabel. "No matches")
        _ (.setForeground match-label (Color. 120 120 120))
        match-case-cb (JCheckBox. "Match Case")
        _ (.setBackground match-case-cb (Color. 248 248 248))

        btn-prev (JButton. "▲ Prev")
        btn-next (JButton. "▼ Next")
        btn-replace (JButton. "Replace")
        btn-replace-all (JButton. "Replace All")
        btn-close (JButton. "✕")
        _ (.setMargin btn-close (Insets. 0 4 0 4))
        _ (.setFocusable btn-close false)

        matches-atom (atom [])
        cur-idx-atom (atom -1)

        select-match! (fn [idx]
                        (let [matches @matches-atom
                              total (count matches)]
                          (when (and (pos? total) (<= 0 idx (dec total)))
                            (reset! cur-idx-atom idx)
                            (let [[s e] (nth matches idx)]
                              (.setCaretPosition editor (int s))
                              (.setSelectionStart editor (int s))
                              (.setSelectionEnd editor (int e))
                              (try
                                (if-let [rect (.modelToView2D editor (int s))]
                                  (.scrollRectToVisible editor (.getBounds rect)))
                                (catch Exception _ nil)))
                            (.setText match-label (str (inc idx) " of " total)))))

        update-matches! (fn []
                          (let [doc (.getDocument editor)
                                text (.getText doc 0 (.getLength doc))
                                query (.getText find-field)
                                case? (.isSelected match-case-cb)
                                matches (if (str/blank? query)
                                          []
                                          (syntax/find-text-matches text query {:case-sensitive? case?}))
                                total (count matches)]
                            (reset! matches-atom matches)
                            (if (zero? total)
                              (do
                                (reset! cur-idx-atom -1)
                                (.setText match-label "No matches"))
                              (let [caret (.getCaretPosition editor)
                                    idx (or (first (keep-indexed (fn [i [s e]] (when (<= s caret e) i)) matches))
                                            (first (keep-indexed (fn [i [s _]] (when (>= s caret) i)) matches))
                                            0)]
                                (select-match! idx)))))

        find-next! (fn []
                     (update-matches!)
                     (let [matches @matches-atom
                           total (count matches)]
                       (when (pos? total)
                         (let [next-idx (mod (inc @cur-idx-atom) total)]
                           (select-match! next-idx)))))

        find-prev! (fn []
                     (update-matches!)
                     (let [matches @matches-atom
                           total (count matches)]
                       (when (pos? total)
                         (let [prev-idx (mod (dec @cur-idx-atom) total)]
                           (select-match! prev-idx)))))

        replace-current! (fn []
                           (let [matches @matches-atom
                                 cur-idx @cur-idx-atom]
                             (when (and (pos? (count matches)) (<= 0 cur-idx (dec (count matches))))
                               (let [[s e] (nth matches cur-idx)
                                     rep-text (.getText replace-field)
                                     doc (.getDocument editor)]
                                 (when (and (= (.getSelectionStart editor) s)
                                            (= (.getSelectionEnd editor) e))
                                   (.remove doc (int s) (int (- e s)))
                                   (.insertString doc (int s) rep-text nil)
                                   (highlight-now!)
                                   (update-title!)
                                   (update-matches!)
                                   (when (pos? (count @matches-atom))
                                     (let [next-idx (min @cur-idx-atom (dec (count @matches-atom)))]
                                       (select-match! next-idx))))))))

        replace-all! (fn []
                       (let [doc (.getDocument editor)
                             text (.getText doc 0 (.getLength doc))
                             query (.getText find-field)
                             rep-text (.getText replace-field)
                             case? (.isSelected match-case-cb)
                             matches (syntax/find-text-matches text query {:case-sensitive? case?})
                             total (count matches)]
                         (if (zero? total)
                           (set-status! "No matches found to replace.")
                           (do
                             (doseq [[s e] (reverse matches)]
                               (.remove doc (int s) (int (- e s)))
                               (.insertString doc (int s) rep-text nil))
                             (highlight-now!)
                             (update-title!)
                             (update-matches!)
                             (let [msg (str "Replaced " total " occurrence" (when (> total 1) "s") ".")]
                               (set-status! msg)
                               (.setText match-label msg))))))

        close-panel! (fn []
                       (.setVisible panel false)
                       (.requestFocusInWindow editor))

        open-find! (fn []
                     (let [sel (.getSelectedText editor)]
                       (when-not (str/blank? sel)
                         (.setText find-field (str/trim sel))))
                     (.setVisible panel true)
                     (.requestFocusInWindow find-field)
                     (.selectAll find-field)
                     (update-matches!))

        open-replace! (fn []
                        (open-find!)
                        (.requestFocusInWindow replace-field)
                        (.selectAll replace-field))]

    (.addActionListener btn-next (proxy [ActionListener] [] (actionPerformed [e] (find-next!))))
    (.addActionListener btn-prev (proxy [ActionListener] [] (actionPerformed [e] (find-prev!))))
    (.addActionListener btn-replace (proxy [ActionListener] [] (actionPerformed [e] (replace-current!))))
    (.addActionListener btn-replace-all (proxy [ActionListener] [] (actionPerformed [e] (replace-all!))))
    (.addActionListener btn-close (proxy [ActionListener] [] (actionPerformed [e] (close-panel!))))
    (.addActionListener match-case-cb (proxy [ActionListener] [] (actionPerformed [e] (update-matches!))))

    (.addActionListener find-field
      (proxy [ActionListener] []
        (actionPerformed [e]
          (find-next!))))

    (.. find-field getDocument (addDocumentListener
      (proxy [DocumentListener] []
        (insertUpdate [e] (update-matches!))
        (removeUpdate [e] (update-matches!))
        (changedUpdate [e] (update-matches!)))))

    (doseq [field [find-field replace-field]]
      (let [im (.getInputMap field JComponent/WHEN_FOCUSED)
            am (.getActionMap field)]
        (.put im (KeyStroke/getKeyStroke "ESCAPE") "close-find")
        (.put am "close-find" (proxy [AbstractAction] [] (actionPerformed [e] (close-panel!))))
        (.put im (KeyStroke/getKeyStroke "shift ENTER") "prev-match")
        (.put am "prev-match" (proxy [AbstractAction] [] (actionPerformed [e] (find-prev!))))))

    (let [row1 (JPanel. (FlowLayout. FlowLayout/LEFT 4 1))
          _ (.setBackground row1 (Color. 248 248 248))
          lbl-find (JLabel. "Find:")
          _ (.setFont lbl-find (Font. "SansSerif" Font/PLAIN 12))
          row2 (JPanel. (FlowLayout. FlowLayout/LEFT 4 1))
          _ (.setBackground row2 (Color. 248 248 248))
          lbl-replace (JLabel. "Replace:")
          _ (.setFont lbl-replace (Font. "SansSerif" Font/PLAIN 12))
          grid (JPanel. (GridLayout. 2 1 0 2))
          _ (.setBackground grid (Color. 248 248 248))]

      (.add row1 lbl-find)
      (.add row1 find-field)
      (.add row1 btn-prev)
      (.add row1 btn-next)
      (.add row1 match-case-cb)
      (.add row1 match-label)
      (.add row1 (Box/createHorizontalStrut 8))
      (.add row1 btn-close)

      (.add row2 lbl-replace)
      (.add row2 replace-field)
      (.add row2 btn-replace)
      (.add row2 btn-replace-all)

      (.add grid row1)
      (.add grid row2)
      (.add panel grid BorderLayout/CENTER))

    (.setVisible panel false)

    {:panel panel
     :open-find! open-find!
     :open-replace! open-replace!
     :close-panel! close-panel!
     :find-next! find-next!
     :find-prev! find-prev!}))

;; --- Autocomplete Background Worker & State ---

(defonce ^ExecutorService autocomplete-executor
  (Executors/newFixedThreadPool 2
    (reify ThreadFactory
      (newThread [_ r]
        (doto (Thread. ^Runnable r "DrClojure-Autocomplete-Worker")
          (.setDaemon true)
          (.setPriority Thread/NORM_PRIORITY))))))

(defonce autocomplete-request-seq (AtomicLong. 0))
(defonce doc-preview-seq (AtomicLong. 0))

(defn- create-autocomplete-renderer []
  (let [font-mono (Font. Font/MONOSPACED Font/PLAIN 12)
        bg-selected (Color. 220 235 252)
        fg-selected (Color. 0 0 0)
        bg-normal Color/WHITE
        fg-special (Color. 130 30 150)
        fg-builtin (Color. 0 90 180)
        fg-user (Color. 20 120 40)
        fg-default (Color. 40 40 40)]
    (proxy [DefaultListCellRenderer] []
      (getListCellRendererComponent [list value index isSelected cellHasFocus]
        (let [c (proxy-super getListCellRendererComponent list value index isSelected cellHasFocus)]
          (when (map? value)
            (let [{:keys [symbol category]} value
                  badge (case category
                          :special " [special]"
                          :builtin " [builtin]"
                          :user    " [user]"
                          "")]
              (.setText c (str symbol badge))
              (.setFont c font-mono)
              (if isSelected
                (do
                  (.setBackground c bg-selected)
                  (.setForeground c fg-selected))
                (do
                  (.setBackground c bg-normal)
                  (.setForeground c (case category
                                      :special fg-special
                                      :builtin fg-builtin
                                      :user    fg-user
                                      fg-default))))))
          c)))))

(defn dismiss-autocomplete!
  [active-popup-atom]
  (when-let [{:keys [popup]} @active-popup-atom]
    (try (.setVisible ^JPopupMenu popup false) (catch Exception _ nil))
    (reset! active-popup-atom nil)))

(defn commit-autocomplete!
  [active-popup-atom]
  (when-let [{:keys [popup list start editor status-fn highlight-fn dirty-fn just-committed-atom]} @active-popup-atom]
    (try (.setVisible ^JPopupMenu popup false) (catch Exception _ nil))
    (reset! active-popup-atom nil)
    (when just-committed-atom
      (reset! just-committed-atom true)
      (SwingUtilities/invokeLater #(reset! just-committed-atom false)))
    (when-let [selected (.getSelectedValue ^JList list)]
      (let [sym (:symbol selected)
            doc (.getDocument editor)
            text (.getText doc 0 (.getLength doc))
            caret (.getCaretPosition editor)
            prefix-info (syntax/get-symbol-prefix-at-pos text caret)
            replace-end (max (int caret) (int (:word-end prefix-info)))]
        (.setSelectionStart editor (int start))
        (.setSelectionEnd editor (int replace-end))
        (.replaceSelection editor sym)
        (.setCaretPosition editor (+ (int start) (count sym)))
        (when dirty-fn (dirty-fn))
        (when highlight-fn (highlight-fn))
        (when status-fn (status-fn (str "Completed: " sym)))
        (.requestFocusInWindow editor)))))

(defn- update-doc-preview!
  ([^JTextArea doc-area candidate ^String text caret]
   (update-doc-preview! doc-area candidate text caret nil))
  ([^JTextArea doc-area candidate ^String text caret extra-context]
   (when doc-area
     (if candidate
       (let [sym (:symbol candidate)
             k (if (and text (not (str/blank? text)))
                 [(str sym) (hash text) caret]
                 (str sym))]
         (if-let [hit (get @syntax/docstring-cache k)]
           (let [formatted (syntax/format-autocomplete-doc hit candidate)]
             (.setText doc-area formatted)
             (.setCaretPosition doc-area 0))
           (if-not (SwingUtilities/isEventDispatchThread)
             (let [extra-text (if (fn? extra-context) (extra-context) extra-context)
                   doc-info-primary (syntax/get-cached-symbol-doc sym text caret)
                   doc-info (if (and (= (:status doc-info-primary) :not-found) (seq extra-text))
                              (let [sec (syntax/get-cached-symbol-doc sym extra-text 0)]
                                (if (not= (:status sec) :not-found) sec doc-info-primary))
                              doc-info-primary)
                   formatted (syntax/format-autocomplete-doc doc-info candidate)]
               (.setText doc-area formatted)
               (.setCaretPosition doc-area 0))
             (let [doc-id (.incrementAndGet doc-preview-seq)
                   extra-text (if (fn? extra-context) (extra-context) extra-context)]
               (.setText doc-area (str sym "\n\nLoading documentation..."))
               (.setCaretPosition doc-area 0)
               (try
                 (.submit ^ExecutorService autocomplete-executor
                   ^Runnable (fn []
                               (let [doc-info-primary (syntax/get-cached-symbol-doc sym text caret)
                                     doc-info (if (and (= (:status doc-info-primary) :not-found) (seq extra-text))
                                                (let [sec (syntax/get-cached-symbol-doc sym extra-text 0)]
                                                  (if (not= (:status sec) :not-found) sec doc-info-primary))
                                                doc-info-primary)
                                     formatted (syntax/format-autocomplete-doc doc-info candidate)]
                                 (SwingUtilities/invokeLater
                                   (fn []
                                     (when (= doc-id (.get doc-preview-seq))
                                       (.setText doc-area formatted)
                                       (.setCaretPosition doc-area 0)))))))
                 (catch Exception _ nil))))))
       (.setText doc-area "")))))

(defn move-popup-selection!
  [active-popup-atom delta]
  (when-let [{:keys [list model doc-area editor extra-context]} @active-popup-atom]
    (let [cnt (.getSize ^DefaultListModel model)
          cur (.getSelectedIndex ^JList list)
          next-idx (cond
                     (neg? cur) 0
                     :else (max 0 (min (dec cnt) (+ cur delta))))]
      (when (pos? cnt)
        (.setSelectedIndex ^JList list next-idx)
        (.ensureIndexIsVisible ^JList list next-idx)
        (when doc-area
          (let [sel (.getSelectedValue ^JList list)
                doc (.getDocument editor)
                txt (.getText doc 0 (.getLength doc))
                c (.getCaretPosition editor)]
            (update-doc-preview! doc-area sel txt c extra-context)))))))

(defn update-autocomplete-filter!
  ([active-popup-atom]
   (update-autocomplete-filter! active-popup-atom nil))
  ([active-popup-atom opts]
   (when-let [{:keys [popup list model doc-area start editor extra-context]} @active-popup-atom]
     (when (or (.isVisible ^JPopupMenu popup) (not (.isShowing editor)))
       (let [doc (.getDocument editor)
             text (.getText doc 0 (.getLength doc))
             caret (.getCaretPosition editor)]
         (if (< caret start)
           (dismiss-autocomplete! active-popup-atom)
           (let [prefix-info (syntax/get-symbol-prefix-at-pos text caret)]
             (if (or (not= (:start prefix-info) start)
                     (and (empty? (:prefix prefix-info)) (< caret start)))
               (dismiss-autocomplete! active-popup-atom)
               (let [prefix (:prefix prefix-info)
                     sync? (and (map? opts) (:sync? opts))]
                 (if sync?
                   (let [candidates (syntax/get-autocomplete-candidates prefix text caret extra-context)]
                     (if (empty? candidates)
                       (dismiss-autocomplete! active-popup-atom)
                       (do
                         (doto ^DefaultListModel model
                           (.clear)
                           (.addAll ^java.util.Collection candidates))
                         (.setSelectedIndex ^JList list 0)
                         (.ensureIndexIsVisible ^JList list 0)
                         (when doc-area
                           (update-doc-preview! doc-area (first candidates) text caret extra-context)))))
                   (let [req-id (.incrementAndGet autocomplete-request-seq)]
                     (try
                       (.submit ^ExecutorService autocomplete-executor
                         ^Runnable (fn []
                                     (let [candidates (syntax/get-autocomplete-candidates prefix text caret extra-context)]
                                       (SwingUtilities/invokeLater
                                         (fn []
                                           (when (= req-id (.get autocomplete-request-seq))
                                             (when-let [{:keys [popup list model doc-area]} @active-popup-atom]
                                               (when (or (.isVisible ^JPopupMenu popup) (not (.isShowing editor)))
                                                 (if (empty? candidates)
                                                   (dismiss-autocomplete! active-popup-atom)
                                                   (do
                                                     (doto ^DefaultListModel model
                                                       (.clear)
                                                       (.addAll ^java.util.Collection candidates))
                                                     (.setSelectedIndex ^JList list 0)
                                                     (.ensureIndexIsVisible ^JList list 0)
                                                     (when doc-area
                                                       (update-doc-preview! doc-area (first candidates) text caret extra-context))))))))))))
                       (catch Exception _ nil)))))))))))))

(defn show-autocomplete-popup!
  ([^JTextComponent editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty!]
   (show-autocomplete-popup! editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! nil nil))
  ([^JTextComponent editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context]
   (show-autocomplete-popup! editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context nil))
  ([^JTextComponent editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom]
   (dismiss-autocomplete! active-popup-atom)
   (let [popup (JPopupMenu.)
         _ (.setBorder popup (BorderFactory/createLineBorder (Color. 180 180 180) 1))
         model (DefaultListModel.)
         _ (.addAll model ^java.util.Collection candidates)
         list (JList. model)
         _ (.setCellRenderer list (create-autocomplete-renderer))
         _ (.setPrototypeCellValue list {:symbol "defprotocol" :category :special})
         _ (.setFixedCellHeight list 20)
         _ (.setFixedCellWidth list 210)
         _ (.setSelectionMode list ListSelectionModel/SINGLE_SELECTION)
         _ (.setSelectedIndex list 0)
         _ (.setFocusable list false)
         scroll-list (JScrollPane. list ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)
         _ (.setBorder scroll-list (BorderFactory/createMatteBorder 0 0 0 1 (Color. 220 220 220)))
         _ (.setPreferredSize scroll-list (Dimension. 220 220))
         doc-area (JTextArea.)
         _ (.setEditable doc-area false)
         _ (.setLineWrap doc-area true)
         _ (.setWrapStyleWord doc-area true)
         _ (.setFont doc-area (Font. Font/MONOSPACED Font/PLAIN 12))
         _ (.setBackground doc-area (Color. 250 250 252))
         _ (.setForeground doc-area (Color. 40 40 40))
         _ (.setMargin doc-area (Insets. 6 8 6 8))
         _ (.setFocusable doc-area false)
         scroll-doc (JScrollPane. doc-area ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)
         _ (.setBorder scroll-doc (BorderFactory/createEmptyBorder))
         _ (.setPreferredSize scroll-doc (Dimension. 400 220))
         content-panel (JPanel. (BorderLayout.))
         _ (.add content-panel scroll-list BorderLayout/WEST)
         _ (.add content-panel scroll-doc BorderLayout/CENTER)
         _ (.add popup content-panel)
         caret (.getCaretPosition editor)
         doc (.getDocument editor)
         text (.getText doc 0 (.getLength doc))
         _ (update-doc-preview! doc-area (first candidates) text caret extra-context)
         _ (.addListSelectionListener list
             (reify ListSelectionListener
               (valueChanged [this e]
                 (when-not (.getValueIsAdjusting e)
                   (let [sel (.getSelectedValue list)
                         cur-doc (.getDocument editor)
                         cur-txt (.getText cur-doc 0 (.getLength cur-doc))
                         cur-caret (.getCaretPosition editor)]
                     (update-doc-preview! doc-area sel cur-txt cur-caret extra-context))))))
         r (try
             (if-let [rect (.modelToView2D editor (int caret))]
               rect
               (.modelToView editor (int caret)))
             (catch Exception _ nil))
         x (if r (int (.getX r)) 0)
         raw-y (if r (int (+ (.getY r) (.getHeight r))) 0)
         popup-height 230
         pt (try (.getLocationOnScreen editor) (catch Exception _ nil))
         popup-y (if (or (instance? JTextField editor)
                         (and pt r
                              (let [screen-bounds (try (.. editor getGraphicsConfiguration getBounds)
                                                       (catch Exception _ nil))
                                    screen-insets (try (.. Toolkit getDefaultToolkit (getScreenInsets (.getGraphicsConfiguration editor)))
                                                       (catch Exception _ (Insets. 0 0 0 0)))
                                    screen-bottom (if screen-bounds
                                                    (- (.getMaxY screen-bounds) (or (some-> screen-insets .bottom) 0))
                                                    Double/MAX_VALUE)
                                    caret-bottom-screen (+ (.y pt) raw-y)]
                                (> (+ caret-bottom-screen popup-height) screen-bottom))))
                   (- (if r (int (.getY r)) 0) popup-height 2)
                   raw-y)
         editor-width (.getWidth editor)
         popup-x (if (and (pos? editor-width) (> (+ x 620) editor-width))
                   (max 0 (- editor-width 630))
                   x)
         state {:popup popup
                :list list
                :model model
                :doc-area doc-area
                :start start
                :word-end word-end
                :status-fn status-fn
                :highlight-fn highlight-now!
                :dirty-fn update-dirty!
                :editor editor
                :extra-context extra-context
                :just-committed-atom just-committed-atom}]
     (reset! active-popup-atom state)
     (.addPopupMenuListener popup
       (reify PopupMenuListener
         (popupMenuWillBecomeInvisible [this e]
           (reset! active-popup-atom nil))
         (popupMenuCanceled [this e]
           (reset! active-popup-atom nil))
         (popupMenuWillBecomeVisible [this e])))
     (.addMouseListener list
       (proxy [MouseAdapter] []
         (mouseClicked [^MouseEvent e]
           (when (= (.getClickCount e) 2)
             (commit-autocomplete! active-popup-atom)))))
     (try
       (.show popup editor popup-x popup-y)
       (catch Exception _ nil))
     (.requestFocusInWindow editor))))

(defn- apply-autocomplete-results!
  [^JTextComponent editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom]
  (cond
    (empty? candidates)
    (do
      (dismiss-autocomplete! active-popup-atom)
      (let [doc (.getDocument editor)
            caret (.getCaretPosition editor)
            prefix-info (syntax/get-symbol-prefix-at-pos (.getText doc 0 (.getLength doc)) caret)
            pfx (:prefix prefix-info)]
        (if (str/blank? pfx)
          (when status-fn (status-fn "No completions available."))
          (when status-fn (status-fn (format "No completions found for '%s'" pfx)))))
      (try (.. Toolkit getDefaultToolkit beep) (catch Exception _ nil))
      :no-match)

    (= (count candidates) 1)
    (let [sym (:symbol (first candidates))]
      (dismiss-autocomplete! active-popup-atom)
      (.setSelectionStart editor (int start))
      (.setSelectionEnd editor (int word-end))
      (.replaceSelection editor sym)
      (.setCaretPosition editor (+ (int start) (count sym)))
      (when update-dirty! (update-dirty!))
      (when highlight-now! (highlight-now!))
      (when status-fn (status-fn (str "Completed: " sym)))
      :single-match)

    :else
    (do
      (show-autocomplete-popup! editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom)
      :multi-match)))

(defn trigger-autocomplete!
  ([^JTextComponent editor status-fn active-popup-atom highlight-now! update-dirty!]
   (trigger-autocomplete! editor status-fn active-popup-atom highlight-now! update-dirty! nil nil nil))
  ([^JTextComponent editor status-fn active-popup-atom highlight-now! update-dirty! extra-context]
   (trigger-autocomplete! editor status-fn active-popup-atom highlight-now! update-dirty! extra-context nil nil))
  ([^JTextComponent editor status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom]
   (trigger-autocomplete! editor status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom nil))
  ([^JTextComponent editor status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom opts]
   (let [async? (if (and (map? opts) (contains? opts :async?))
                  (:async? opts)
                  (SwingUtilities/isEventDispatchThread))
         doc (.getDocument editor)
         text (.getText doc 0 (.getLength doc))
         caret (.getCaretPosition editor)
         prefix-info (syntax/get-symbol-prefix-at-pos text caret)
         prefix (:prefix prefix-info)
         start (:start prefix-info)
         word-end (:word-end prefix-info)]
     (if-not async?
       (let [candidates (syntax/get-autocomplete-candidates prefix text caret extra-context)]
         (apply-autocomplete-results! editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom))
       (let [req-id (.incrementAndGet autocomplete-request-seq)]
         (.submit ^ExecutorService autocomplete-executor
           ^Runnable (fn []
                       (let [candidates (syntax/get-autocomplete-candidates prefix text caret extra-context)]
                         (SwingUtilities/invokeLater
                           (fn []
                             (when (= req-id (.get autocomplete-request-seq))
                               (let [cur-doc (.getDocument editor)
                                     cur-txt (.getText cur-doc 0 (.getLength cur-doc))
                                     cur-caret (.getCaretPosition editor)]
                                 (if (and (= cur-txt text) (= cur-caret caret))
                                   (apply-autocomplete-results! editor candidates start word-end status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom)
                                   (let [cur-prefix-info (syntax/get-symbol-prefix-at-pos cur-txt cur-caret)
                                         cur-prefix (:prefix cur-prefix-info)
                                         cur-start (:start cur-prefix-info)]
                                     (when (and (= cur-start start) (>= cur-caret start) (str/starts-with? cur-prefix prefix))
                                       (let [filtered (vec (filter (fn [{:keys [symbol]}]
                                                                     (str/starts-with? (str/lower-case symbol) (str/lower-case cur-prefix)))
                                                                   candidates))]
                                         (when (pos? (count filtered))
                                           (show-autocomplete-popup! editor filtered start (:word-end cur-prefix-info) status-fn active-popup-atom highlight-now! update-dirty! extra-context just-committed-atom))))))))))))))))))

(defn setup-autocomplete-keys!
  [^JTextComponent editor active-popup-atom]
  (.addKeyListener editor
    (proxy [KeyAdapter] []
      (keyPressed [^KeyEvent e]
        (when-let [st @active-popup-atom]
          (let [code (.getKeyCode e)]
            (cond
              (or (= code KeyEvent/VK_ENTER) (= code KeyEvent/VK_TAB))
              (do
                (.consume e)
                (commit-autocomplete! active-popup-atom))

              (= code KeyEvent/VK_ESCAPE)
              (do
                (.consume e)
                (dismiss-autocomplete! active-popup-atom))

              (= code KeyEvent/VK_UP)
              (do
                (.consume e)
                (move-popup-selection! active-popup-atom -1))

              (= code KeyEvent/VK_DOWN)
              (do
                (.consume e)
                (move-popup-selection! active-popup-atom 1))

              (= code KeyEvent/VK_PAGE_UP)
              (do
                (.consume e)
                (move-popup-selection! active-popup-atom -6))

              (= code KeyEvent/VK_PAGE_DOWN)
              (do
                (.consume e)
                (move-popup-selection! active-popup-atom 6))

              :else nil))))

      (keyReleased [^KeyEvent e]
        (when-let [st @active-popup-atom]
          (let [code (.getKeyCode e)]
            (when-not (contains? #{KeyEvent/VK_UP KeyEvent/VK_DOWN KeyEvent/VK_PAGE_UP KeyEvent/VK_PAGE_DOWN
                                   KeyEvent/VK_ENTER KeyEvent/VK_TAB KeyEvent/VK_ESCAPE
                                   KeyEvent/VK_SHIFT KeyEvent/VK_CONTROL KeyEvent/VK_ALT KeyEvent/VK_META}
                                 code)
              (update-autocomplete-filter! active-popup-atom))))))))

(defn setup-editor-context-menu!
  ([^JTextComponent editor jump-fn! rename-fn! doc-fn! comment-fn! find-fn! replace-fn!]
   (setup-editor-context-menu! editor jump-fn! rename-fn! doc-fn! comment-fn! nil nil nil find-fn! replace-fn!))
  ([^JTextComponent editor jump-fn! rename-fn! doc-fn! comment-fn! format-all-fn! format-sel-fn! find-fn! replace-fn!]
   (setup-editor-context-menu! editor jump-fn! rename-fn! doc-fn! comment-fn! format-all-fn! format-sel-fn! nil find-fn! replace-fn!))
  ([^JTextComponent editor jump-fn! rename-fn! doc-fn! comment-fn! format-all-fn! format-sel-fn! autocomplete-fn! find-fn! replace-fn!]
   (let [popup (JPopupMenu.)
         item-jump (JMenuItem. "Jump to Definition (F12)")
         item-rename (JMenuItem. "Rename Symbol... (Shift+F6)")
         item-doc (JMenuItem. "Quick Documentation (Ctrl+Q)")
         item-autocomplete (JMenuItem. "Autocomplete (Ctrl+Space)")
         item-format-all (JMenuItem. "Format All (Ctrl+Shift+F)")
         item-format-sel (JMenuItem. "Format Selection (Ctrl+Alt+F)")
         item-comment (JMenuItem. "Toggle Comment (Ctrl+/)")
         item-indent (JMenuItem. "Indent Selection (Tab)")
         item-unindent (JMenuItem. "Unindent Selection (Shift+Tab)")
         item-find (JMenuItem. "Find... (Ctrl+F)")
         item-replace (JMenuItem. "Replace... (Ctrl+H)")
         item-cut (JMenuItem. "Cut")
         item-copy (JMenuItem. "Copy")
         item-paste (JMenuItem. "Paste")]
     (.addActionListener item-jump (proxy [ActionListener] [] (actionPerformed [e] (when jump-fn! (jump-fn!)))))
     (.addActionListener item-rename (proxy [ActionListener] [] (actionPerformed [e] (when rename-fn! (rename-fn!)))))
     (.addActionListener item-doc (proxy [ActionListener] [] (actionPerformed [e] (when doc-fn! (doc-fn!)))))
     (.addActionListener item-autocomplete (proxy [ActionListener] [] (actionPerformed [e] (when autocomplete-fn! (autocomplete-fn!)))))
     (.addActionListener item-format-all (proxy [ActionListener] [] (actionPerformed [e] (if format-all-fn! (format-all-fn!) (format-all! editor)))))
     (.addActionListener item-format-sel (proxy [ActionListener] [] (actionPerformed [e] (if format-sel-fn! (format-sel-fn!) (format-selection! editor)))))
     (.addActionListener item-comment (proxy [ActionListener] [] (actionPerformed [e] (when comment-fn! (comment-fn!)))))
     (.addActionListener item-indent (proxy [ActionListener] [] (actionPerformed [e] (indent-selection! editor))))
     (.addActionListener item-unindent (proxy [ActionListener] [] (actionPerformed [e] (unindent-selection! editor))))
     (.addActionListener item-find (proxy [ActionListener] [] (actionPerformed [e] (when find-fn! (find-fn!)))))
     (.addActionListener item-replace (proxy [ActionListener] [] (actionPerformed [e] (when replace-fn! (replace-fn!)))))
     (.addActionListener item-cut (proxy [ActionListener] [] (actionPerformed [e] (.cut editor))))
     (.addActionListener item-copy (proxy [ActionListener] [] (actionPerformed [e] (.copy editor))))
     (.addActionListener item-paste (proxy [ActionListener] [] (actionPerformed [e] (.paste editor))))
     (.add popup item-jump)
     (.add popup item-rename)
     (.add popup item-doc)
     (.add popup item-autocomplete)
     (.addSeparator popup)
     (.add popup item-format-all)
     (.add popup item-format-sel)
     (.add popup item-comment)
     (.add popup item-indent)
     (.add popup item-unindent)
     (.addSeparator popup)
     (.add popup item-find)
     (.add popup item-replace)
     (.addSeparator popup)
     (.add popup item-cut)
     (.add popup item-copy)
     (.add popup item-paste)
     (.addMouseListener editor
       (proxy [MouseAdapter] []
         (mousePressed [^MouseEvent e]
           (when (SwingUtilities/isRightMouseButton e)
             (when (str/blank? (.getSelectedText editor))
               (let [pt (.getPoint e)
                     pos (try (.viewToModel2D editor pt)
                              (catch Exception _
                                (.viewToModel editor pt)))]
                 (when (and (number? pos) (>= pos 0))
                   (.setCaretPosition editor (int pos)))))))))
     (.setComponentPopupMenu editor popup))))

(defn setup-interactions-context-menu!
  "Right-click context menu for the integrated interactions console."
  [^JTextPane console autocomplete-fn! clear-fn!]
  (let [popup (JPopupMenu.)
        item-autocomplete (JMenuItem. "Autocomplete (Ctrl+Space)")
        item-copy (JMenuItem. "Copy")
        item-paste (JMenuItem. "Paste")
        item-clear (JMenuItem. "Clear Console (Ctrl+L)")]
    (.addActionListener item-autocomplete (proxy [ActionListener] [] (actionPerformed [e] (when autocomplete-fn! (autocomplete-fn!)))))
    (.addActionListener item-copy (proxy [ActionListener] [] (actionPerformed [e] (.copy console))))
    (.addActionListener item-paste (proxy [ActionListener] [] (actionPerformed [e] (.paste console))))
    (.addActionListener item-clear (proxy [ActionListener] [] (actionPerformed [e] (when clear-fn! (clear-fn!)))))
    (.add popup item-autocomplete)
    (.addSeparator popup)
    (.add popup item-copy)
    (.add popup item-paste)
    (.addSeparator popup)
    (.add popup item-clear)
    (.setComponentPopupMenu console popup)))

(defn setup-undo! [^JTextComponent editor]
  (let [undo-mgr (javax.swing.undo.UndoManager.)
        im (.getInputMap editor)
        am (.getActionMap editor)]
    (.. editor getDocument
        (addUndoableEditListener
          (reify UndoableEditListener
            (undoableEditHappened [_ e]
              (let [edit (.getEdit e)]
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
              (.setFont text-area (Font. Font/MONOSPACED Font/PLAIN 13)))
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

;; --- Multi-Window Management ---

(def active-windows
  (atom {}))

(def exit-handler
  (atom (fn [] (System/exit 0))))

(declare open-ide-window!)

;; --- Main UI Builder ---

(defn create-ide [initial-file]
  (let [init-content (if (and (not (str/blank? initial-file)) (.exists (java.io.File. initial-file)))
                       (try (slurp initial-file :encoding "UTF-8") (catch Exception _ ""))
                       "")
        _ (when (seq init-content)
            (syntax/warm-buffer-cache-async! init-content))
        frame (proxy [JFrame] [(str "Untitled - " app-name)]
                (dispose []
                  (swap! active-windows dissoc this)
                  (proxy-super dispose)))
        eval-ctx (eval/make-eval-context)
        cur-file (atom (or initial-file ""))
        saved-content (atom init-content)
        dirty? (atom false)
        font-size (atom 14)

        ;; Definitions Editor & Line Numbers
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
        definitions-panel (JPanel. (BorderLayout.))
        _ (.add definitions-panel editor-scroll BorderLayout/CENTER)
        syntax-controller (syntax/setup-syntax-highlighting! editor {:delay-ms 60})
        highlight-now! (:highlight-now! syntax-controller)
        bracket-info (:bracket-info syntax-controller)

        ;; --- Unified Interactions Console (Output + REPL input) ---
        interactions-doc (DefaultStyledDocument.)
        interactions-pane (proxy [JTextPane] [interactions-doc]
                            (getScrollableTracksViewportWidth []
                              (let [parent (.getParent this)]
                                (if (instance? JViewport parent)
                                  (>= (.getWidth parent) (.. this getUI (getPreferredSize this) width))
                                  true))))
        _ (do (.setName interactions-pane "interactions-console")
              (.setMargin interactions-pane (Insets. 4 6 4 6))
              (.setBackground interactions-pane (Color. 250 250 250))
              (.setCaretColor interactions-pane (Color. 30 30 30)))
        interactions-scroll (JScrollPane. interactions-pane)
        interactions-panel (JPanel. (BorderLayout.))
        _ (.add interactions-panel interactions-scroll BorderLayout/CENTER)

        ;; Styles for console segments
        attr-normal (let [a (SimpleAttributeSet.)]
                      (StyleConstants/setForeground a (Color. 40 40 40))
                      (StyleConstants/setBold a false)
                      a)
        attr-prompt (let [a (SimpleAttributeSet.)]
                      (StyleConstants/setForeground a (Color. 0 100 200))
                      (StyleConstants/setBold a true)
                      a)
        attr-result (let [a (SimpleAttributeSet.)]
                      (StyleConstants/setForeground a (Color. 20 120 40))
                      (StyleConstants/setBold a true)
                      a)
        attr-error (let [a (SimpleAttributeSet.)]
                     (StyleConstants/setForeground a (Color. 180 20 20))
                     (StyleConstants/setBold a false)
                     a)
        attr-system (let [a (SimpleAttributeSet.)]
                      (StyleConstants/setForeground a (Color. 110 110 110))
                      (StyleConstants/setItalic a true)
                      a)

        prompt-start-pos (atom 0)
        current-prompt-str (atom "user=> ")

        ;; Split Pane: Top = Definitions, Bottom = Unified Interactions
        split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT definitions-panel interactions-panel)
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
        btn-run (JButton. "▶ Run (F5)")
        btn-stop (JButton. "⏹ Stop")
        _ (.setEnabled btn-stop false)
        btn-find (JButton. "Find (Ctrl+F)")
        btn-clear (JButton. "Clear Output (Ctrl+L)")
        btn-cheatsheet (JButton. "Cheatsheet (F1)")
        btn-font-plus (JButton. "A+")
        btn-font-minus (JButton. "A-")

        fc (JFileChooser.)
        _ (.setFileFilter fc (javax.swing.filechooser.FileNameExtensionFilter. "Clojure files (*.clj, *.cljc, *.edn)" (into-array ["clj" "cljc" "edn"])))

        undo-mgr (setup-undo! editor)
        active-popup (atom nil)
        input-autocomplete-committed (atom false)]

    ;; --- Setup Fonts ---
    (letfn [(apply-font! [sz]
              (let [f (Font. Font/MONOSPACED Font/PLAIN sz)]
                (.setFont editor f)
                (.setFont line-numbers f)
                (.setFont interactions-pane f)
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

              ;; Console text append helpers
              (append-styled-text! [text attr]
                (let [len (.getLength interactions-doc)]
                  (.insertString interactions-doc len text attr)
                  (reset! prompt-start-pos (.getLength interactions-doc))
                  (.setCaretPosition interactions-pane @prompt-start-pos)))

              (append-console! [text]
                (SwingUtilities/invokeLater
                  (fn []
                    (let [len (.getLength interactions-doc)
                          target-pos @prompt-start-pos]
                      (if (>= target-pos len)
                        (do
                          (.insertString interactions-doc len text attr-normal)
                          (reset! prompt-start-pos (.getLength interactions-doc)))
                        (do
                          (.insertString interactions-doc target-pos text attr-normal)
                          (swap! prompt-start-pos + (.length ^String text))))
                      (.setCaretPosition interactions-pane (.getLength interactions-doc))))))

              (insert-new-prompt! [prompt-str]
                (SwingUtilities/invokeLater
                  (fn []
                    (let [len (.getLength interactions-doc)]
                      (when (and (pos? len)
                                 (not= (.getText interactions-doc (dec len) 1) "\n"))
                        (.insertString interactions-doc len "\n" attr-normal))
                      (let [new-len (.getLength interactions-doc)]
                        (reset! current-prompt-str prompt-str)
                        (.insertString interactions-doc new-len prompt-str attr-prompt)
                        (reset! prompt-start-pos (.getLength interactions-doc))
                        (.setCaretPosition interactions-pane @prompt-start-pos))))))

              (get-current-command []
                (let [p-start @prompt-start-pos
                      len (.getLength interactions-doc)]
                  (if (> len p-start)
                    (.getText interactions-doc p-start (- len p-start))
                    "")))

              (set-current-command! [new-cmd]
                (let [p-start @prompt-start-pos
                      len (.getLength interactions-doc)]
                  (when (> len p-start)
                    (.remove interactions-doc p-start (- len p-start)))
                  (.insertString interactions-doc p-start new-cmd attr-normal)
                  (.setCaretPosition interactions-pane (.getLength interactions-doc))))]

        ;; Banner Initialization
        (letfn [(print-banner! []
                  (let [c-ver (clojure-version)
                        j-ver (System/getProperty "java.version")
                        banner (str "============================================================\n"
                                    " " app-name " " app-version "  |  Clojure " c-ver "  |  Java " j-ver "\n"
                                    "============================================================\n"
                                    " Definitions (Top): Type Clojure code and press [Run] or F5\n"
                                    " Interactions (Bottom): Type Clojure expressions or standard input here\n"
                                    " Stdin Console: Use (read-line) in code; input is entered directly at prompt\n"
                                    " Cheatsheet: Click [Cheatsheet] or press F1 to open Clojure Cheatsheet\n"
                                    "------------------------------------------------------------\n")]
                    (SwingUtilities/invokeLater
                      (fn []
                        (.setText interactions-pane "")
                        (append-styled-text! banner attr-system)
                        (insert-new-prompt! "user=> ")))))]
          (print-banner!)

          ;; Status Updater
          (letfn [(set-status! [st]
                    (SwingUtilities/invokeLater
                      (fn []
                        (cond
                          (= st :running)
                          (do (.setText status-state " ● Running... ")
                              (.setForeground status-state (Color. 200 100 0))
                              (.setEnabled btn-run false)
                              (.setEnabled btn-stop true))
                          (= st :waiting-input)
                          (do (.setText status-state " ● Waiting for input... ")
                              (.setForeground status-state (Color. 0 100 220))                              
                              (.requestFocusInWindow interactions-pane))
                          (= st :idle)
                          (do (.setText status-state " ● Ready ")
                              (.setForeground status-state (Color. 0 130 0))
                              (.setEnabled btn-run true)
                              (.setEnabled btn-stop false))
                          (string? st)
                          (do (.setText status-state (str " " st " "))
                              (.setForeground status-state (Color. 0 100 180)))
                          :else nil))))]

            ;; --- Evaluation Logic ---
            (letfn [(run-code-string! [code-str]
                      (eval/eval-async eval-ctx code-str
                        {:on-output append-console!
                         :on-status-change set-status!
                         :on-complete (fn [res]
                                        (SwingUtilities/invokeLater
                                          (fn []
                                            (case (:status res)
                                              :ok
                                              (append-styled-text! (str "=> " (eval/format-value (:value res)) "\n") attr-result)
                                              :error
                                              (append-styled-text! (str (eval/format-error (:error res)) "\n") attr-error)
                                              :interrupted
                                              (append-styled-text! "; [Evaluation stopped by user]\n" attr-system)
                                              nil)
                                            (when-not (eval/waiting-for-input? eval-ctx)
                                              (insert-new-prompt! "user=> "))
                                            (.requestFocusInWindow interactions-pane))))}))

                    (run-definitions! []
                      (let [code (.getText editor)]
                        (SwingUtilities/invokeLater
                          (fn []
                            (append-styled-text! "\n; --- Running Definitions ---\n" attr-system)
                            (run-code-string! code)))))

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
                          (SwingUtilities/invokeLater
                            (fn []
                              (append-styled-text! (str "\n; --- Running Selection ---\n" code "\n") attr-system)
                              (run-code-string! code))))))

                    (stop-current-eval! []
                      (eval/stop-eval! eval-ctx)
                      (set-status! :idle)
                      (insert-new-prompt! "user=> "))

                    (jump-action! []
                      (jump-to-definition! frame editor))

                    (rename-action! []
                      (rename-symbol-dialog! frame editor highlight-now! update-title! set-status!))

                    (comment-action! []
                      (toggle-comment! editor)
                      (update-dirty!)
                      (highlight-now!))

                    (format-all-action! []
                      (format-all! editor set-status!)
                      (update-dirty!)
                      (highlight-now!))

                    (format-selection-action! []
                      (format-selection! editor set-status!)
                      (update-dirty!)
                      (highlight-now!))

                    (quick-doc-action! []
                      (show-quick-doc! frame editor))

                    (console-autocomplete-action! []
                      (let [editor-text (fn [] (.getText (.getDocument editor) 0 (.getLength (.getDocument editor))))]
                        (trigger-autocomplete! interactions-pane set-status! active-popup nil nil editor-text input-autocomplete-committed)))

                    (autocomplete-action! []
                      (if (.hasFocus interactions-pane)
                        (console-autocomplete-action!)
                        (trigger-autocomplete! editor set-status! active-popup highlight-now! update-dirty!)))

                    (send-stdin-eof! []
                      (when (eval/waiting-for-input? eval-ctx)
                        (when @active-popup
                          (dismiss-autocomplete! active-popup))
                        (let [text (get-current-command)]
                          (append-styled-text! "\n" attr-normal)
                          (when-not (empty? text)
                            (eval/push-stdin! eval-ctx text))
                          (eval/push-stdin-eof! eval-ctx))))]

              (let [find-ctrl (create-find-replace-panel editor highlight-now! update-title! set-status!)
                    find-panel (:panel find-ctrl)
                    open-find! (:open-find! find-ctrl)
                    open-replace! (:open-replace! find-ctrl)
                    close-find! (:close-panel! find-ctrl)
                    find-next! (:find-next! find-ctrl)
                    find-prev! (:find-prev! find-ctrl)]
                (.add definitions-panel find-panel BorderLayout/SOUTH)

                ;; --- Interactions Console Key Bindings & Stdin Handlers ---
                (.setFocusTraversalKeys interactions-pane KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS java.util.Collections/EMPTY_SET)
                (setup-autocomplete-keys! interactions-pane active-popup)
                (setup-interactions-context-menu! interactions-pane console-autocomplete-action! print-banner!)

                (let [im (.getInputMap interactions-pane JComponent/WHEN_FOCUSED)
                      am (.getActionMap interactions-pane)]
                  (.put im (KeyStroke/getKeyStroke "control SPACE") "console-autocomplete")
                  (.put im (KeyStroke/getKeyStroke KeyEvent/VK_SPACE KeyEvent/CTRL_DOWN_MASK) "console-autocomplete")
                  (.put im (KeyStroke/getKeyStroke "TAB") "console-autocomplete")
                  (.put am "console-autocomplete"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e]
                        (if @active-popup
                          (commit-autocomplete! active-popup)
                          (console-autocomplete-action!)))))

                  (.put im (KeyStroke/getKeyStroke "control L") "clear-console")
                  (.put am "clear-console"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e]
                        (print-banner!))))

                  (.put im (KeyStroke/getKeyStroke "control D") "stdin-eof")
                  (.put im (KeyStroke/getKeyStroke KeyEvent/VK_D KeyEvent/CTRL_DOWN_MASK) "stdin-eof")
                  (.put am "stdin-eof"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e]
                        (send-stdin-eof!)))))

                ;; Protect prior output from edits; route Enter / Up / Down
                (.addKeyListener interactions-pane
                  (proxy [KeyAdapter] []
                    (keyPressed [e]
                      (let [code (.getKeyCode e)
                            caret (.getCaretPosition interactions-pane)
                            p-start @prompt-start-pos]
                        ;; Guard backspace/delete before prompt boundary
                        (when (and (= code KeyEvent/VK_BACK_SPACE)
                                   (<= caret p-start)
                                   (= (.getSelectionStart interactions-pane) (.getSelectionEnd interactions-pane)))
                          (.consume e))
                        (when (and (= code KeyEvent/VK_DELETE)
                                   (< caret p-start))
                          (.consume e))

                        ;; Ctrl+D stdin EOF
                        (when (and (= code KeyEvent/VK_D)
                                   (pos? (bit-and (.getModifiersEx e) KeyEvent/CTRL_DOWN_MASK)))
                          (when (eval/waiting-for-input? eval-ctx)
                            (.consume e)
                            (send-stdin-eof!)))

                        ;; Enter: submit code or standard input
                        (when (= code KeyEvent/VK_ENTER)
                          (when-not (or @input-autocomplete-committed @active-popup)
                            (.consume e)
                            (let [cmd (get-current-command)]
                              ;; Append newline and move prompt-start-pos so stdout starts on new line
                              (append-styled-text! "\n" attr-normal)
                              (if (eval/waiting-for-input? eval-ctx)
                                (eval/push-stdin! eval-ctx cmd)
                                (if (eval/evaluating? eval-ctx)
                                  (do
                                    (append-styled-text! "; [Warning: Code is currently running. Press Stop to cancel.]\n" attr-system)
                                    (insert-new-prompt! "user=> "))
                                  (if (str/blank? cmd)
                                    (insert-new-prompt! "user=> ")
                                    (do
                                      (swap! (:history eval-ctx) eval/history-add cmd)
                                      (run-code-string! cmd))))))))

                        ;; History traversal on Up / Down
                        (when (and (not @active-popup) (= code KeyEvent/VK_UP))
                          (.consume e)
                          (let [[new-hist display-text] (eval/history-prev @(:history eval-ctx) (get-current-command))]
                            (reset! (:history eval-ctx) new-hist)
                            (set-current-command! display-text)))

                        (when (and (not @active-popup) (= code KeyEvent/VK_DOWN))
                          (.consume e)
                          (let [[new-hist display-text] (eval/history-next @(:history eval-ctx))]
                            (reset! (:history eval-ctx) new-hist)
                            (set-current-command! display-text)))))

                    (keyTyped [e]
                      (let [caret (.getCaretPosition interactions-pane)
                            p-start @prompt-start-pos]
                        ;; Prevent typing prior to prompt
                        (when (and (< caret p-start)
                                   (not (contains? #{KeyEvent/VK_BACK_SPACE KeyEvent/VK_DELETE} (int (.getKeyChar e)))))
                          (.setCaretPosition interactions-pane (.getLength interactions-doc)))))))

                ;; Wire toolbar buttons
                (.addActionListener btn-run (proxy [ActionListener] [] (actionPerformed [e] (run-definitions!))))
                (.addActionListener btn-stop (proxy [ActionListener] [] (actionPerformed [e] (stop-current-eval!))))
                (.addActionListener btn-find (proxy [ActionListener] [] (actionPerformed [e] (open-find!))))
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

                ;; Setup Editor Listeners & Shortcuts
                (setup-bracket-matching! editor bracket-info)
                (setup-auto-brackets! editor)
                (setup-editor-keys! editor comment-action!)
                (setup-autocomplete-keys! editor active-popup)

                (let [im (.getInputMap editor)
                      am (.getActionMap editor)]
                  (.put im (KeyStroke/getKeyStroke "F5") "run-definitions")
                  (.put am "run-definitions"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (run-definitions!))))

                  (.put im (KeyStroke/getKeyStroke "F12") "jump-to-def")
                  (.put im (KeyStroke/getKeyStroke "control B") "jump-to-def")
                  (.put am "jump-to-def"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (jump-action!))))

                  (.put im (KeyStroke/getKeyStroke "shift F6") "rename-symbol")
                  (.put im (KeyStroke/getKeyStroke "F2") "rename-symbol")
                  (.put im (KeyStroke/getKeyStroke KeyEvent/VK_F6 KeyEvent/SHIFT_DOWN_MASK) "rename-symbol")
                  (.put am "rename-symbol"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (rename-action!))))

                  (.put im (KeyStroke/getKeyStroke "control Q") "quick-doc")
                  (.put im (KeyStroke/getKeyStroke "shift F1") "quick-doc")
                  (.put am "quick-doc"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (quick-doc-action!))))

                  (.put im (KeyStroke/getKeyStroke "control SPACE") "autocomplete")
                  (.put im (KeyStroke/getKeyStroke KeyEvent/VK_SPACE KeyEvent/CTRL_DOWN_MASK) "autocomplete")
                  (.put am "autocomplete"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (autocomplete-action!))))

                  (.put im (KeyStroke/getKeyStroke "control F") "open-find")
                  (.put am "open-find"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (open-find!))))

                  (.put im (KeyStroke/getKeyStroke "control H") "open-replace")
                  (.put im (KeyStroke/getKeyStroke "control R") "open-replace")
                  (.put am "open-replace"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (open-replace!))))

                  (.put im (KeyStroke/getKeyStroke "F3") "find-next")
                  (.put am "find-next"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (find-next!))))

                  (.put im (KeyStroke/getKeyStroke "shift F3") "find-prev")
                  (.put am "find-prev"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (find-prev!))))

                  (.put im (KeyStroke/getKeyStroke "control shift F") "format-all")
                  (.put am "format-all"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (format-all-action!))))

                  (.put im (KeyStroke/getKeyStroke "control alt F") "format-selection")
                  (.put im (KeyStroke/getKeyStroke "control alt L") "format-selection")
                  (.put am "format-selection"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e] (format-selection-action!))))

                  (.put im (KeyStroke/getKeyStroke "ESCAPE") "editor-escape")
                  (.put am "editor-escape"
                    (proxy [javax.swing.AbstractAction] []
                      (actionPerformed [e]
                        (if (.isVisible find-panel)
                          (close-find!)
                          (stop-current-eval!))))))

                (setup-editor-context-menu! editor jump-action! rename-action! quick-doc-action! comment-action! format-all-action! format-selection-action! autocomplete-action! open-find! open-replace!)

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
                            (spit file-path text :encoding "UTF-8")
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
                        (open-ide-window! nil frame))

                      (close-window! []
                        (if (= (prompt-save-if-dirty!) :proceed)
                          (do
                            (dismiss-autocomplete! active-popup)
                            (swap! active-windows dissoc frame)
                            (.dispose frame)
                            (when (empty? @active-windows)
                              (@exit-handler))
                            :proceed)
                          :cancel))

                      (exit-application! []
                        (let [windows (vals @active-windows)
                              can-exit? (loop [ws windows]
                                          (if (empty? ws)
                                            true
                                            (let [{:keys [prompt-save-fn]} (first ws)]
                                              (if (= (prompt-save-fn) :proceed)
                                                (recur (rest ws))
                                                false))))]
                          (when can-exit?
                            (doseq [{:keys [^JFrame frame]} (vals @active-windows)]
                              (try (.dispose frame) (catch Throwable _ nil)))
                            (reset! active-windows {})
                            (@exit-handler)
                            :proceed)))

                      (file-open! []
                        (when (= (prompt-save-if-dirty!) :proceed)
                          (when (= (.showOpenDialog fc frame) JFileChooser/APPROVE_OPTION)
                            (let [f (.getSelectedFile fc)
                                  path (.getCanonicalPath f)]
                              (try
                                (let [content (slurp path :encoding "UTF-8")]
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
                                    "Error" JOptionPane/ERROR_MESSAGE)))))))]

                (swap! active-windows assoc frame
                  {:frame frame
                   :prompt-save-fn prompt-save-if-dirty!
                   :close-fn close-window!
                   :file-new-fn file-new!
                   :cur-file-atom cur-file
                   :dirty?-atom dirty?})

                (when-not (empty? init-content)
                  (highlight-now!))

                (update-title!)
                (update-line-numbers! editor line-numbers)

                ;; --- Menu Bar ---
                (let [menu-bar (JMenuBar.)
                      menu-file (JMenu. "File")
                      _ (.setMnemonic menu-file (int \F))
                      item-new (JMenuItem. "New" (int \N))
                      item-open (JMenuItem. "Open..." (int \O))
                      item-save (JMenuItem. "Save" (int \S))
                      item-save-as (JMenuItem. "Save As..." (int \A))
                      item-close (JMenuItem. "Close Window" (int \W))
                      item-exit (JMenuItem. "Exit" (int \X))
                      _ (do (.setAccelerator item-new (KeyStroke/getKeyStroke "control N"))
                            (.setAccelerator item-open (KeyStroke/getKeyStroke "control O"))
                            (.setAccelerator item-save (KeyStroke/getKeyStroke "control S"))
                            (.setAccelerator item-save-as (KeyStroke/getKeyStroke "control shift S"))
                            (.setAccelerator item-close (KeyStroke/getKeyStroke "control W"))
                            (.addActionListener item-new (proxy [ActionListener] [] (actionPerformed [e] (file-new!))))
                            (.addActionListener item-open (proxy [ActionListener] [] (actionPerformed [e] (file-open!))))
                            (.addActionListener item-save (proxy [ActionListener] [] (actionPerformed [e] (file-save!))))
                            (.addActionListener item-save-as (proxy [ActionListener] [] (actionPerformed [e] (file-save-as!))))
                            (.addActionListener item-close (proxy [ActionListener] [] (actionPerformed [e] (close-window!))))
                            (.addActionListener item-exit (proxy [ActionListener] [] (actionPerformed [e] (exit-application!))))
                            (.add menu-file item-new)
                            (.add menu-file item-open)
                            (.add menu-file item-save)
                            (.add menu-file item-save-as)
                            (.addSeparator menu-file)
                            (.add menu-file item-close)
                            (.add menu-file item-exit))

                      menu-edit (JMenu. "Edit")
                      _ (.setMnemonic menu-edit (int \E))
                      item-undo (JMenuItem. "Undo")
                      item-redo (JMenuItem. "Redo")
                      item-find (JMenuItem. "Find...")
                      item-replace (JMenuItem. "Replace...")
                      item-find-next (JMenuItem. "Find Next")
                      item-find-prev (JMenuItem. "Find Previous")
                      item-jump (JMenuItem. "Jump to Definition")
                      item-rename (JMenuItem. "Rename Symbol...")
                      item-doc (JMenuItem. "Quick Documentation")
                      item-autocomplete (JMenuItem. "Autocomplete")
                      item-format-all (JMenuItem. "Format All")
                      item-format-sel (JMenuItem. "Format Selection")
                      item-comment (JMenuItem. "Toggle Comment")
                      item-indent (JMenuItem. "Indent Selection")
                      item-unindent (JMenuItem. "Unindent Selection")
                      item-clear (JMenuItem. "Clear Output")
                      _ (do (.setAccelerator item-undo (KeyStroke/getKeyStroke "control Z"))
                            (.setAccelerator item-redo (KeyStroke/getKeyStroke "control Y"))
                            (.setAccelerator item-find (KeyStroke/getKeyStroke "control F"))
                            (.setAccelerator item-replace (KeyStroke/getKeyStroke "control H"))
                            (.setAccelerator item-find-next (KeyStroke/getKeyStroke "F3"))
                            (.setAccelerator item-find-prev (KeyStroke/getKeyStroke "shift F3"))
                            (.setAccelerator item-jump (KeyStroke/getKeyStroke "F12"))
                            (.setAccelerator item-rename (KeyStroke/getKeyStroke "shift F6"))
                            (.setAccelerator item-doc (KeyStroke/getKeyStroke "control Q"))
                            (.setAccelerator item-autocomplete (KeyStroke/getKeyStroke "control SPACE"))
                            (.setAccelerator item-format-all (KeyStroke/getKeyStroke "control shift F"))
                            (.setAccelerator item-format-sel (KeyStroke/getKeyStroke "control alt F"))
                            (.setAccelerator item-comment (KeyStroke/getKeyStroke "control SLASH"))
                            (.setAccelerator item-indent (KeyStroke/getKeyStroke "TAB"))
                            (.setAccelerator item-unindent (KeyStroke/getKeyStroke "shift TAB"))
                            (.setAccelerator item-clear (KeyStroke/getKeyStroke "control L"))
                            (.addActionListener item-undo (proxy [ActionListener] [] (actionPerformed [e] (when (.canUndo undo-mgr) (.undo undo-mgr)))))
                            (.addActionListener item-redo (proxy [ActionListener] [] (actionPerformed [e] (when (.canRedo undo-mgr) (.redo undo-mgr)))))
                            (.addActionListener item-find (proxy [ActionListener] [] (actionPerformed [e] (open-find!))))
                            (.addActionListener item-replace (proxy [ActionListener] [] (actionPerformed [e] (open-replace!))))
                            (.addActionListener item-find-next (proxy [ActionListener] [] (actionPerformed [e] (find-next!))))
                            (.addActionListener item-find-prev (proxy [ActionListener] [] (actionPerformed [e] (find-prev!))))
                            (.addActionListener item-jump (proxy [ActionListener] [] (actionPerformed [e] (jump-action!))))
                            (.addActionListener item-rename (proxy [ActionListener] [] (actionPerformed [e] (rename-action!))))
                            (.addActionListener item-doc (proxy [ActionListener] [] (actionPerformed [e] (quick-doc-action!))))
                            (.addActionListener item-autocomplete (proxy [ActionListener] [] (actionPerformed [e] (autocomplete-action!))))
                            (.addActionListener item-format-all (proxy [ActionListener] [] (actionPerformed [e] (format-all-action!))))
                            (.addActionListener item-format-sel (proxy [ActionListener] [] (actionPerformed [e] (format-selection-action!))))
                            (.addActionListener item-comment (proxy [ActionListener] [] (actionPerformed [e] (comment-action!))))
                            (.addActionListener item-indent (proxy [ActionListener] [] (actionPerformed [e] (indent-selection! editor))))
                            (.addActionListener item-unindent (proxy [ActionListener] [] (actionPerformed [e] (unindent-selection! editor))))
                            (.addActionListener item-clear (proxy [ActionListener] [] (actionPerformed [e] (print-banner!))))
                            (.add menu-edit item-undo)
                            (.add menu-edit item-redo)
                            (.addSeparator menu-edit)
                            (.add menu-edit item-find)
                            (.add menu-edit item-replace)
                            (.add menu-edit item-find-next)
                            (.add menu-edit item-find-prev)
                            (.addSeparator menu-edit)
                            (.add menu-edit item-jump)
                            (.add menu-edit item-rename)
                            (.add menu-edit item-doc)
                            (.add menu-edit item-autocomplete)
                            (.addSeparator menu-edit)
                            (.add menu-edit item-format-all)
                            (.add menu-edit item-format-sel)
                            (.add menu-edit item-comment)
                            (.add menu-edit item-indent)
                            (.add menu-edit item-unindent)
                            (.addSeparator menu-edit)
                            (.add menu-edit item-clear))

                      menu-run (JMenu. "Run")
                      _ (.setMnemonic menu-run (int \R))
                      item-run-def (JMenuItem. "Run Definitions")
                      item-run-sel (JMenuItem. "Run Selection / Current Form")
                      item-stop (JMenuItem. "Stop Evaluation")
                      _ (do (.setAccelerator item-run-def (KeyStroke/getKeyStroke "F5"))
                            (.setAccelerator item-run-sel (KeyStroke/getKeyStroke "control E"))
                            (.setAccelerator item-stop (KeyStroke/getKeyStroke "ESCAPE"))
                            (.addActionListener item-run-def (proxy [ActionListener] [] (actionPerformed [e] (run-definitions!))))
                            (.addActionListener item-run-sel (proxy [ActionListener] [] (actionPerformed [e] (run-selection!))))
                            (.addActionListener item-stop (proxy [ActionListener] [] (actionPerformed [e] (stop-current-eval!))))
                            (.add menu-run item-run-def)
                            (.add menu-run item-run-sel)
                            (.add menu-run item-stop))

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

                      menu-help (JMenu. "Help")
                      _ (.setMnemonic menu-help (int \H))
                      item-cheat (JMenuItem. "Clojure Cheatsheet")
                      item-cheat-dialog (JMenuItem. "Cheatsheet Examples (Dialog)")
                      item-help-doc (JMenuItem. "Quick Documentation")
                      item-about (JMenuItem. "About DrClojure")
                      _ (do (.setAccelerator item-cheat (KeyStroke/getKeyStroke "F1"))
                            (.setAccelerator item-help-doc (KeyStroke/getKeyStroke "control Q"))
                            (.addActionListener item-cheat (proxy [ActionListener] [] (actionPerformed [e] (open-cheatsheet!))))
                            (.addActionListener item-cheat-dialog (proxy [ActionListener] [] (actionPerformed [e] (show-cheatsheet-dialog! frame editor))))
                            (.addActionListener item-help-doc (proxy [ActionListener] [] (actionPerformed [e] (quick-doc-action!))))
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
                            (.add menu-help item-help-doc)
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
                  (.add toolbar btn-find)
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

                    (.setDefaultCloseOperation frame JFrame/DO_NOTHING_ON_CLOSE)
                    (.addWindowListener frame
                      (proxy [WindowAdapter] []
                        (windowClosing [e]
                          (close-window!))
                        (windowClosed [e]
                          (swap! active-windows dissoc frame))))

                    (.setPreferredSize frame (Dimension. 950 720))
                    (.pack frame)
                    (.setLocationRelativeTo frame nil)
                    (.requestFocusInWindow editor)
                    frame)))))))))))

(defn open-ide-window!
  ([] (open-ide-window! nil nil))
  ([initial-file] (open-ide-window! initial-file nil))
  ([initial-file parent-frame]
   (let [frame (create-ide initial-file)]
     (if (and parent-frame (instance? JFrame parent-frame) (.isShowing ^JFrame parent-frame))
       (try
         (let [loc (.getLocationOnScreen ^JFrame parent-frame)
               screen-bounds (.. (GraphicsEnvironment/getLocalGraphicsEnvironment)
                                 getDefaultScreenDevice
                                 getDefaultConfiguration
                                 getBounds)
               new-x (min (+ (.x loc) 30) (max 0 (- (.width screen-bounds) 400)))
               new-y (min (+ (.y loc) 30) (max 0 (- (.height screen-bounds) 300)))]
           (.setLocation frame (max 0 (int new-x)) (max 0 (int new-y))))
         (catch Throwable _
           (try (.setLocationRelativeTo frame parent-frame) (catch Throwable _ nil))))
       (.setLocationRelativeTo frame nil))
     (.setVisible frame true)
     frame)))
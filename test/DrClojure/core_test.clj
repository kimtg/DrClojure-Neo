(ns DrClojure.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [DrClojure.core :as core]
            [DrClojure.ui :as ui])
  (:import (java.awt.event KeyEvent)
           (javax.swing JDialog JFrame JMenuItem JTextPane KeyStroke JPopupMenu JList JTextArea JTextField JComponent JLabel SwingUtilities DefaultListModel)
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
  (testing "Edit menu contains Find/Replace, Navigation, Doc, Comment, and Indent actions"
    (let [frame (ui/create-ide nil)
          menubar (.getJMenuBar frame)
          edit-menu (.getMenu menubar 1) ;; 0 = File, 1 = Edit
          item-count (.getItemCount edit-menu)
          items (set (keep (fn [i] (when-let [item (.getItem edit-menu i)] (.getText item)))
                           (range item-count)))]
      (is (contains? items "Find..."))
      (is (contains? items "Replace..."))
      (is (contains? items "Find Next"))
      (is (contains? items "Find Previous"))
      (is (contains? items "Jump to Definition"))
      (is (contains? items "Rename Symbol..."))
      (is (contains? items "Quick Documentation"))
      (is (contains? items "Format All"))
      (is (contains? items "Format Selection"))
      (is (contains? items "Toggle Comment"))
      (is (contains? items "Indent Selection"))
      (is (contains? items "Unindent Selection"))
      (.dispose frame))))

(deftest run-menu-hotkey-test
  (testing "Run Definitions hotkey accelerator is F5"
    (let [frame (ui/create-ide nil)
          menubar (.getJMenuBar frame)
          run-menu (.getMenu menubar 2) ;; 0 = File, 1 = Edit, 2 = Run
          run-item (.getItem run-menu 0)
          accel (.getAccelerator run-item)]
      (is (= "Run Definitions" (.getText run-item)))
      (is (= (KeyStroke/getKeyStroke "F5") accel))
      (.dispose frame))))

(deftest jump-to-definition-no-selection-test
  (testing "Jump to definition with caret only (no selection) on multi-line code does not modify status bar"
    (let [frame (JFrame.)
          editor (JTextPane.)
          status-atom (atom nil)
          set-status! (fn [msg] (reset! status-atom msg))
          code (str "(defn calculate-total [price tax]\n"
                    "  (+ price tax))\n\n"
                    "(defn print-receipt [item]\n"
                    "  (calculate-total 100 10))\n")]
      (.setText editor code)
      ;; Place caret on "calculate-total" on line 5 without selection
      (let [target-offset (.indexOf code "calculate-total 100")]
        (is (pos? target-offset))
        (.setCaretPosition editor target-offset)
        (is (nil? (.getSelectedText editor)))
        (ui/jump-to-definition! frame editor set-status!)
        ;; Should jump to line 1 definition and place caret at start of "calculate-total" without selecting
        (is (= 6 (.getCaretPosition editor)))
        (is (= 6 (.getSelectionStart editor)))
        (is (= 6 (.getSelectionEnd editor)))
        (is (nil? (.getSelectedText editor)))
        (is (nil? @status-atom)))))

  (testing "Jump to definition on Unicode / Korean symbol without selection does not modify status bar"
    (let [frame (JFrame.)
          editor (JTextPane.)
          status-atom (atom nil)
          set-status! (fn [msg] (reset! status-atom msg))
          code (str "(defn 넓이-계산 [가로 세로]\n"
                    "  (* 가로 세로))\n\n"
                    "(넓이-계산 10 20)\n")]
      (.setText editor code)
      (let [usage-offset (.indexOf code "(넓이-계산 10")]
        ;; Place caret on '(' immediately before 넓이-계산
        (.setCaretPosition editor usage-offset)
        (ui/jump-to-definition! frame editor set-status!)
        (is (= 6 (.getCaretPosition editor)))
        (is (= 6 (.getSelectionStart editor)))
        (is (= 6 (.getSelectionEnd editor)))
        (is (nil? (.getSelectedText editor)))
        (is (nil? @status-atom))))))

(deftest toggle-comment-test
  (testing "Single line toggle comment"
    (let [pane (JTextPane.)]
      (.setText pane "(println \"hello\")")
      (.setCaretPosition pane 3)
      (ui/toggle-comment! pane)
      (is (= "; (println \"hello\")" (.getText pane)))
      (ui/toggle-comment! pane)
      (is (= "(println \"hello\")" (.getText pane)))))

  (testing "Multi-line toggle comment"
    (let [pane (JTextPane.)]
      (.setText pane "(defn foo []\n  (+ 1 2))\n")
      (.setSelectionStart pane 0)
      (.setSelectionEnd pane (.length (.getText pane)))
      (ui/toggle-comment! pane)
      (is (= "; (defn foo []\n;   (+ 1 2))\n" (.getText pane)))
      (ui/toggle-comment! pane)
      (is (= "(defn foo []\n  (+ 1 2))\n" (.getText pane)))))

  (testing "Preserves empty lines when commenting block"
    (let [pane (JTextPane.)]
      (.setText pane "(def a 1)\n\n(def b 2)")
      (.setSelectionStart pane 0)
      (.setSelectionEnd pane (.length (.getText pane)))
      (ui/toggle-comment! pane)
      (is (= "; (def a 1)\n\n; (def b 2)" (.getText pane)))
      (ui/toggle-comment! pane)
      (is (= "(def a 1)\n\n(def b 2)" (.getText pane))))))

(deftest smart-enter-test
  (testing "Smart enter inside unclosed form indents 2 spaces from form start"
    (let [pane (JTextPane.)]
      (.setText pane "(defn square [x]")
      (.setCaretPosition pane (.length (.getText pane)))
      (ui/handle-smart-enter! pane)
      (is (= "(defn square [x]\n  " (.getText pane)))
      (is (= (count "(defn square [x]\n  ") (.getCaretPosition pane)))))

  (testing "Smart enter inside let bindings indents properly"
    (let [pane (JTextPane.)]
      (.setText pane "(let [a 1")
      (.setCaretPosition pane (.length (.getText pane)))
      (ui/handle-smart-enter! pane)
      (is (= "(let [a 1\n  " (.getText pane)))))

  (testing "Smart enter replaces selection if text was selected"
    (let [pane (JTextPane.)]
      (.setText pane "(foo [x] REPLACE_ME)")
      (let [idx (.indexOf (.getText pane) "REPLACE_ME")]
        (.setSelectionStart pane idx)
        (.setSelectionEnd pane (+ idx 10))
        (ui/handle-smart-enter! pane)
        (is (= "(foo [x] \n  )" (.getText pane))))))

  (testing "Smart enter auto-dedents when previous line closed forms"
    (let [pane (JTextPane.)]
      ;; Line with 4 spaces closes 1 form -> new line auto-dedents to 2 spaces
      (.setText pane "(defn foo [x]\n  (let [a 1]\n    (+ a 1)))")
      ;; Caret at end of '(+ a 1))' (pos 39)
      (.setCaretPosition pane 39)
      (ui/handle-smart-enter! pane)
      (is (= "(defn foo [x]\n  (let [a 1]\n    (+ a 1))\n  )" (.getText pane))))
    (let [pane (JTextPane.)]
      ;; Line with 4 spaces closes all forms -> new line auto-dedents to 0 spaces
      (.setText pane "(defn foo [x]\n  (let [a 1]\n    (+ a 1)))")
      (.setCaretPosition pane (.length (.getText pane)))
      (ui/handle-smart-enter! pane)
      (is (= "(defn foo [x]\n  (let [a 1]\n    (+ a 1)))\n" (.getText pane)))))

  (testing "Smart enter between paired delimiters expands with indented body and dedented closing bracket"
    (let [pane (JTextPane.)]
      (.setText pane "()")
      (.setCaretPosition pane 1)
      (ui/handle-smart-enter! pane)
      (is (= "(\n  \n)" (.getText pane)))
      (is (= 4 (.getCaretPosition pane))))
    (let [pane (JTextPane.)]
      (.setText pane "[]")
      (.setCaretPosition pane 1)
      (ui/handle-smart-enter! pane)
      (is (= "[\n  \n]" (.getText pane)))
      (is (= 4 (.getCaretPosition pane))))
    (let [pane (JTextPane.)]
      (.setText pane "{}")
      (.setCaretPosition pane 1)
      (ui/handle-smart-enter! pane)
      (is (= "{\n  \n}" (.getText pane)))
      (is (= 4 (.getCaretPosition pane))))
    (let [pane (JTextPane.)]
      ;; Nested between delimiters carries forward base indent + 2 and dedents closing delimiter
      (.setText pane "  (let [x 1]\n    ())")
      (.setCaretPosition pane (+ (.indexOf (.getText pane) "(" 12) 1))
      (ui/handle-smart-enter! pane)
      (is (= "  (let [x 1]\n    (\n      \n    ))" (.getText pane))))))

(deftest find-replace-panel-test
  (testing "Inline find and replace panel operations"
    (let [editor (JTextPane.)
          _ (.setText editor "(println \"apple\")\n(println \"banana\")\n(println \"apple\")\n")
          status-atom (atom nil)
          title-atom (atom nil)
          highlight-atom (atom nil)
          ctrl (ui/create-find-replace-panel
                 editor
                 (fn [] (reset! highlight-atom true))
                 (fn [] (reset! title-atom true))
                 (fn [msg] (reset! status-atom msg)))
          panel (:panel ctrl)
          find-next! (:find-next! ctrl)
          find-prev! (:find-prev! ctrl)
          open-find! (:open-find! ctrl)
          close-panel! (:close-panel! ctrl)]

      ;; Initially panel is hidden
      (is (false? (.isVisible panel)))

      ;; Open find
      (open-find!)
      (is (true? (.isVisible panel)))

      ;; Find field components
      (let [grid (.getComponent panel 0)
            row1 (.getComponent grid 0)
            row2 (.getComponent grid 1)
            find-field (.getComponent row1 1)
            match-label (.getComponent row1 5)
            replace-field (.getComponent row2 1)
            btn-replace (.getComponent row2 2)
            btn-replace-all (.getComponent row2 3)]

        ;; Search for apple
        (.setText find-field "apple")
        ;; Verify match label shows "1 of 2"
        (is (= "1 of 2" (.getText match-label)))
        (is (= "apple" (.getSelectedText editor)))

        ;; Next match
        (find-next!)
        (is (= "2 of 2" (.getText match-label)))
        (is (= "apple" (.getSelectedText editor)))
        (is (= 47 (.getSelectionStart editor)))

        ;; Prev match
        (find-prev!)
        (is (= "1 of 2" (.getText match-label)))
        (is (= 10 (.getSelectionStart editor)))

        ;; Replace current match with orange
        (.setText replace-field "orange")
        (.doClick btn-replace)
        (is (not= -1 (.indexOf (.getText editor) "orange")))

        ;; Replace all remaining apples
        (.setText find-field "apple")
        (.setText replace-field "pear")
        (.doClick btn-replace-all)
        (is (= -1 (.indexOf (.getText editor) "apple")))
        (is (not= -1 (.indexOf (.getText editor) "pear")))

        ;; Close panel
        (close-panel!)
        (is (false? (.isVisible panel)))))))

(deftest auto-brackets-test
  (testing "Auto-closing delimiter insertion and selection wrapping"
    (let [editor (JTextPane.)]
      (ui/setup-auto-brackets! editor)
      (let [kl (first (.getKeyListeners editor))]
        (is (some? kl))

        ;; 1. Open parenthesis inserts '()' and places caret inside
        (.setText editor "")
        (.setCaretPosition editor 0)
        (.keyTyped kl (KeyEvent. editor KeyEvent/KEY_TYPED (System/currentTimeMillis) 0 KeyEvent/VK_UNDEFINED \())
        (is (= "()" (.getText editor)))
        (is (= 1 (.getCaretPosition editor)))

        ;; 2. Closing parenthesis when next char matches steps over
        (.keyTyped kl (KeyEvent. editor KeyEvent/KEY_TYPED (System/currentTimeMillis) 0 KeyEvent/VK_UNDEFINED \)))
        (is (= "()" (.getText editor)))
        (is (= 2 (.getCaretPosition editor)))

        ;; 3. Selection wrapping: selecting 'hello' and typing '[' wraps to '[hello]'
        (.setText editor "hello")
        (.setSelectionStart editor 0)
        (.setSelectionEnd editor 5)
        (.keyTyped kl (KeyEvent. editor KeyEvent/KEY_TYPED (System/currentTimeMillis) 0 KeyEvent/VK_UNDEFINED \[))
        (is (= "[hello]" (.getText editor)))

        ;; 4. Paired backspace deletion: caret between '[]' deletes both
        (.setText editor "[]")
        (.setCaretPosition editor 1)
        (.keyPressed kl (KeyEvent. editor KeyEvent/KEY_PRESSED (System/currentTimeMillis) 0 KeyEvent/VK_BACK_SPACE KeyEvent/CHAR_UNDEFINED))
        (is (= "" (.getText editor)))
        (is (= 0 (.getCaretPosition editor)))))))

(deftest quick-doc-test
  (testing "Quick doc lookup for core symbol does not modify status bar"
    (let [frame (JFrame.)
          editor (JTextPane.)
          status-atom (atom nil)
          set-status! (fn [msg] (reset! status-atom msg))]
      (.setText editor "(map inc [1 2 3])")
      (.setCaretPosition editor 2) ;; On "map"
      ;; show-quick-doc! opens a non-modal JDialog
      (ui/show-quick-doc! frame editor set-status!)
      ;; Verify status bar was not changed
      (is (nil? @status-atom))
      ;; Clean up any opened dialogs
      (doseq [w (JFrame/getWindows)]
        (when (instance? JDialog w)
          (.dispose w)))
      (.dispose frame))))

(deftest file-menu-items-test
  (testing "File menu contains New, Open, Save, Save As, Close Window, and Exit actions"
    (let [frame (ui/create-ide nil)
          menubar (.getJMenuBar frame)
          file-menu (.getMenu menubar 0) ;; 0 = File
          item-count (.getItemCount file-menu)
          items (set (keep (fn [i] (when-let [item (.getItem file-menu i)] (.getText item)))
                           (range item-count)))]
      (is (contains? items "New"))
      (is (contains? items "Open..."))
      (is (contains? items "Save"))
      (is (contains? items "Save As..."))
      (is (contains? items "Close Window"))
      (is (contains? items "Exit"))
      (.dispose frame))))

(deftest multi-window-creation-test
  (testing "File -> New spawns a separate independent window and registers in active-windows"
    (let [frame1 (ui/create-ide nil)
          f1-info (get @ui/active-windows frame1)
          file-new-fn (:file-new-fn f1-info)]
      (is (some? f1-info))
      (is (fn? file-new-fn))
      ;; Trigger File -> New to spawn a second window
      (let [frame2 (file-new-fn)
            f2-info (get @ui/active-windows frame2)]
        (is (instance? JFrame frame2))
        (is (not= frame1 frame2))
        (is (some? f2-info))
        (is (contains? @ui/active-windows frame1))
        (is (contains? @ui/active-windows frame2))
        (is (= 2 (count @ui/active-windows)))
        ;; Verify independent title and dirty state
        (is (not (.startsWith (.getTitle frame1) "*")))
        (is (not (.startsWith (.getTitle frame2) "*")))
        ;; Disposing frames unregisters them from active-windows
        (.dispose frame2)
        (is (not (contains? @ui/active-windows frame2)))
        (.dispose frame1)
        (is (not (contains? @ui/active-windows frame1)))))))

(deftest multi-window-close-test
  (testing "Closing one window does not trigger exit-handler when other windows remain open"
    (let [orig-handler @ui/exit-handler
          exit-called? (atom false)
          _ (reset! ui/exit-handler (fn [] (reset! exit-called? true)))
          frame1 (ui/create-ide nil)
          frame2 (ui/open-ide-window! nil frame1)
          close-fn1 (:close-fn (get @ui/active-windows frame1))
          close-fn2 (:close-fn (get @ui/active-windows frame2))]
      (try
        (is (contains? @ui/active-windows frame1))
        (is (contains? @ui/active-windows frame2))
        ;; Close frame1
        (close-fn1)
        ;; frame1 should be removed, frame2 should still exist
        (is (not (contains? @ui/active-windows frame1)))
        (is (contains? @ui/active-windows frame2))
        ;; exit-handler should NOT have been called yet
        (is (false? @exit-called?))

        ;; Now close frame2 (the last remaining window)
        (close-fn2)
        (is (not (contains? @ui/active-windows frame2)))
        ;; Now exit-handler SHOULD have been called
        (is (true? @exit-called?))
        (finally
          (reset! ui/exit-handler orig-handler)
          (.dispose frame1)
          (.dispose frame2))))))

(deftest multi-window-independent-state-test
  (testing "Each window maintains its own independent state atoms and title"
    (let [frame1 (ui/open-ide-window! "deps.edn")
          frame2 (ui/open-ide-window! nil frame1)
          info1 (get @ui/active-windows frame1)
          info2 (get @ui/active-windows frame2)]
      (try
        (is (some? info1))
        (is (some? info2))
        (is (= "deps.edn" @(:cur-file-atom info1)))
        (is (= "" @(:cur-file-atom info2)))
        (is (false? @(:dirty?-atom info1)))
        (is (false? @(:dirty?-atom info2)))
        (is (.contains (.getTitle frame1) "deps.edn"))
        (is (.contains (.getTitle frame2) "Untitled"))
        (finally
          (.dispose frame1)
          (.dispose frame2))))))

(deftest multi-window-exit-all-test
  (testing "File -> Exit safely disposes all open windows and calls exit-handler"
    (let [orig-windows @ui/active-windows
          _ (reset! ui/active-windows {})
          orig-handler @ui/exit-handler
          exit-called? (atom false)
          _ (reset! ui/exit-handler (fn [] (reset! exit-called? true)))
          frame1 (ui/open-ide-window! nil)
          frame2 (ui/open-ide-window! nil frame1)
          menubar (.getJMenuBar frame1)
          file-menu (.getMenu menubar 0)
          exit-item (.getItem file-menu 6)] ;; index 6 is Exit (after separator)
      (try
        (is (= "Exit" (.getText exit-item)))
        (is (= 2 (count @ui/active-windows)))
        ;; Trigger Exit menu item
        (.doClick exit-item)
        ;; Both windows should be closed and removed from active-windows
        (is (= 0 (count @ui/active-windows)))
        (is (true? @exit-called?))
        (finally
          (reset! ui/exit-handler orig-handler)
          (reset! ui/active-windows orig-windows)
          (.dispose frame1)
          (.dispose frame2))))))

(deftest format-editor-test
  (testing "Format All re-indents document and preserves caret"
    (let [editor (JTextPane.)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          unindented "(defn greet [name]\n(println (str \"hello, \" name)))\n"]
      (.setText editor unindented)
      (.setCaretPosition editor 5)
      (ui/format-all! editor status-fn)
      (is (= "(defn greet [name]\n  (println (str \"hello, \" name)))\n" (.getText editor)))
      (is (= 5 (.getCaretPosition editor)))
      (is (= " Formatted entire document " @status-msg))

      ;; Calling format-all! again when already formatted
      (ui/format-all! editor status-fn)
      (is (= " Already formatted " @status-msg))))

  (testing "Format Selection only formats selected range"
    (let [editor (JTextPane.)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          code (str "(defn a []\n"
                    "1)\n"
                    "(defn b []\n"
                    "2)\n")]
      (.setText editor code)
      ;; Select (defn b [] \n 2)
      (let [b-idx (.indexOf code "(defn b")]
        (.setSelectionStart editor b-idx)
        (.setSelectionEnd editor (.length code))
        (ui/format-selection! editor status-fn)
        (is (= (str "(defn a []\n"
                    "1)\n"
                    "(defn b []\n"
                    "  2)\n")
               (.getText editor)))
        (is (= " Formatted selection " @status-msg)))))

  (testing "Format Selection without selection formats current line"
    (let [editor (JTextPane.)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          code (str "(defn foo []\n"
                    "bar)\n")]
      (.setText editor code)
      ;; Caret on "bar" without selection
      (.setCaretPosition editor (.indexOf code "bar"))
      (is (nil? (.getSelectedText editor)))
      (ui/format-selection! editor status-fn)
      (is (= "(defn foo []\n  bar)\n" (.getText editor)))
      (is (= " Formatted current line " @status-msg))))

  (testing "Edit menu format accelerators"
    (let [frame (ui/create-ide nil)
          menubar (.getJMenuBar frame)
          edit-menu (.getMenu menubar 1)
          item-count (.getItemCount edit-menu)
          items (into {} (keep (fn [i] (when-let [it (.getItem edit-menu i)] [(.getText it) it]))
                               (range item-count)))
          item-format-all (get items "Format All")
          item-format-sel (get items "Format Selection")]
      (is (some? item-format-all))
      (is (= (KeyStroke/getKeyStroke "control shift F") (.getAccelerator item-format-all)))
      (is (some? item-format-sel))
      (is (= (KeyStroke/getKeyStroke "control alt F") (.getAccelerator item-format-sel)))
      (.dispose frame))))

(deftest autocomplete-trigger-test
  (testing "Single match auto-completes immediately without opening popup"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          dirty-called (atom false)
          update-dirty! (fn [] (reset! dirty-called true))]
      (.setText editor "(defrecor")
      (.setCaretPosition editor 9)
      (let [res (ui/trigger-autocomplete! editor status-fn popup-atom nil update-dirty!)]
        (is (= :single-match res))
        (is (= "(defrecord" (.getText editor)))
        (is (= 10 (.getCaretPosition editor)))
        (is (nil? @popup-atom))
        (is @dirty-called)
        (is (= "Completed: defrecord" @status-msg)))))

  (testing "Single match with user-defined function"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          code "(defn calculate-surface-area [r] (* 4 3.14 r r))\n(calc"]
      (.setText editor code)
      (.setCaretPosition editor (.length code))
      (let [res (ui/trigger-autocomplete! editor status-fn popup-atom nil nil)]
        (is (= :single-match res))
        (is (= "(defn calculate-surface-area [r] (* 4 3.14 r r))\n(calculate-surface-area"
               (.getText editor)))
        (is (= "Completed: calculate-surface-area" @status-msg)))))

  (testing "Multiple matches creates popup list with real-time doc preview pane"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))]
      (.setText editor "(pri")
      (.setCaretPosition editor 4)
      (let [res (ui/trigger-autocomplete! editor status-fn popup-atom nil nil)]
        (is (= :multi-match res))
        (is (some? @popup-atom))
        (let [{:keys [list model doc-area]} @popup-atom]
          (is (> (.getSize model) 1))
          (is (= 0 (.getSelectedIndex list)))
          (is (instance? JTextArea doc-area))
          ;; Initial docstring for first candidate (starts with "print" or "printf" or "println")
          (let [first-cand (.getElementAt model 0)
                first-doc (.getText ^JTextArea doc-area)]
            (is (.contains first-doc (:symbol first-cand)))
            (is (.contains first-doc "clojure.core/")))
          ;; Move selection to index 1 and verify doc-area updates
          (ui/move-popup-selection! popup-atom 1)
          (let [second-cand (.getElementAt model 1)
                second-doc (.getText ^JTextArea doc-area)]
            (is (= 1 (.getSelectedIndex list)))
            (is (.contains second-doc (:symbol second-cand)))))
        ;; Commit candidate
        (ui/commit-autocomplete! popup-atom)
        (is (nil? @popup-atom))
        (is (.startsWith (.getText editor) "(pri")))))

  (testing "Doc preview pane shows user-defined function docstring from buffer"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          code (str "(defn calculate-area\n"
                    "  \"Calculates the area of a circle.\"\n"
                    "  [r]\n"
                    "  (* 3.14 r r))\n"
                    "(defn calculate-volume\n"
                    "  \"Calculates the volume of a sphere.\"\n"
                    "  [r]\n"
                    "  (* (/ 4.0 3.0) 3.14 r r r))\n"
                    "(calc")]
      (.setText editor code)
      (.setCaretPosition editor (.length code))
      (let [res (ui/trigger-autocomplete! editor status-fn popup-atom nil nil)]
        (is (= :multi-match res))
        (is (some? @popup-atom))
        (let [{:keys [list model doc-area]} @popup-atom]
          (is (= 2 (.getSize model)))
          (let [doc-text (.getText ^JTextArea doc-area)]
            (is (.contains doc-text "calculate-area"))
            (is (.contains doc-text "Calculates the area of a circle."))
            (is (.contains doc-text "([r])")))
          ;; Move down to calculate-volume
          (ui/move-popup-selection! popup-atom 1)
          (let [doc-text-2 (.getText ^JTextArea doc-area)]
            (is (.contains doc-text-2 "calculate-volume"))
            (is (.contains doc-text-2 "Calculates the volume of a sphere."))
            (is (.contains doc-text-2 "([r])"))))
        (ui/dismiss-autocomplete! popup-atom))))

  (testing "Zero matches reports no completions in status bar"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))]
      (.setText editor "(xyznonexistent123")
      (.setCaretPosition editor 18)
      (let [res (ui/trigger-autocomplete! editor status-fn popup-atom nil nil)]
        (is (= :no-match res))
        (is (nil? @popup-atom))
        (is (= "No completions found for 'xyznonexistent123'" @status-msg)))))

  (testing "Popup navigation and dismissal"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))]
      (.setText editor "(def")
      (.setCaretPosition editor 4)
      (ui/trigger-autocomplete! editor status-fn popup-atom nil nil)
      (is (some? @popup-atom))
      ;; Verify initial doc shows def documentation
      (let [{:keys [doc-area]} @popup-atom]
        (is (.contains (.getText ^JTextArea doc-area) "def  [special form]")))
      ;; Move down
      (ui/move-popup-selection! popup-atom 1)
      (is (= 1 (.getSelectedIndex ^JList (:list @popup-atom))))
      ;; Move up
      (ui/move-popup-selection! popup-atom -1)
      (is (= 0 (.getSelectedIndex ^JList (:list @popup-atom))))
      ;; Dismiss
      (ui/dismiss-autocomplete! popup-atom)
      (is (nil? @popup-atom)))))

(deftest autocomplete-integration-and-menu-test
  (testing "Edit menu has Autocomplete item with Ctrl+Space accelerator"
    (let [frame (ui/create-ide nil)]
      (try
        (let [menubar (.getJMenuBar frame)
              edit-menu (.getMenu menubar 1)
              item-count (.getItemCount edit-menu)
              items (into {} (keep (fn [i] (when-let [it (.getItem edit-menu i)] [(.getText it) it]))
                                   (range item-count)))
              item-auto (get items "Autocomplete")]
          (is (some? item-auto))
          (is (= (KeyStroke/getKeyStroke "control SPACE") (.getAccelerator item-auto))))
        (finally
          (.dispose frame)
          (swap! ui/active-windows dissoc frame))))))

(deftest autocomplete-repl-test
  (testing "Single match auto-completes immediately in JTextField"
    (let [input (JTextField.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))]
      (.setText input "(defrecor")
      (.setCaretPosition input 9)
      (let [res (ui/trigger-autocomplete! input status-fn popup-atom nil nil)]
        (is (= :single-match res))
        (is (= "(defrecord" (.getText input)))
        (is (= 10 (.getCaretPosition input)))
        (is (nil? @popup-atom))
        (is (= "Completed: defrecord" @status-msg)))))

  (testing "Single match in JTextField completing user-defined function from editor buffer via extra-context"
    (let [input (JTextField.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          editor-code "(defn compute-cube-unique-fn [x] (* x x x))"]
      (.setText input "(compute-cube")
      (.setCaretPosition input 13)
      (let [res (ui/trigger-autocomplete! input status-fn popup-atom nil nil editor-code)]
        (is (= :single-match res))
        (is (= "(compute-cube-unique-fn" (.getText input)))
        (is (= "Completed: compute-cube-unique-fn" @status-msg)))))

  (testing "Multiple matches creates popup above prompt with live doc preview"
    (let [input (JTextField.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          editor-code (str "(defn calculate-hypot [a b] (Math/sqrt (+ (* a a) (* b b))))\n"
                           "(defn calculate-area [w h] (* w h))")]
      (.setText input "(calc")
      (.setCaretPosition input 5)
      (let [res (ui/trigger-autocomplete! input status-fn popup-atom nil nil editor-code)]
        (is (= :multi-match res))
        (is (some? @popup-atom))
        (let [{:keys [list model doc-area]} @popup-atom]
          (is (= 2 (.getSize model)))
          (is (instance? JTextArea doc-area))
          (let [first-doc (.getText ^JTextArea doc-area)]
            (is (.contains first-doc "calculate-area")))
          ;; Move selection to calculate-hypot
          (ui/move-popup-selection! popup-atom 1)
          (let [second-doc (.getText ^JTextArea doc-area)]
            (is (.contains second-doc "calculate-hypot")))
          ;; Commit selected candidate
          (ui/commit-autocomplete! popup-atom)
          (is (nil? @popup-atom))
          (is (= "(calculate-hypot" (.getText input)))))))

  (testing "REPL input field bindings and context menu in create-ide"
    (let [frame (ui/create-ide nil)]
      (try
        (let [content-pane (.getContentPane frame)
              all-tfs (atom [])
              collect (fn c [cmp]
                        (when (instance? JTextField cmp)
                          (swap! all-tfs conj cmp))
                        (when (instance? java.awt.Container cmp)
                          (doseq [child (.getComponents cmp)]
                            (c child))))
              _ (collect content-pane)
              input-field (first (filter (fn [tf]
                                           (when-let [p (.getParent tf)]
                                             (some #(and (instance? JLabel %) (= (.getText ^JLabel %) " > "))
                                                   (.getComponents p))))
                                         @all-tfs))]
          (is (some? input-field))
          ;; Check input map has repl-autocomplete for Ctrl+Space and Tab
          (let [im (.getInputMap input-field JComponent/WHEN_FOCUSED)]
            (is (= "repl-autocomplete" (.get im (KeyStroke/getKeyStroke "control SPACE"))))
            (is (= "repl-autocomplete" (.get im (KeyStroke/getKeyStroke "TAB")))))
          ;; Check context menu exists and has Autocomplete item
          (let [menu (.getComponentPopupMenu input-field)]
            (is (some? menu))
            (let [menu-items (into {} (keep (fn [i] (when-let [it (.getComponent menu i)]
                                                      (when (instance? JMenuItem it)
                                                        [(.getText ^JMenuItem it) it])))
                                            (range (.getComponentCount menu))))]
              (is (contains? menu-items "Autocomplete (Ctrl+Space)"))
              (is (contains? menu-items "Clear")))))
        (finally
          (.dispose frame)
          (swap! ui/active-windows dissoc frame))))))

(deftest autocomplete-repl-key-precedence-test
  (testing "Enter commits autocomplete without submitting to REPL evaluator"
    (let [input (JTextField.)
          popup-atom (atom nil)
          committed? (atom false)
          eval-executed? (atom false)
          status-fn (fn [_] nil)]
      (ui/setup-autocomplete-keys! input popup-atom)
      (.addActionListener input
        (proxy [java.awt.event.ActionListener] []
          (actionPerformed [e]
            (when-not (or @committed? (and @popup-atom (.isVisible ^JPopupMenu (:popup @popup-atom))))
              (reset! eval-executed? true)))))
      (.setText input "(pri")
      (.setCaretPosition input 4)
      (ui/trigger-autocomplete! input status-fn popup-atom nil nil nil committed?)
      (is (some? @popup-atom))
      ;; Simulate Enter key press
      (let [enter-event (KeyEvent. input KeyEvent/KEY_PRESSED 0 0 KeyEvent/VK_ENTER \newline)]
        (doseq [kl (.getKeyListeners input)]
          (.keyPressed kl enter-event))
        (is (.isConsumed enter-event))
        (is (nil? @popup-atom))
        (is (.startsWith (.getText input) "(pri"))
        (is (false? @eval-executed?)))))

  (testing "Up/Down arrows navigate autocomplete popup and do not navigate REPL history"
    (let [input (JTextField.)
          popup-atom (atom nil)
          status-fn (fn [_] nil)
          history-changed? (atom false)]
      (ui/setup-autocomplete-keys! input popup-atom)
      (.addKeyListener input
        (proxy [java.awt.event.KeyAdapter] []
          (keyPressed [e]
            (when-not (or (.isConsumed e) @popup-atom)
              (when (or (= (.getKeyCode e) KeyEvent/VK_UP) (= (.getKeyCode e) KeyEvent/VK_DOWN))
                (reset! history-changed? true))))))
      (.setText input "(pri")
      (.setCaretPosition input 4)
      (ui/trigger-autocomplete! input status-fn popup-atom nil nil)
      (is (some? @popup-atom))
      (let [down-event (KeyEvent. input KeyEvent/KEY_PRESSED 0 0 KeyEvent/VK_DOWN KeyEvent/CHAR_UNDEFINED)]
        (doseq [kl (.getKeyListeners input)]
          (.keyPressed kl down-event))
        (is (.isConsumed down-event))
        (is (= 1 (.getSelectedIndex ^JList (:list @popup-atom))))
        (is (false? @history-changed?)))
      (ui/dismiss-autocomplete! popup-atom)))

  (testing "Escape dismisses autocomplete popup cleanly"
    (let [input (JTextField.)
          popup-atom (atom nil)
          status-fn (fn [_] nil)]
      (ui/setup-autocomplete-keys! input popup-atom)
      (.setText input "(pri")
      (.setCaretPosition input 4)
      (ui/trigger-autocomplete! input status-fn popup-atom nil nil)
      (is (some? @popup-atom))
      (let [esc-event (KeyEvent. input KeyEvent/KEY_PRESSED 0 0 KeyEvent/VK_ESCAPE KeyEvent/CHAR_UNDEFINED)]
        (doseq [kl (.getKeyListeners input)]
          (.keyPressed kl esc-event))
        (is (.isConsumed esc-event))
        (is (nil? @popup-atom))
        (is (= "(pri" (.getText input)))))))

(deftest autocomplete-background-nonblocking-test
  (testing "trigger-autocomplete! with {:async? true} returns Future without blocking thread"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))
          code "(pri"]
      (.setText editor code)
      (.setCaretPosition editor 4)
      (let [fut (ui/trigger-autocomplete! editor status-fn popup-atom nil nil nil nil {:async? true})]
        (is (instance? java.util.concurrent.Future fut))
        ;; Await completion
        (.get ^java.util.concurrent.Future fut)
        (SwingUtilities/invokeAndWait (fn [] nil))
        (is (some? @popup-atom))
        (ui/dismiss-autocomplete! popup-atom))))

  (testing "update-autocomplete-filter! dynamically narrows candidates in background"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-fn (fn [_] nil)]
      (.setText editor "(pri")
      (.setCaretPosition editor 4)
      (ui/trigger-autocomplete! editor status-fn popup-atom nil nil)
      (is (some? @popup-atom))
      (let [initial-count (.getSize ^DefaultListModel (:model @popup-atom))]
        (is (> initial-count 1))
        ;; User types "n" -> "(prin"
        (.setText editor "(prin")
        (.setCaretPosition editor 5)
        (ui/update-autocomplete-filter! popup-atom {:sync? true})
        (let [filtered-count (.getSize ^DefaultListModel (:model @popup-atom))]
          (is (<= filtered-count initial-count))
          (is (every? #(str/starts-with? (str/lower-case (:symbol %)) "prin")
                      (for [i (range filtered-count)]
                        (.getElementAt ^DefaultListModel (:model @popup-atom) i)))))
        (ui/dismiss-autocomplete! popup-atom)))

  (testing "trigger-autocomplete! with blank prefix displays popup with fixed cell dimensions"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-msg (atom nil)
          status-fn (fn [msg] (reset! status-msg msg))]
      ;; Cursor is at position 0 (empty editor, blank prefix)
      (.setText editor "")
      (.setCaretPosition editor 0)
      (let [res (ui/trigger-autocomplete! editor status-fn popup-atom nil nil nil nil {:async? false})]
        (is (= :multi-match res))
        (is (some? @popup-atom))
        (let [{:keys [list model]} @popup-atom]
          (is (> (.getSize ^DefaultListModel model) 600))
          (is (= 20 (.getFixedCellHeight ^JList list)))
          (is (= 210 (.getFixedCellWidth ^JList list)))
          (is (some? (.getPrototypeCellValue ^JList list))))
        (ui/dismiss-autocomplete! popup-atom))))

  (testing "update-autocomplete-filter! with blank prefix repopulates full list smoothly"
    (let [editor (JTextPane.)
          popup-atom (atom nil)
          status-fn (fn [_] nil)]
      (.setText editor "a")
      (.setCaretPosition editor 1)
      (ui/trigger-autocomplete! editor status-fn popup-atom nil nil nil nil {:async? false})
      (is (some? @popup-atom))
      ;; User deletes "a", leaving prefix blank
      (.setText editor "")
      (.setCaretPosition editor 0)
      (ui/update-autocomplete-filter! popup-atom {:sync? true})
      (let [{:keys [model]} @popup-atom]
        (is (> (.getSize ^DefaultListModel model) 600)))
      (ui/dismiss-autocomplete! popup-atom)))))
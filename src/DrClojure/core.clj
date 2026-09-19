(ns DrClojure.core
  "Main entry point for DrClojure IDE."
  (:gen-class)
  (:require [DrClojure.ui :as ui])
  (:import (javax.swing SwingUtilities UIManager)))

(def app-title ui/app-name)

(defn start-gui
  "Initializes and shows the DrClojure GUI on the Swing Event Dispatch Thread."
  ([] (start-gui nil))
  ([initial-file]
   (SwingUtilities/invokeLater
     (fn []
       (try
         (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
         (catch Throwable _ nil))
       (let [frame (ui/create-ide initial-file)]
         (.setVisible frame true))))))

(defn -main
  "Application entry point. Accepts an optional file path to open."
  [& args]
  (start-gui (first args)))

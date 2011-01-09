(ns notes
    (:import 
      (jcurses.widgets.Window)
      (jcurses.widgets.Panel)
      (jcurses.widgets.TextArea)))

(def _command_stack {})
(def _x 0)
(def _y 0)

(def COMMAND_MODE 0)
(def INSERT_MODE 2)

(defn exit [window]
      (.close window))

(defn read_key []
      ;needs a lot of work here..
      (println (read-line))
)

(defn process_command [window x y input_key]
      ;command mode: to be filled out
      )

(defn process_insert [window x y input_key]
      ;insert mode: to be filled out
      )

(defn process [window x y input_key mode]
      (if input_key
        (cond
          (= mode COMMAND_MODE)
          (process_command window x y input_key)
          (= mode INSERT_MODE)
          (process_insert window x y input_key))
        ;
        (process window x y (read_key) mode))
      (.repaint window))
        

(defn main []
      (let
        ;window should be a custom class i believe
        [window (new jcurses.widgets.Window 80 40 true "Clojure Notes")]
        (.setShadow window false)
        ;no addWidget method?
        ;(.addWidget (.getRootPanel window) (new jcurses.widgets.TextArea 80 40 "hello") nil)
        (.show window)
        (process window 0 0 nil COMMAND_MODE)))

(main)

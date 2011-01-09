(ns notes
    (:import 
      (jcurses.system.Toolkit)
      (jcurses.system.InputChar)
      (jcurses.system.CharColor)))

(declare _command_stack)
(declare _buffer)
(declare _x _y)

(def _command_stack {})
(def _buffer {})
(def _x 0)
(def _y 0)

(def COMMAND_MODE 0)
(def INSERT_MODE 2)


;should be read from a config file
(def __text_color (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

(declare construct_toolbar)
(declare update)
(declare read_key)
(declare process_command)
(declare process_insert)
(declare process)

(defn construct_toolbar [mode]
      (let
        [p (str "Position: " 
                (java.lang.Integer/toString _y) "," (java.lang.Integer/toString _x)) 
           m (str "Mode: " 
                  (cond (= mode COMMAND_MODE) "Command" (= mode INSERT_MODE) "Insert"))]
        (str p " " m)))

(defn update [mode]
      (let
        [height (- (jcurses.system.Toolkit/getScreenHeight) 1)]
        (jcurses.system.Toolkit/printString (construct_toolbar mode) 0 height __text_color)))

(defn read_key []
      (jcurses.system.Toolkit/readCharacter))

(defn process_command [x y input_key]
      (do
        (jcurses.system.Toolkit/clearScreen __text_color)

        ;display key code
        (jcurses.system.Toolkit/printString (java.lang.Integer/toString (.getCode input_key)) x y __text_color)))

(defn process_insert [x y input_key])

(defn process [x y input_key mode]
      (do
        (if input_key
          (cond
            (= mode COMMAND_MODE)
            (process_command x y input_key)
            (= mode INSERT_MODE)
            (process_insert x y input_key)))
        (update mode)
        (process x y (read_key) mode)))

(defn main []
      (do
        ;required
        (jcurses.system.Toolkit/init)
        (process 0 0 nil COMMAND_MODE)))

(main)

(ns notes
    (:import 
      (jcurses.system.Toolkit)
      (jcurses.system.InputChar)
      (jcurses.system.CharColor)))

(declare _command_stack)
(declare _buffer)

(def _command_stack {})
(def _buffer {})

(def COMMAND_MODE 0)
(def INSERT_MODE 2)


;should be read from a config file
(def __text_color (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

;Toolkit calls
(defn init []
      (jcurses.system.Toolkit/init))

(defn getScreenHeight []
      (jcurses.system.Toolkit/getScreenHeight))

(defn clearScreen [text_color]
      (jcurses.system.Toolkit/clearScreen text_color))

(defn printString [str y x text_color]
      (jcurses.system.Toolkit/printString str y x text_color))

(defn readCharacter []
      (jcurses.system.Toolkit/readCharacter))

(defn move [x y]
      (jcurses.system.Toolkit/move x y))

;declared in order of drawing
(declare construct_toolbar)
(declare move_cursor)
(declare update)
(declare process_command)
(declare process_insert)
(declare process)

;still needs work here
(defn construct-toolbar [x y mode input_key]
      (let
        [p (str "Position: " 
                (java.lang.Integer/toString y) "," (java.lang.Integer/toString x)) 
           m (str "Mode: " 
                  (cond (= mode COMMAND_MODE) "Command" (= mode INSERT_MODE) "Insert"))
        k (str "Key Code: " (if (not (= input_key nil)) (java.lang.Integer/toString (.getCode input_key)) "" ))]
        (str p " - " m " - " k)))

(defn update [x y mode input_key]
      (let
        [height (- (getScreenHeight) 1)]
        (clearScreen __text_color)
        (printString (construct-toolbar x y mode input_key) 0 height __text_color)
        (move x y)))


(defn process-command [x y input_key]
      )

(defn process-insert [x y input_key])

(defn process [x y input_key mode]
      (loop [r_x x r_y y r_input_key input_key r_mode mode]
        (if r_input_key
          (cond
            (= r_mode COMMAND_MODE)
            (process-command r_x r_y r_input_key)
            (= r_mode INSERT_MODE)
            (process-insert r_x r_y r_input_key)))
        (update r_x r_y r_mode r_input_key)
        (recur r_x r_y (readCharacter) r_mode)))

(defn main []
      (do
        ;required
        (init)
        (process 0 0 nil COMMAND_MODE)))

(main)

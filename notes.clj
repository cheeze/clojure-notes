(ns notes
    (:import 
      (jcurses.system.Toolkit)
      (jcurses.system.InputChar)
      (jcurses.system.CharColor)))

(declare _command_stack)
(declare _buffer)

;global state..
(def _command_stack {})
(def _buffer {})

;constants
(def COMMAND_MODE 0)
(def INSERT_MODE 2)

;key codes
(def KEY_ESC 27)
(def KEY_I 105)

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

(defn- _position-helper [x y]
       (str (java.lang.Integer/toString y) "," (java.lang.Integer/toString x)))

(defn- _mode-helper [mode]
       (cond (= mode COMMAND_MODE) "Command" (= mode INSERT_MODE) "Insert"))

(defn- _key-code-helper [input_key]
       (if (not (= input_key nil)) (java.lang.Integer/toString (.getCode input_key)) "nil" ))

(defn- _command-stack-helper []
       "")

(defn construct-toolbar [x y mode input_key]
      (let
        [p (str "Position: " (_position-helper x y))
           m (str "Mode: " (_mode-helper mode))
           k (str "Key Code: " (_key-code-helper input_key))
           c (str "Command Stack: " (_command-stack-helper))]
        (str p " - " m " - " k " - " c)))

(defn update [x y mode input_key]
      (do
        (clearScreen __text_color)
        ;loop through the buffer and print (use x and y to determine position)
        (printString (construct-toolbar x y mode input_key) 0 (- (getScreenHeight) 1) __text_color)
        (move x y)))


(defn process-command [x y input_key])

(defn process-insert [x y input_key])

(defn process [x y input_key mode]
      (loop [r_x x r_y y r_input_key input_key r_mode mode]
            (if r_input_key
              (cond
                ;check for special conditions here
                (= (.getCode r_input_key) KEY_ESC)
                (do
                  (update r_x r_y COMMAND_MODE r_input_key)
                  (recur r_x r_y (readCharacter) COMMAND_MODE))

                :else
                (cond
                  (= r_mode COMMAND_MODE)
                  (cond
                    (= (.getCode r_input_key) KEY_I)
                    (do
                      (update r_x r_y INSERT_MODE r_input_key)
                      (recur r_x r_y (readCharacter) INSERT_MODE))

                    :else
                    (do
                      (process-command r_x r_y r_input_key)
                      (update r_x r_y r_mode r_input_key)
                      (recur r_x r_y (readCharacter) COMMAND_MODE)))

                  (= r_mode INSERT_MODE)
                  (do
                    (process-insert r_x r_y r_input_key)
                    (update r_x r_y r_mode r_input_key)
                    (recur r_x r_y (readCharacter) INSERT_MODE))))
              (do
                (update r_x r_y r_mode r_input_key)
                (recur r_x r_y (readCharacter) mode)))))

(defn main []
      (do
        ;required
        (init)
        (process 0 0 nil COMMAND_MODE)))

(main)

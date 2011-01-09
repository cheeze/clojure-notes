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
(def KEY_RETURN 10)
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

(defn printString [str x y text_color]
      (jcurses.system.Toolkit/printString str y x text_color))

(defn readCharacter []
      (jcurses.system.Toolkit/readCharacter))

(defn move [x y]
      (jcurses.system.Toolkit/move x y))

;declared in order of drawing
(declare construct_toolbar)
(declare update)
(declare process_command)
(declare process_insert)
(declare process)

;display helper
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

(defn update [pos mode input_key]
      (do
        (clearScreen __text_color)
        ;loop through the buffer and print (use x and y to determine position)
        (printString (construct-toolbar (first pos) (second pos) mode input_key) (- (getScreenHeight) 1) 0 __text_color)
        (move (first pos) (second pos))))

;put into stack
;check for commands (execute if any)
;returns new x and y coordinates
(defn process-command [pos input_key] pos)

;put into buffer
;returns new x and y coordinates
(defn process-insert [pos input_key]
      (do
        (printString (.toString input_key) (first pos) (second pos) __text_color)
        [(first pos) (+ 1 (second pos))]))

(defn process [pos input_key mode]
      (loop [r_pos pos r_input_key input_key r_mode mode]
            (if r_input_key
              (cond
                ;GLOBAL special codes
                (= (.getCode r_input_key) KEY_ESC)
                (do
                  (update r_pos COMMAND_MODE r_input_key)
                  (recur r_pos (readCharacter) COMMAND_MODE))

                :else
                (cond
                  (= r_mode COMMAND_MODE)
                  (cond
                    ;COMMAND_MODE special codes
                    ;
                    ;switch to INSERT_MODE
                    (= (.getCode r_input_key) KEY_I)
                    (do
                      (update r_pos INSERT_MODE r_input_key)
                      (recur r_pos (readCharacter) INSERT_MODE))

                    :else
                    (let [n_pos (process-command r_pos r_input_key)]
                      (update n_pos r_mode r_input_key)
                      (recur n_pos (readCharacter) r_mode)))

                  (= r_mode INSERT_MODE)
                  (cond
                    ;INSERT_MODE special codes
                    ;
                    ;press enter
                    (= (.getCode r_input_key) KEY_RETURN)
                    nil

                    :else
                    (let [n_pos (process-insert r_pos r_input_key)]
                      (update n_pos r_mode r_input_key)
                      (recur n_pos (readCharacter) r_mode)))))
              (do
                (update r_pos r_mode r_input_key)
                (recur r_pos (readCharacter) r_mode)))))

(defn main []
      (do
        (init)
        (process [0 0] nil COMMAND_MODE)))

(main)

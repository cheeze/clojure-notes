(ns notes
    (:import 
      (jcurses.system.Toolkit)
      (jcurses.system.InputChar)
      (jcurses.system.CharColor)))

;constants
(def COMMAND_MODE 0)
(def LAST_LINE_MODE 1)
(def INSERT_MODE 2)
(def VISUAL_MODE 3)

;key codes
(def KEY_RETURN 10)
(def KEY_ESC 27)
(def KEY_COLON 58)
(def KEY_I 105)

;should be read from a config file
(def __text_color (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

;Toolkit calls
(defn init []
      (jcurses.system.Toolkit/init))

(defn getScreenWidth []
      (jcurses.system.Toolkit/getScreenWidth))

(defn getScreenHeight []
      (jcurses.system.Toolkit/getScreenHeight))

(defn clearScreen [text_color]
      (jcurses.system.Toolkit/clearScreen text_color))

(defn printString [str x y text_color]
      (jcurses.system.Toolkit/printString str x y text_color))

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
       (cond 
         (= mode COMMAND_MODE)
         "Command"

         (= mode LAST_LINE_MODE)
         "Last Line"

         (= mode INSERT_MODE)
         "Insert"

         (= mode VISUAL_MODE)
         "Visual"))

(defn- _key-code-helper [input_key]
       (if (not (= input_key nil)) (java.lang.Integer/toString (.getCode input_key)) "nil" ))

(defn- _command-stack-helper [command_stack]
       (defn _compress [c command_stack]
             (if (= command_stack [])
               c
               (recur (str c (first command_stack)) (rest command_stack))))
       (_compress "" command_stack))

(defn construct-toolbar [x y mode command_stack]
      (let
        [p (str "Position: " (_position-helper x y))
           m (str "Mode: " (_mode-helper mode))
           c (str "Command Stack: " (_command-stack-helper command_stack))]
        (str p " - " m " - " c)))

(defn- _get_x [pos]
       (first (first pos)))

(defn- _get_y [pos]
       (second (first pos)))

(defn update [pos mode input_key command_stack]
      (do
        (clearScreen __text_color)
        (let [toolbar (construct-toolbar (_get_x pos) (_get_y pos) mode command_stack)
           key_code (str (_key-code-helper input_key))]
          (printString toolbar 0 (- (getScreenHeight) 1) __text_color)
          (printString key_code (- (getScreenWidth) 3) (- (getScreenHeight) 1) __text_color))
        (move (_get_x pos) (_get_y pos))))

;put into stack
;check for commands (execute if any)
;returns new x and y coordinates
(defn process-command [pos input_key] pos)

;returns sequence with head being new x and y coordinates
(defn- process-initial-last-line-position [pos mode command_stack]
       (let [x (+ 4 (.length (construct-toolbar (first (last pos)) (second (last pos)) mode command_stack)))
               y (- (getScreenHeight) 1)]
         (cons [x y] pos)))

(defn- process-last-line-position [pos]
       [[(+ 1 (_get_x pos)) (_get_y pos)] (last pos)])

;put into buffer
;returns new x and y coordinates
(defn process-insert [pos input_key]
      (do
        (printString (.toString input_key) (_get_x pos) (_get_y pos) __text_color)
        [[(+ 1 (_get_x pos)) (_get_y pos)]]))

(defn process [pos mode input_key command_stack]
      (loop [r_pos pos r_mode mode r_input_key input_key r_command_stack command_stack]
            (if r_input_key
              (cond
                ;GLOBAL special codes
                (= (.getCode r_input_key) KEY_ESC)
                (do
                  (let [n_pos [(last r_pos)]]
                    (update n_pos COMMAND_MODE r_input_key [])
                    (recur n_pos COMMAND_MODE (readCharacter) [])))

                :else
                (cond
                  (= r_mode COMMAND_MODE)
                  (cond
                    ;COMMAND_MODE special codes
                    ;
                    ;switch to INSERT_MODE
                    (= (.getCode r_input_key) KEY_I)
                    (do
                      (update r_pos INSERT_MODE r_input_key r_command_stack)
                      (recur r_pos INSERT_MODE (readCharacter) r_command_stack))

                    ;switch to LAST_LINE_MODE
                    (= (.getCode r_input_key) KEY_COLON)
                    (let [n_command_stack (conj [] (.toString r_input_key))]
                      (let [n_pos (process-initial-last-line-position r_pos r_mode n_command_stack)]
                        (update n_pos LAST_LINE_MODE r_input_key n_command_stack)
                        (recur n_pos LAST_LINE_MODE (readCharacter) n_command_stack)))

                    :else
                    (let [n_pos (process-command r_pos r_input_key)]
                      (update n_pos r_mode r_input_key r_command_stack)
                      (recur n_pos r_mode (readCharacter) r_command_stack)))

                  (= r_mode LAST_LINE_MODE)
                  (cond
                    ;LAST_LINE_MODE special codes
                    ;
                    ;reset LAST_LINE_MODE
                    (= (.getCode r_input_key) KEY_COLON)
                    (let [n_command_stack (conj [] (.toString r_input_key))]
                      (let [n_pos (process-initial-last-line-position r_pos r_mode n_command_stack)]
                        (update n_pos LAST_LINE_MODE r_input_key n_command_stack)
                        (recur n_pos LAST_LINE_MODE (readCharacter) n_command_stack)))

                    ;need backspace too
                    ;and return

                    :else
                    (let [n_command_stack (conj r_command_stack (.toString r_input_key))]
                      (let [n_pos (process-last-line-position r_pos)]
                        (update n_pos LAST_LINE_MODE r_input_key n_command_stack)
                        (recur n_pos LAST_LINE_MODE (readCharacter) n_command_stack))))

                  (= r_mode INSERT_MODE)
                  (cond
                    ;INSERT_MODE special codes
                    ;
                    ;press enter
                    (= (.getCode r_input_key) KEY_RETURN)
                    nil

                    :else
                    (let [n_pos (process-insert r_pos r_input_key)]
                      (update n_pos r_mode r_input_key r_command_stack)
                      (recur n_pos r_mode (readCharacter) r_command_stack)))

                  (= r_mode VISUAL_MODE)
                  (cond
                    ;VISUAL_MODE special codes

                    :else
                    (let []
                      (update r_pos r_mode r_input_key r_command_stack)
                      (recur r_pos r_mode (readCharacter) r_command_stack)))))
              (do
                (update r_pos r_mode r_input_key r_command_stack)
                (recur r_pos r_mode (readCharacter) r_command_stack))))
      ;properly exit here if something goes wrong
      nil)

(defn main []
      (do
        (init)
        (process [[0 0]] COMMAND_MODE nil [])))

(main)

(ns notes
  (:import 
     (jcurses.system.Toolkit)
     (jcurses.system.InputChar)
     (jcurses.system.CharColor)))

;constants
(def COMMAND_STACK "command stack: ")
(def CURRENT_POSITION "curr pos: ")
(def CURRENT_MODE "curr mode: ")

(def NORMAL "normal")
(def COMMAND "command")
(def INSERT "insert")
(def VISUAL "visual")

(def NORMAL_MODE 0)
(def COMMAND_MODE 1)
(def INSERT_MODE 2)
(def VISUAL_MODE 3)

;key codes
(def KEY_RETURN 10)
(def KEY_ESC 27)
(def KEY_COLON 58)
(def KEY_I 105)
(def KEY_V 118)
(def KEY_BACKSPACE 263)

;misc keys
(def ANCHOR 0)

;should be read from a config file
(def __text_color (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

(def __error_color (new jcurses.system.CharColor (jcurses.system.CharColor/WHITE) (jcurses.system.CharColor/RED)))

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
  ;(println x y str))
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
(defn- _get_x [pos]
  (first (first pos)))

(defn- _get_y [pos]
  (second (first pos)))

(defn- _position-helper [x y]
  (str (java.lang.Integer/toString y) "," (java.lang.Integer/toString x)))

(defn- _mode-helper [mode]
  (cond 
    (= mode NORMAL_MODE)
    NORMAL

    (= mode COMMAND_MODE)
    COMMAND

    (= mode INSERT_MODE)
    INSERT

    (= mode VISUAL_MODE)
    VISUAL))

(defn- _key-code-helper [input_key]
  (if (not (= input_key nil)) (java.lang.Integer/toString (.getCode input_key)) "nil" ))

(defn- _command-stack-helper [command_stack]
  (defn _compress [c command_stack]
    (if (= command_stack [])
      c
      (recur (str c (first command_stack)) (rest command_stack))))
  (_compress "" command_stack))

(defn- construct-toolbar [x y mode command_stack]
  (let
    [p (str CURRENT_POSITION (_position-helper x y))
     m (str CURRENT_MODE (_mode-helper mode))
     c (str COMMAND_STACK (_command-stack-helper command_stack))]
    (cond
      (= mode NORMAL_MODE)
      (str p " - " m " - " c)

      (= mode COMMAND_MODE)
      c

      (= mode INSERT_MODE)
      (str p " - " m)

      (= mode VISUAL_MODE)
      (str p " - " m " - " c))))

(defn- show-text [text row]
  (do
    (if text
      (printString text 0 row __text_color))))

(defn- show-buffer [buffer anchor]
  (let [height (- (getScreenHeight) 1)]
    (let [bounds (range anchor (+ anchor height))
          page-bounds (range 0 height)]
      (dorun (map show-text (map buffer bounds) page-bounds)))))

(defn- update [buffer pos mode input_key command_stack misc]
  (do
    (clearScreen __text_color)
    (let [toolbar (construct-toolbar (_get_x pos) (_get_y pos) mode command_stack)
          key_code (str (_key-code-helper input_key))]
      (show-buffer buffer (misc ANCHOR))
      (printString toolbar 0 (- (getScreenHeight) 1) __text_color)
      (printString key_code (- (getScreenWidth) 3) (- (getScreenHeight) 1) __text_color))
    (move (_get_x pos) (_get_y pos))))

;GENERIC
;push onto stack (or remove if backspace)
(defn- generic-push-command-stack [input_key command_stack]
  (if (not (.isSpecialCode input_key))
    (cond 
      (= (.getCode input_key) KEY_RETURN)
      command_stack

      :else
      (concat command_stack (.toString input_key)))
    (cond
      (= (.getCode input_key) KEY_BACKSPACE)
      (drop-last command_stack))))

;NORMAL
;check for commands (execute if any)
;returns new x and y coordinates
(defn- normal-evaluate-command-stack [pos command_stack] pos)

;returns sequence with head being new x and y coordinates
(defn- normal-process-initial-command-position [pos mode command_stack]
  (let [x (+ (.length COMMAND_STACK) 1)
        y (- (getScreenHeight) 1)]
    (cons [x y] pos)))

;COMMAND
;evaluate whatever is on the stack (print error if invalid)
(defn- command-evaluate-command-stack [pos command_stack] 
  (let [c (str "invalid command: " (_command-stack-helper command_stack))]
    (printString c 0 (- (getScreenHeight) 1) __error_color)
    (readCharacter)
    pos))

;drop the last element in the stack
(defn- command-backspace-command-stack [command_stack]
  (drop-last command_stack))

;go back one position
(defn- command-backspace-update-position [pos]
  (cons [(- (_get_x pos) 1) (_get_y pos)] (rest pos)))

;update to toolbar position
(defn- command-update-position [pos input_key]
  (cons [(+ (_get_x pos) 1) (_get_y pos)] (rest pos)))

;INSERT
;update the position
;returns new x and y coordinates
(defn- insert-update-position [pos input_key]
  (cond
    (and (= (.getCode input_key) KEY_BACKSPACE) (= (_get_x pos) 0))
    (if
      (= (_get_y pos) 0)
      pos
      [[(_get_x pos) (- (_get_y pos) 1)]]) ;TODO: the x position needs to be at the edge

    (= (.getCode input_key) KEY_BACKSPACE)
    [[(- (_get_x pos) 1) (_get_y pos)]]

    (= (.getCode input_key) KEY_RETURN)
    [[0 (+ (_get_y pos) 1)]]

    :else
    [[(+ 1 (_get_x pos)) (_get_y pos)]]))

(defn- insert-update-buffer [buffer pos input_key] 
  (cond
    (= (.getCode input_key) KEY_BACKSPACE)
    (if
      (= (_get_x pos) 0)
      buffer
      buffer)

    (= (.getCode input_key) KEY_RETURN)
    buffer

    :else
    (let [s (buffer (_get_y pos))]
      (if s
        (let [pre (.substring s 0 (_get_x pos))
              post (.substring s (_get_x pos) (.length s))]
          ;need to check input_key for type of character
          (assoc buffer (_get_y pos) (.concat pre (.concat (.toString input_key) post))))
        (assoc buffer (_get_y pos) (.toString input_key))))))

;main loop
(defn- process [buffer pos mode input_key command_stack misc]
  (loop [r_buffer buffer r_pos pos r_mode mode r_input_key input_key r_command_stack command_stack r_misc misc]
    (if r_input_key
      (cond
        ;GLOBAL special codes
        (= (.getCode r_input_key) KEY_ESC)
        (do
          (let [n_pos [(last r_pos)]]
            (update r_buffer n_pos NORMAL_MODE r_input_key [] r_misc)
            (recur r_buffer n_pos NORMAL_MODE (readCharacter) [] r_misc)))

        :else
        (cond
          (= r_mode NORMAL_MODE)
          (cond
            ;NORMAL_MODE special codes
            ;
            ;switch to COMMAND_MODE
            (= (.getCode r_input_key) KEY_COLON)
            (let [n_command_stack (conj [] (.toString r_input_key))]
              (let [n_pos (normal-process-initial-command-position r_pos r_mode n_command_stack)]
                (update r_buffer n_pos COMMAND_MODE r_input_key n_command_stack r_misc)
                (recur r_buffer n_pos COMMAND_MODE (readCharacter) n_command_stack r_misc)))

            ;switch to INSERT_MODE
            (= (.getCode r_input_key) KEY_I)
            (do
              (update r_buffer r_pos INSERT_MODE r_input_key [] r_misc)
              (recur r_buffer r_pos INSERT_MODE (readCharacter) [] r_misc))

            ;switch to VISUAL_MODE
            (= (.getCode r_input_key) KEY_V)
            (do
              (update r_buffer r_pos VISUAL_MODE r_input_key [] r_misc)
              (recur r_buffer r_pos VISUAL_MODE (readCharacter) [] r_misc))

            ;push input_key onto stack and evaluate
            :else
            (let [n_command_stack (generic-push-command-stack r_input_key r_command_stack)]
              (let [n_pos (normal-evaluate-command-stack r_pos n_command_stack)]
                (update r_buffer n_pos r_mode r_input_key n_command_stack r_misc)
                (recur r_buffer n_pos r_mode (readCharacter) n_command_stack r_misc))))

          (= r_mode COMMAND_MODE)
          (cond
            ;COMMAND_MODE special codes
            ;
            (= (.getCode r_input_key) KEY_RETURN)
            (let [n_pos (command-evaluate-command-stack r_pos r_command_stack)]
              (update r_buffer n_pos COMMAND_MODE r_input_key r_command_stack r_misc)
              (recur r_buffer n_pos COMMAND_MODE (readCharacter) r_command_stack r_misc))

            (= (.getCode r_input_key) KEY_BACKSPACE)
            (let [n_command_stack (command-backspace-command-stack r_command_stack)]
              (cond
                (> (.length (vec n_command_stack)) 0)
                (let [n_pos (command-backspace-update-position r_pos)]
                  (update r_buffer n_pos COMMAND_MODE r_input_key n_command_stack r_misc)
                  (recur r_buffer n_pos COMMAND_MODE (readCharacter) n_command_stack r_misc))

                :else
                (let [n_pos [(last r_pos)]]
                  (update r_buffer n_pos NORMAL_MODE r_input_key [] r_misc)
                  (recur r_buffer n_pos NORMAL_MODE (readCharacter) n_command_stack r_misc))))

            :else
            (let [n_command_stack (generic-push-command-stack r_input_key r_command_stack)]
              (let [n_pos (command-update-position r_pos r_input_key)]
                (update r_buffer n_pos COMMAND_MODE r_input_key n_command_stack r_misc)
                (recur r_buffer n_pos COMMAND_MODE (readCharacter) n_command_stack r_misc))))

          (= r_mode INSERT_MODE)
          (cond
            ;INSERT_MODE special codes
            ;

            :else
            (let [n_pos (insert-update-position r_pos r_input_key)
                  n_buffer (insert-update-buffer r_buffer r_pos r_input_key)]
              (update n_buffer n_pos r_mode r_input_key r_command_stack r_misc)
              (recur n_buffer n_pos r_mode (readCharacter) r_command_stack r_misc)))

          (= r_mode VISUAL_MODE)
          (cond
            ;VISUAL_MODE special codes

            :else
            (let []
              (update r_buffer r_pos r_mode r_input_key r_command_stack r_misc)
              (recur r_buffer r_pos r_mode (readCharacter) r_command_stack r_misc)))))
      (do
        (update r_buffer r_pos r_mode r_input_key r_command_stack r_misc)
        (recur r_buffer r_pos r_mode (readCharacter) r_command_stack r_misc)))))

(defn main []
  (do
    ;use *command-line-args* to access .. command line args
    ;
    ;about process args:
    ;  buffer - associative array: line-number -> string
    ;           numbers less than 0 are reserved (such as clipboard)
    ;
    ;  position - stack: only changes when going into command mode
    ;             should only contain at most 2 elements
    ;
    ;  mode - enum: defines the mode on the next iteration
    ;               which occurs after a keystroke.
    ;
    ;  input_key - inputchar: java class. see InputChar.java
    ;
    ;  command_stack - stack: keeps track of what the user currently
    ;                  has typed in normal and command mode
    ;                  note it resets on changing modes.
    ;  
    ;  misc - associative array: * -> *
    ;(init)
    (process {0 "test" 1 "what do you want" 2 "" 3 "hello" 45 "45" 46 "46" 47 "47" 48 "48" 49 "49" 50 "50"} [[0 0]] NORMAL_MODE nil [] {ANCHOR 0})))

(main)

(ns cvim
  (:import
     (jcurses.system.Toolkit)
     (jcurses.system.InputChar)
     (jcurses.system.CharColor)))

;constants
(def COMMAND_STACK_STR "command stack: ")
(def CURRENT_POSITION_STR "curr pos: ")
(def CURRENT_MODE_STR "curr mode: ")

(def NORMAL "normal")
(def COMMAND "command")
(def INSERT "insert")
(def VISUAL "visual")

(def NORMAL_MODE 0)
(def COMMAND_MODE 1)
(def INSERT_MODE 2)
(def VISUAL_MODE 3)

;input key codes
(def KEY_RETURN 10)
(def KEY_ESC 27)
(def KEY_COLON 58)
(def KEY_H 104)
(def KEY_I 105)
(def KEY_J 106)
(def KEY_K 107)
(def KEY_L 108)
(def KEY_V 118)
(def KEY_BACKSPACE 263)

;state keys
(def BUFFER 0)
(def POSITION 1)
(def MODE 2)
(def INPUT_KEY 3)
(def COMMAND_STACK 4)
(def ANCHOR 5)
(def ERROR_DISPLAY 20)

;configuration constants
;special keys
(def TO_NORMAL_MODE_KEY (new jcurses.system.InputChar KEY_ESC))
(def NORMAL_TO_COMMAND_MODE_KEY (new jcurses.system.InputChar KEY_COLON))
(def NORMAL_TO_INSERT_MODE_KEY (new jcurses.system.InputChar KEY_I))
(def NORMAL_TO_VISUAL_MODE_KEY (new jcurses.system.InputChar KEY_V))

(def TEXT_COLOR (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

(def ERROR_COLOR (new jcurses.system.CharColor (jcurses.system.CharColor/WHITE) (jcurses.system.CharColor/RED)))

;Toolkit calls
(defn init []
  (jcurses.system.Toolkit/init))

(defn get-screen-width []
  (jcurses.system.Toolkit/getScreenWidth))

(defn get-screen-height []
  (jcurses.system.Toolkit/getScreenHeight))

(defn clear-screen []
  (jcurses.system.Toolkit/clearScreen TEXT_COLOR))

(defn print-string [string x y]
  (jcurses.system.Toolkit/printString string x y TEXT_COLOR))

(defn print-error [string x y]
  (jcurses.system.Toolkit/printString string x y ERROR_COLOR))

(defn read-character []
  (jcurses.system.Toolkit/readCharacter))

(defn move [x y]
  (jcurses.system.Toolkit/move x y))

;display helpers
(declare move-cursor)
(declare get-x)
(declare get-y)
(declare position-to-string)
(declare mode-to-string)
(declare input-key-to-string)
(declare command-stack-to-string)

(defn- move-cursor [state]
  (let [position (state POSITION)]
    (move (get-x position) (get-y position))))

(defn- get-x [pos]
  (first (first pos)))

(defn- get-y [pos]
  (second (first pos)))

(defn- position-to-string [pos]
  (let [x (first (last pos))
        y (second (last pos))]
    (str (java.lang.Integer/toString y) "," (java.lang.Integer/toString x))))

(defn- mode-to-string [mode]
  (cond
    (= mode NORMAL_MODE)
    NORMAL

    (= mode COMMAND_MODE)
    COMMAND

    (= mode INSERT_MODE)
    INSERT

    (= mode VISUAL_MODE)
    VISUAL))

(defn- input-key-to-string [input_key]
  (if (not (= input_key nil)) (java.lang.Integer/toString (.getCode input_key)) "nil"))

(defn- command-stack-to-string [command_stack]
  (defn _compress [c command_stack]
    (if (= command_stack [])
      c
      (recur (str c (first command_stack)) (rest command_stack))))
  (_compress "" command_stack))

;CONSTRUCTION
(declare construct-status-bar)
(declare construct-information-bar)

;construct main status bar
(defn- construct-status-bar [state]
  (let [mode (state MODE)
        command_stack (state COMMAND_STACK)]
    (let [mode_str (str CURRENT_MODE_STR (mode-to-string mode))
          command_stack_str (str COMMAND_STACK_STR (command-stack-to-string command_stack))]
      (cond
        (= mode NORMAL_MODE)
        (str mode_str " - " command_stack_str)

        (= mode COMMAND_MODE)
        command_stack_str

        (= mode INSERT_MODE)
        mode_str

        (= mode VISUAL_MODE)
        (str mode_str " - " command_stack_str)))))

;construct information bar
(defn- construct-information-bar [state]
  (let [position (state POSITION)
        input_key (state INPUT_KEY)]
    (let [position_str (position-to-string position)
          input_key_str (input-key-to-string input_key)]
      (str CURRENT_POSITION_STR ": " position_str " - " input_key_str))))

;DISPLAY
(declare show-text)
(declare show-buffer)
(declare show-bar)
(declare update)

;display buffer helper
(defn- show-text [text row]
  (if text
    (print-string text 0 row)
    (print-string "~" 0 row)))

(defn- show-buffer [state]
  (let [buffer (state BUFFER)
        anchor (state ANCHOR)
        height (- (get-screen-height) 1)]
    (let [bounds (range anchor (+ anchor height))
          page-bounds (range 0 height)]
      (dorun (map show-text (map buffer bounds) page-bounds)))))

(defn- show-bar [state]
  (let [status_bar (construct-status-bar state)
        information_bar (construct-information-bar state)
        screen_height (get-screen-height)
        screen_width (get-screen-width)]
        (print-string information_bar (- screen_width 25) (- screen_height 1))
    (if (state ERROR_DISPLAY)
      (print-error (state ERROR_DISPLAY) 0 (- screen_height 1))
      (print-string status_bar 0 (- screen_height 1)))))


(defn- update [state]
  (do ;TODO: might need to limit how fast this can redraw..
    (clear-screen)
    (show-buffer state)
    (show-bar state)
    (move-cursor state)
    (assoc state INPUT_KEY (read-character) ERROR_DISPLAY nil)))

;MODE
;helpers
(declare push-command-stack)
(declare pop-command-stack)

(defn- push-command-stack [command_stack input_key]
  (if (not (.isSpecialCode input_key))
    (cond
      (= (.getCode input_key) KEY_RETURN)
      command_stack

      :else
      (concat command_stack (.toString input_key)))
    (cond
      (= (.getCode input_key) KEY_BACKSPACE)
      command_stack)))

(defn pop-command-stack [command_stack]
  (drop-last command_stack))

(declare move-cursor-up)
(declare move-cursor-down)
(declare move-cursor-left)
(declare move-cursor-right)

(defn move-cursor-up [position n]
  (let [new_y (- (get-y position) n)]
    (if (< new_y 0)
      position
      (cons [(get-x position) (- (get-y position) n)] (rest position)))))

(defn move-cursor-down [position n]
  (move-cursor-up position (* n -1)))

(defn move-cursor-left [position n]
  (let [new_x (- (get-x position) n)]
    (if (< new_x 0)
      position
      (cons [(- (get-x position) n) (get-y position)] (rest position)))))

(defn move-cursor-right [position n]
  (move-cursor-left position (* n -1)))



;generic
(declare generic-input-key-esc)
(defn- generic-input-key-esc [state]
  (let [position (state POSITION)]
    (let [new_position [(last position)]
          new_mode NORMAL_MODE
          new_command_stack []]
      (assoc state MODE new_mode POSITION new_position COMMAND_STACK new_command_stack))))

;normal mode
(declare normal-evaluate-command-stack)
(declare normal-push-command-stack)
(declare normal-to-command)
(declare normal-to-insert)
(declare normal-to-visual)

;evaluate the command stack in normal mode
(defn- normal-evaluate-command-stack [state]
  (let [command_stack (state COMMAND_STACK)]
    (cond
      ;what is command_stack

      :else
      state)))

(defn- normal-push-command-stack [state]
  (let [position (state POSITION)
        input_key (state INPUT_KEY)
        command_stack (state COMMAND_STACK)]
    (if (.isSpecialCode input_key)
      (cond
        (= (.getCode input_key) KEY_BACKSPACE)
        (cond
          (= (.length (vec command_stack)) 0)
          state

          :else
          (let [new_command_stack (pop-command-stack command_stack)]
            (assoc state COMMAND_STACK new_command_stack)))

        :else
        state)
        (cond
          ;change modes
          (= (.getCode input_key) (.getCode NORMAL_TO_COMMAND_MODE_KEY))
          (normal-to-command state)

          (= (.getCode input_key) (.getCode NORMAL_TO_INSERT_MODE_KEY))
          (normal-to-insert state)

          (= (.getCode input_key) (.getCode NORMAL_TO_VISUAL_MODE_KEY))
          (normal-to-visual state)

          (= (.getCode input_key) KEY_H)
          (let [new_position (move-cursor-left position 1)]
            (assoc state POSITION new_position))

          (= (.getCode input_key) KEY_J)
          (let [new_position (move-cursor-down position 1)]
            (assoc state POSITION new_position))

          (= (.getCode input_key) KEY_K)
          (let [new_position (move-cursor-up position 1)]
            (assoc state POSITION new_position))

          (= (.getCode input_key) KEY_L)
          (let [new_position (move-cursor-right position 1)]
            (assoc state POSITION new_position))

          :else
          (let [new_command_stack (push-command-stack command_stack input_key)]
            (normal-evaluate-command-stack (assoc state COMMAND_STACK new_command_stack)))))))

;switch to command mode
;change cursor position when going from normal to command mode
;write NORMAL_TO_COMMAND_MODE_KEY onto command_stack
(defn- normal-to-command [state]
  (let [position (state POSITION)
        command_stack (state COMMAND_STACK)
        screen_height (get-screen-height)]
    (let [new_position (cons [16 (- screen_height 1)] position)
          new_command_stack [NORMAL_TO_COMMAND_MODE_KEY]
          new_mode COMMAND_MODE]
      (assoc state MODE new_mode POSITION new_position COMMAND_STACK new_command_stack))))

;switch to insert mode
(defn- normal-to-insert [state]
  (let [new_command_stack []
        new_mode INSERT_MODE]
    (assoc state MODE new_mode COMMAND_STACK new_command_stack)))

;switch to visual mode
(defn- normal-to-visual [state]
  (let [new_command_stack []
        new_mode VISUAL_MODE]
    (assoc state MODE new_mode COMMAND_STACK new_command_stack)))

;command mode
(declare command-evaluate-command-stack)
(declare command-push-command-stack)

;evaluate the command stack in command mode
(defn- command-evaluate-command-stack [state]
  (let [command_stack (state COMMAND_STACK)
        screen_height (get-screen-height)]
    (cond
      ;what is command_stack

      :else
      (let [error_string (str "invalid command: " (command-stack-to-string command_stack))]
        (generic-input-key-esc (assoc state ERROR_DISPLAY error_string))))))

(defn- command-push-command-stack [state]
  (let [position (state POSITION)
        input_key (state INPUT_KEY)
        command_stack (state COMMAND_STACK)]
    (if
      (.isSpecialCode input_key)
      (cond
        (= (.getCode input_key) KEY_BACKSPACE)
        (cond
          (< (.length (vec command_stack)) 2) ;go back to NORMAL_MODE
          (let [new_position [(last position)]
                new_mode NORMAL_MODE
                new_command_stack []]
            (assoc state POSITION new_position MODE new_mode COMMAND_STACK new_command_stack))

          :else
          (let [new_position (move-cursor-left position 1)
                new_command_stack (pop-command-stack command_stack)]
            (assoc state POSITION new_position COMMAND_STACK new_command_stack))))
      (cond
        (= (.getCode input_key) KEY_RETURN)
        (command-evaluate-command-stack state)

        :else
        (let [new_position (move-cursor-right position 1)
              new_command_stack (push-command-stack command_stack input_key)]
          (assoc state POSITION new_position COMMAND_STACK new_command_stack))))))

;insert mode

;visual mode

;
(defn- main-loop [state]
  (loop [recursive_state state]
    (let [input_key (recursive_state INPUT_KEY)
          mode (recursive_state MODE)]
      (if input_key
        (cond
          ;GLOBAL special codes
          (= (.getCode input_key) (.getCode TO_NORMAL_MODE_KEY))
          (let [new_state (generic-input-key-esc recursive_state)]
            (recur (update new_state)))

          :else
          (cond
            (= mode NORMAL_MODE)
            (let [new_state (normal-push-command-stack recursive_state)]
              (recur (update new_state)))

            (= mode COMMAND_MODE)
            (let [new_state (command-push-command-stack recursive_state)]
              (recur (update new_state)))

            (= mode INSERT_MODE)
            (recur (update recursive_state))

            (= mode VISUAL MODE)
            (recur (update recursive_state))

            :else
            (recur (update recursive_state))))
        (do
          (recur (update recursive_state)))))))

(defn main []
  (do
    (init)
    (let [initial_state {BUFFER {0 "test" 1 "what do you want" 2 "" 3 "hello" 45 "45" 46 "46" 47 "47" 48 "48" 49 "49" 50 "50"}
                         POSITION [[0 0]]
                         MODE NORMAL_MODE
                         INPUT_KEY nil
                         COMMAND_STACK []
                         ANCHOR 0}]
      (main-loop initial_state))))

(main)

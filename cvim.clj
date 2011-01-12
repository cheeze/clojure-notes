(ns cvim
  (:import
     (jcurses.system.Toolkit)
     (jcurses.system.InputChar)
     (jcurses.system.CharColor))
  (:use cvim_constants))

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
  (let [x (get-x pos)
        y (get-y pos)]
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
    (print-string status_bar 0 (- screen_height 1))
    (print-string information_bar (- screen_width 25) (- screen_height 1))))


(defn- update [state]
  (do
    (clear-screen)
    (show-buffer state)
    (show-bar state)
    (move-cursor state)
    (assoc state INPUT_KEY (read-character))))

;MODE
;helpers
(declare helper-push-command-stack)
(defn- helper-push-command-stack [state]
  (let [command_stack (state COMMAND_STACK)
        input_key (state INPUT_KEY)]
    (if (not (.isSpecialCode input_key))
      (cond
        (= (.getCode input_key) KEY_RETURN)
        state

        :else
        (let [new_command_stack (concat command_stack (.toString input_key))]
          (assoc state COMMAND_STACK new_command_stack)))
      (cond
        (= (.getCode input_key) KEY_BACKSPACE)
        (let [new_command_stack (drop-last command_stack)]
          (assoc state COMMAND_STACK new_command_stack))))))

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
  (let [input_key (state INPUT_KEY)]
    (if (.isSpecialCode input_key)
      (cond
        :else
        state)
      (cond
        (= (.getCode input_key) (.getCode NORMAL_TO_COMMAND_MODE_KEY))
        (normal-to-command state)

        (= (.getCode input_key) NORMAL_TO_INSERT_MODE_KEY)
        (normal-to-insert state)

        (= (.getCode input_key) NORMAL_TO_VISUAL_MODE_KEY)
        (normal-to-visual state)

        :else
        (let [new_state (helper-push-command-stack state)]
          (normal-evaluate-command-stack new_state))))))

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
        (print-error error_string 0 (- screen_height 1))
        (read-character)
        (generic-input-key-esc state)))))

(defn- command-push-command-stack [state]
  (let [position (state POSITION)
        input_key (state INPUT_KEY)
        command_stack (state COMMAND_STACK)]
    (if
      (.isSpecialCode input_key)
      (cond
        (= (.getCode input_key) KEY_BACKSPACE)
        (cond
          (< (.length (vec command_stack)) 3) ;go back to NORMAL_MODE
          (let [new_position [(last position)]
                new_mode NORMAL_MODE
                new_command_stack []]
            (assoc state POSITION new_position MODE new_mode COMMAND_STACK new_command_stack))

          :else
          (let [new_position (cons [(- (get-x position) 1) (get-y position)] (rest position))
                new_command_stack (drop-last command_stack)]
            (assoc state POSITION new_position COMMAND_STACK new_command_stack))))
       (cond
         (= (.getCode input_key) KEY_RETURN)
         (command-evaluate-command-stack state)

         :else
         (let [new_state (helper-push-command-stack state)
               new_position (cons [(+ (get-x position) 1) (get-y position)] (rest position))]
           (assoc new_state POSITION new_position))))))

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
          (= (.getCode input_key) TO_NORMAL_MODE_KEY)
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
    (let [initial_state {
                         BUFFER {0 "test" 1 "what do you want" 2 "" 3 "hello" 45 "45" 46 "46" 47 "47" 48 "48" 49 "49" 50 "50"}
                         POSITION [[0 0]]
                         MODE NORMAL_MODE
                         INPUT_KEY nil
                         COMMAND_STACK []
                         ANCHOR 0
                         }]
      (main-loop initial_state))))

(main)

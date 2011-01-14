(ns cvim
  (:import
     (jcurses.system Toolkit)
     (jcurses.system InputChar)
     (jcurses.system CharColor)
     (java.io BufferedWriter)
     (java.io FileWriter)
     (java.io BufferedReader)
     (java.io FileReader)))

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
(def FILENAME 6)
(def MODIFIED 7)
(def SPECIAL_DISPLAY 20)

;configuration constants
;special keys
(def TO_NORMAL_MODE_KEY (new jcurses.system.InputChar KEY_ESC))
(def NORMAL_TO_COMMAND_MODE_KEY (new jcurses.system.InputChar KEY_COLON))
(def NORMAL_TO_INSERT_MODE_KEY (new jcurses.system.InputChar KEY_I))
(def NORMAL_TO_VISUAL_MODE_KEY (new jcurses.system.InputChar KEY_V))

(def TEXT_COLOR (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

(def SPECIAL_COLOR (new jcurses.system.CharColor (jcurses.system.CharColor/WHITE) (jcurses.system.CharColor/RED)))

;Toolkit calls
(defn init []
  (jcurses.system.Toolkit/init))

(defn shutdown []
  (jcurses.system.Toolkit/shutdown))

(defn get-screen-width []
  (jcurses.system.Toolkit/getScreenWidth))

(defn get-screen-height []
  (jcurses.system.Toolkit/getScreenHeight))

(defn clear-screen []
  (jcurses.system.Toolkit/clearScreen TEXT_COLOR))

(defn print-string [string x y]
  (jcurses.system.Toolkit/printString string x y TEXT_COLOR))

(defn print-special [string x y]
  (jcurses.system.Toolkit/printString string x y SPECIAL_COLOR))

(defn read-character []
  (jcurses.system.Toolkit/readCharacter))

(defn move [x y]
  (jcurses.system.Toolkit/move y x))

;function declarations
(declare update-state)

(declare move-cursor)
(declare get-x)
(declare get-y)
(declare position-to-string)
(declare mode-to-string)
(declare input-key-to-string)
(declare command-stack-to-string)
(declare anchor-to-string)

(declare construct-status-bar)
(declare construct-information-bar)

(declare show-text)
(declare show-buffer)
(declare show-bar)
(declare update)

(declare push-command-stack)
(declare pop-command-stack)

(declare move-cursor-up)
(declare move-cursor-down)
(declare move-cursor-left)
(declare move-cursor-right)

(declare normalize-buffer)

(declare open-filename)
(declare open-command)
(declare write-out-buffer)
(declare write-command)
(declare quit-command)

(declare generic-input-key-esc)

(declare normal-evaluate-command-stack)
(declare normal-push-command-stack)
(declare normal-to-command)
(declare normal-to-insert)
(declare normal-to-visual)

(declare normal-move-cursor-up)
(declare normal-move-cursor-down)
(declare normal-move-cursor-left)
(declare normal-move-cursor-right)

(declare command-evaluate-command-stack)
(declare command-push-command-stack)

(declare insert-move-buffer-up)
(declare insert-move-buffer-down)

(declare insert-input-key)
(declare insert-remove-input-key)
(declare insert-push-command-stack)

;global helpers
(def update-state
  (fn update-state
    ([map key val] 
     (if (= key BUFFER)
       (assoc map key val MODIFIED true)
       (assoc map key val)))
    ([map key val & kvs ]
     (let [ret (if (= key BUFFER)
                 (assoc map key val MODIFIED true)
                 (assoc map key val))]
       (if kvs
         (recur ret (first kvs) (second kvs) (nnext kvs))
         ret)))))

;display helpers
(defn- move-cursor [state]
  (let [position (state POSITION)
        anchor (state ANCHOR)
        mode (state MODE)]
    (if (= mode COMMAND_MODE)
      (move (get-x position) (get-y position))
      (move (get-x position) (- (get-y position) anchor)))))

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

(defn- anchor-to-string [anchor]
  (java.lang.Integer/toString anchor))

;CONSTRUCTION
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
        input_key (state INPUT_KEY)
        anchor (state ANCHOR)]
    (let [position_str (position-to-string position)
          input_key_str (input-key-to-string input_key)
          anchor_str (anchor-to-string anchor)]
      (str CURRENT_POSITION_STR position_str " key: " input_key_str " anchor: " anchor_str))))

;DISPLAY
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
    (print-string information_bar (- screen_width 40) (- screen_height 1))
    (if (state SPECIAL_DISPLAY)
      (print-special (state SPECIAL_DISPLAY) 0 (- screen_height 1))
      (print-string status_bar 0 (- screen_height 1)))))

(defn- update-anchor [state]
  (let [position_y (second (last (state POSITION)))
        height (- (get-screen-height) 2)
        anchor (state ANCHOR)]
    (cond
      (> position_y (+ height anchor))
      (update-state state ANCHOR (- position_y height))

      (< position_y anchor)
      (update-state state ANCHOR position_y)

      :else
      state)))

(defn- continue [state]
  (update-state state INPUT_KEY (read-character) SPECIAL_DISPLAY nil))

(defn- update [state]
  (let [state (update-anchor state)]
    (clear-screen)
    (show-buffer state)
    (show-bar state)
    (move-cursor state)
    (continue state)))

;COMMANDS
(defn- open-filename [filename]
  (with-open [reader (new BufferedReader (new FileReader filename))]
    (let [line (line-seq reader)]
      (normalize-buffer (reduce merge (map (defn f [k v] {k v}) (range (.length (vec line))) line))))))

(defn- open-command [state command_str]
  (let [filename (get (.split command_str " ") 1)]
    (if (and (not (= filename nil)) (not (= (.length filename) 0)))
      (let [new_buffer (open-filename filename)]
        (generic-input-key-esc (update-state state POSITION [[0 0]] FILENAME filename BUFFER new_buffer SPECIAL_DISPLAY (str "opening" "\"" filename "\"") MODIFIED false)))
      (generic-input-key-esc (update-state state SPECIAL_DISPLAY "no file name [:o <filename>]")))))

(defn- write-out-buffer [filename buffer]
  (with-open [writer (new BufferedWriter (new FileWriter filename))]
    (dotimes [i (+ (reduce max (keys buffer)) 1)]
      (.write writer (str (buffer i) "\n")))))

(defn- write-command [state command_str]
  (let [filename (get (.split command_str " ") 1)]
    (if (and (not (= filename nil)) (not (= (.length filename) 0)))
      (do
        (write-out-buffer filename (state BUFFER))
        (generic-input-key-esc (update-state state FILENAME filename SPECIAL_DISPLAY (str "writing " "\"" filename "\"") MODIFIED false)))
      (generic-input-key-esc (update-state state SPECIAL_DISPLAY "no file name [:w <filename>]")))))

(defn- quit-command [state]
  (if (state MODIFIED)
    (generic-input-key-esc (update-state state SPECIAL_DISPLAY "file modified"))
    (do
      (shutdown)
      (System/exit 0))))

;MODE
;helpers
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

(defn- pop-command-stack [command_stack]
  (drop-last command_stack))

(defn- move-cursor-up [position n]
  (let [new_y (- (get-y position) n)]
    (if (< new_y 0)
      position
      (cons [(get-x position) (- (get-y position) n)] (rest position)))))

(defn- move-cursor-down [position n]
  (move-cursor-up position (* n -1)))

(defn- move-cursor-left [position n]
  (let [new_x (- (get-x position) n)]
    (if (< new_x 0)
      position
      (cons [(- (get-x position) n) (get-y position)] (rest position)))))

(defn- move-cursor-right [position n]
  (move-cursor-left position (* n -1)))

(defn- normalize-buffer [buffer]
  (if buffer
    (let [line_count (reduce max (keys buffer))
          create_buffer (defn f [k] (if (buffer k) {k (buffer k)} {k ""}))]
      (reduce merge (map create_buffer (range (+ line_count 1)))))
    {0 ""}))

;generic
(defn- generic-input-key-esc [state]
  (let [position (state POSITION)]
    (let [new_position [(last position)]
          new_mode NORMAL_MODE
          new_command_stack []]
      (update-state state MODE new_mode POSITION new_position COMMAND_STACK new_command_stack))))

;normal mode
;evaluate the command stack in normal mode
(defn- normal-evaluate-command-stack [state]
  (let [command_stack (state COMMAND_STACK)]
    (cond
      ;what is command_stack

      :else
      state)))

(defn- normal-move-vertical-helper [state new_x new_y]
  (let [buffer (state BUFFER)]
    (let [line (buffer new_y)]
      (if (> new_x (.length line))
        (let [new_position [[(.length line) new_y]]]
          (update-state state POSITION new_position))
        (let [new_position [[new_x new_y]]]
          (update-state state POSITION new_position))))))

(defn- normal-move-cursor-up [state]
  (let [position (state POSITION)]
    (let [new_y (get-y (move-cursor-up position 1))
          new_x (get-x position)]
      (normal-move-vertical-helper state new_x new_y))))

(defn- normal-move-cursor-down [state]
  (let [buffer (state BUFFER)
        position (state POSITION)]
    (let [new_y (get-y (move-cursor-down position 1))
          new_x (get-x position)]
      (if (buffer new_y)
        (normal-move-vertical-helper state new_x new_y)
        state))))

(defn- normal-move-cursor-left [state]
  (let [position (state POSITION)]
    (let [new_position (move-cursor-left position 1)]
      (update-state state POSITION new_position))))

(defn- normal-move-cursor-right [state]
  (let [buffer (state BUFFER)
        position (state POSITION)]
    (let [new_position (move-cursor-right position 1)
          line (buffer (get-y position))]
      (if (> (get-x new_position) (.length line))
        state
        (update-state state POSITION new_position)))))

(defn- normal-push-command-stack [state]
  (let [buffer (state BUFFER)
        position (state POSITION)
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
            (update-state state COMMAND_STACK new_command_stack)))

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
        (normal-move-cursor-left state)

        (= (.getCode input_key) KEY_J)
        (normal-move-cursor-down state)

        (= (.getCode input_key) KEY_K)
        (normal-move-cursor-up state)

        (= (.getCode input_key) KEY_L)
        (normal-move-cursor-right state)

        :else
        (let [new_command_stack (push-command-stack command_stack input_key)]
          (normal-evaluate-command-stack (update-state state COMMAND_STACK new_command_stack)))))))

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
      (update-state state MODE new_mode POSITION new_position COMMAND_STACK new_command_stack))))

;switch to insert mode
(defn- normal-to-insert [state]
  (let [new_command_stack []
        new_mode INSERT_MODE]
    (update-state state MODE new_mode COMMAND_STACK new_command_stack)))

;switch to visual mode
(defn- normal-to-visual [state]
  (let [new_command_stack []
        new_mode VISUAL_MODE]
    (update-state state MODE new_mode COMMAND_STACK new_command_stack)))

;command mode
;evaluate the command stack in command mode
(defn- command-evaluate-command-stack [state]
  (let [command_stack (state COMMAND_STACK)
        screen_height (get-screen-height)]
    (let [command_str (command-stack-to-string command_stack)]
      (cond
        (.startsWith command_str ":o")
        (open-command state command_str)

        (.startsWith command_str ":w")
        (write-command state command_str)

        (.startsWith command_str ":q")
        (quit-command state)

        :else
        (generic-input-key-esc (update-state state SPECIAL_DISPLAY (str "invalid command: " command_str)))))))

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
            (update-state state POSITION new_position MODE new_mode COMMAND_STACK new_command_stack))

          :else
          (let [new_position (move-cursor-left position 1)
                new_command_stack (pop-command-stack command_stack)]
            (update-state state POSITION new_position COMMAND_STACK new_command_stack))))
      (cond
        (= (.getCode input_key) KEY_RETURN)
        (command-evaluate-command-stack state)

        :else
        (let [new_position (move-cursor-right position 1)
              new_command_stack (push-command-stack command_stack input_key)]
          (update-state state POSITION new_position COMMAND_STACK new_command_stack))))))

;insert mode
;in both functions, y_position defines where the current cursor is
(defn- insert-move-buffer-up [buffer y_position]
  (let [line_count (reduce max (keys buffer))]
    (let [decrement (defn f [line_number] 
                      (cond
                        (>= line_number y_position) 
                        {(- line_number 1) (buffer line_number)}

                        (< line_number (- y_position 1))
                        {line_number (buffer line_number)}

                        (= line_number (- y_position 1))
                        {}))]
      (reduce merge (map decrement (keys buffer))))))

(defn- insert-move-buffer-down [buffer y_position]
  (let [increment (defn f [line_number] 
                    (if (>= line_number y_position) 
                      {(+ line_number 1) (buffer line_number)}
                      {line_number (buffer line_number)}))]
    (reduce merge (map increment (keys buffer)))))

(defn- insert-add-input-key [state]
  (let [buffer (state BUFFER)
        position (state POSITION)
        input_key (state INPUT_KEY)]
    (let [line (buffer (get-y position))
          new_position (move-cursor-right position 1)]
      (if line
        (let [pre (.substring line 0 (get-x position))
              post (.substring line (get-x position) (.length line))]
          (let [new_buffer (assoc buffer (get-y position) (str pre (.toString input_key) post))]
            (update-state state BUFFER new_buffer POSITION new_position)))
        (let [new_buffer (assoc buffer (get-y position) (.toString input_key))]
          (update-state state BUFFER new_buffer POSITION new_position))))))

(defn- insert-remove-input-key [state]
  (let [buffer (state BUFFER)
        position (state POSITION)]
    (if (= (get-x position) 0)
      ;backspace at beginning of line
      (if (> (get-y position) 0)
        ;not at top line
        (if (= (.length (buffer (- (get-y position) 1))) 0)
          ;line above has no text
          (let [new_buffer (insert-move-buffer-up buffer (get-y position))
                new_position (move-cursor-up position 1)]
            (update-state state BUFFER new_buffer POSITION new_position))
          ;line above has text
          (let [above_line (buffer (- (get-y position) 1))]
            (let [new_buffer (insert-move-buffer-up buffer (get-y position))
                  new_position (move-cursor-right (move-cursor-up position 1) (.length above_line))
                  above_y_position (- (get-y position) 1)]
              (let [new_buffer (assoc new_buffer above_y_position (str above_line (new_buffer above_y_position)))]
                (update-state state BUFFER new_buffer POSITION new_position)))))
        ;at top line, should not move
        state)
      ;backspace at anywhere in the line
      (let [line (buffer (get-y position))
            new_position (move-cursor-left position 1)]
        (let [pre (.substring line 0 (- (get-x position) 1))
              post (.substring line (get-x position) (.length line))]
          (if (= (.length (str pre post)) 0)
            (let [new_buffer (assoc buffer (get-y position) "")]
              (update-state state BUFFER new_buffer POSITION new_position))
            (let [new_buffer (assoc buffer (get-y position) (str pre post))]
              (update-state state BUFFER new_buffer POSITION new_position))))))))

(defn- insert-return-input-key [state]
  (let [buffer (state BUFFER)
        position (state POSITION)
        input_key (state INPUT_KEY)]
    (if (= (get-x position) 0)
      ;return at beginning of line
      (let [new_buffer (assoc (insert-move-buffer-down buffer (get-y position)) (get-y position) "")
            new_position (move-cursor-down position 1)]
        (update-state state BUFFER new_buffer POSITION new_position))
      ;return at anywhere else
      (let [line (buffer (get-y position))]
        (let [pre (.substring line 0 (get-x position))
              post (.substring line (get-x position) (.length line))]
          (let [new_buffer (insert-move-buffer-down buffer (get-y position))]
            (let [new_buffer (assoc (assoc new_buffer (get-y position) pre) (+ (get-y position) 1) post)
                  new_position (move-cursor-left (move-cursor-down position 1) (.length pre))]
              (update-state state BUFFER new_buffer POSITION new_position))))))))

(defn- insert-push-command-stack [state]
  (let [buffer (state BUFFER)
        position (state POSITION)
        input_key (state INPUT_KEY)]
    (if (.isSpecialCode input_key)
      (cond
        (= (.getCode input_key) KEY_BACKSPACE)
        (insert-remove-input-key state)

        :else
        state)
      (cond
        (= (.getCode input_key) KEY_RETURN)
        (insert-return-input-key state)

        :else
        (insert-add-input-key state)))))

;visual mode
(defn- visual-push-command-stack [state]
  state)

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
            (let [new_state (insert-push-command-stack recursive_state)]
              (recur (update new_state)))

            (= mode VISUAL MODE)
            (let [new_state (visual-push-command-stack recursive_state)]
              (recur (update new_state)))

            :else
            (recur (update recursive_state))))
        (do
          (recur (update recursive_state)))))))

(defn main []
  (do
    (init)
    (let [filename (if *command-line-args* (.get *command-line-args* 0) nil)]
      (let [buffer (if filename
                     (open-filename filename)
                     (normalize-buffer {0 ""}))]
        (let [initial_state {BUFFER buffer
                             POSITION [[0 0]]
                             MODE NORMAL_MODE
                             INPUT_KEY nil
                             COMMAND_STACK []
                             ANCHOR 0
                             MODIFIED false}]
          (main-loop initial_state))))))

(main)

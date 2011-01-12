(ns cvim_constants
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
(def KEY_I 105)
(def KEY_V 118)
(def KEY_BACKSPACE 263)

;state keys
(def BUFFER 0)
(def POSITION 1)
(def MODE 2)
(def INPUT_KEY 3)
(def COMMAND_STACK 4)
(def ANCHOR 5)

;configuration constants
;special keys
(def TO_NORMAL_MODE_KEY (new jcurses.system.InputChar KEY_ESC))
(def NORMAL_TO_COMMAND_MODE_KEY (new jcurses.system.InputChar KEY_COLON))
(def NORMAL_TO_INSERT_MODE_KEY (new jcurses.system.InputChar KEY_I))
(def NORMAL_TO_VISUAL_MODE_KEY (new jcurses.system.InputChar KEY_V))

(def TEXT_COLOR (new jcurses.system.CharColor (jcurses.system.CharColor/BLACK) (jcurses.system.CharColor/WHITE)))

(def ERROR_COLOR (new jcurses.system.CharColor (jcurses.system.CharColor/WHITE) (jcurses.system.CharColor/RED)))

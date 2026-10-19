;;; keybindings.el --- my-pi layer keybindings file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sebastian Hahn <sebastian.hahn@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; `$' is Spacemacs' "AI" prefix (used by the llm-client, openai and
;; claude-code layers).  pi fits under it as `SPC $ p'.
(spacemacs/declare-prefix "$" "AI")
(spacemacs/declare-prefix "$p" "pi")

(spacemacs/set-leader-keys
  "$pj" 'pi-start                     ; start a new pi RPC session
  "$ps" 'pi-send-prompt               ; send a prompt
  "$pa" 'pi-abort                     ; abort current run
  "$pn" 'pi-new-session               ; start a fresh pi session
  "$pc" 'pi-compact                   ; compact context
  "$pm" 'pi-select-model              ; pick the active model
  "$pM" 'pi-cycle-model               ; cycle active model
  "$pt" 'pi-set-thinking-level        ; pick the thinking level
  "$pp" 'pi-bash                      ; run a bash command through pi
  "$pg" 'pi-refresh                   ; refresh session state
  "$pb" 'pi-switch-to-buffer          ; switch between pi sessions
  "$pq" 'pi-quit                      ; quit current pi session
)

;; Major-mode bindings inside a pi session buffer (`,' in normal state).
(spacemacs/set-leader-keys-for-major-mode 'pi-mode
  "," 'pi-send                        ; send the bottom prompt
  "a" 'pi-abort
  "n" 'pi-new-session
  "c" 'pi-compact
  "m" 'pi-select-model
  "M" 'pi-cycle-model
  "t" 'pi-set-thinking-level
  "p" 'pi-bash
  "g" 'pi-refresh
  "b" 'pi-switch-to-buffer
  "q" 'pi-quit)

;;; keybindings.el ends here

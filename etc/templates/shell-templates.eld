sh-mode

;; (shell-template  #!/usr/bin/env bash

;; # Uncomment for error handling
;; # set -e  # Exit immediately if a command exits with non-zero status
;; # trap 'echo "Error on line $LINENO"; exit 1' ERR
;; # trap 'echo "Script interrupted."; exit 130' INT

;; show_usage() {
;;   echo "Usage: $0 ..."
;; }

;; process_item() {
;;   local item="$1"
;;   echo "Processing '$item'"
;; }

;; main() {
;;   if [ $# -eq 0 ]; then
;;     show_usage
;;     exit 1
;;   fi

;;   # Your code here
;;   for var in "$@"; do
;;     process_item "$var"
;;   done
;; }

;; main "$@"
;; ;; shell script template
;; ;; from: https://collin.blog/2025/03/02/shell-script-template/
;; ;; "...something basic with enough structure to remind me how I like to do
;; ;; things. I thought maybe it would be helpful for other people who don’t write
;; ;; shell scripts too often but want to them to be nice when they do."
;; ;;
;; )

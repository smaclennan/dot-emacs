;; -*- lexical-binding: t -*-
;; LIFO stack macros

;; At the cost of a sentinel ('STACK) at the start of the stack we can
;; create a very efficient push/pop that has no branches. Because we
;; are always using setcdr to a non-empty stack, we never need to
;; assign back to the stack variable.

;; We always push and pop from the end of the list. When we get down
;; to just the sentinel, the `cdr' in `stack-pop' will return nil
;; `car-safe' will return nil. `car-safe' is slightly more efficient
;; than `car'.

;; `stack-clear' guarantees that the sentinel is kept on the list.

(defmacro stack-create ()
  "Create an empty lifo stack."
  `(cons 'STACK nil))

(defmacro stack-push (stack element)
  "Push an element onto the stack."
  `(setcdr ,stack (cons ,element (cdr ,stack))))

(defmacro stack-pop (stack)
  "Remove the topmost element from STACK and return it.
If the stack is empty, return nil."
  `(prog1
      (car-safe (cdr ,stack))
    (setcdr ,stack (cdr-safe (cdr ,stack)))))

(defmacro stack-top (stack)
  "Return the topmost element of STACK or nil if it is empty."
  `(car-safe (cdr ,stack)))

(defmacro stack-clear (stack)
  "Remove all elements from STACK."
  `(setcdr ,stack nil))

(provide 'stack)

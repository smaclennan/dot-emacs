;; LIFO stack macros

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

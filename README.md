# what even is this lisp

"Welcome."

## messy organised findings that ill make nice later

- once program is running, you can run commands in the repl and it will make a cool output

  - current allowed functions seem to be `+, -, *, %, defun` (line 28)

- if you want to add more functions, you have to first add them to the list on line 28 and then also add to the chunk of `((eq...`s below them

  - example for future use:

    ```lisp
    (defun waow ()
    (format t ":3"))
    ```

    ```lisp
    (list '+ '- '* '/ '% 'defun 'waow)
    ```

    ```lisp
    ((eq 'waow) (waow))
    ```

- what is even going on here
- uhh if you try to function that doesnt exist it prints it into a textfile for some reason?
- can we make the error handling nicer
  - side quest :3
- ok also robot turtles in lisp
  - side quest 2: electric boogaloo :3

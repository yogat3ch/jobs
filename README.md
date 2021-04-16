# Jobs

A basic Rstudio Addin that allows for code to be rapidly run as a background task.
The creative motivation comes from finding that some R code can occupy the console for long enough to interrupt one's workflow - with this addin one can quickly send slow code to be run as a background task and continue using the console while it completes.

This can be done via passing the code to be executed as an expression to the function:

```
jobs::jobscript({purrr::walk(1:10, ~{message(.x); Sys.sleep(1)})})
```

or by copying the code to the clipboard:


```
purrr::walk(1:10, ~{message(.x); Sys.sleep(1)}) # Copy This Ctrl/Cmd + C
jobscript()
```

Or better yet, bind the addin to a key combination like Alt + B Alt + G (Alt B G) to quickly run the code that is currently on the clipboard.


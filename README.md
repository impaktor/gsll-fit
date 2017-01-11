# GSLL-fit
This is my experiment to use GSL (GNU Scientific library) for C, but rather
than using C/C++, I use the common lisp bindings, by
using [GSLL](https://common-lisp.net/project/gsll/), to fit a function to
data.

In particular, I'll start from the fitting [example](https://www.gnu.org/software/gsl/manual/html_node/Multimin-Examples.html#Multimin-Examples) or
[here](https://www.gnu.org/software/gsl/manual/html_node/Providing-a-function-to-minimize.html#Providing-a-function-to-minimize).


## How I do it

Install GSLL, and load it up
```
(ql:quickload "gsll")
(asdf:operate 'asdf:load-op :gsll)
```

Then to lookup the common lisp binding and documentation for, say GSL function `gsl_matrix_get_col`:
```
CL-USER> (gsl:gsl-lookup "gsl_matrix_get_col")
GSLL:COLUMN
T
CL-USER> (documentation #'GSLL:COLUMN 'function)
"Copy the elements of the ith column of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the column."
```

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_min.h>

/* https://www.gnu.org/software/gsl/manual/html_node/Multimin-Examples.html#Multimin-Examples */

/* Bäst (samma som gsl-doc, men på en sida?):
 http://linux.math.tifr.res.in/programming-doc/gsl/gsl-ref_34.html */

/* Paraboloid centered on (p[0],p[1]), with
   scale factors (p[2],p[3]) and minimum p[4] */
/* https://www.gnu.org/software/gsl/manual/html_node/Providing-a-function-to-minimize.html#Providing-a-function-to-minimize */

double my_f(const gsl_vector *v, void *params)
{
  double x, y;
  double *p = (double *)params;

  x = gsl_vector_get(v, 0);
  y = gsl_vector_get(v, 1);

  return p[2] * (x - p[0]) * (x - p[0]) +
    p[3] * (y - p[1]) * (y - p[1]) + p[4];
}


int main(void)
{
  /* Position of the minimum (1,2), scale factors
     10,20, height 30. */
  double parameters[5] = { 1.0, 2.0, 10.0, 20.0, 30.0 };

  gsl_vector *x;
  gsl_multimin_function my_func;   /* LISP: NIL */

  my_func.n = 2;                /* size_t, dimension of system, i.e. number of components of vector x. */
  my_func.f = &my_f;            /* double (* f) (const gsl_vector * x, void * params)
                                   function should return f(x, params) for x and params */
  my_func.params = (void *)parameters;

  /* Starting point, x = (5,7) */
  x = gsl_vector_alloc (2);              /* LISP: NIL */
  gsl_vector_set(x, 0, 5.0);
  gsl_vector_set(x, 1, 7.0);

  const gsl_multimin_fminimizer_type *T;  /* LISP: NIL */
  gsl_multimin_fminimizer *s;             /* LISP: NIL */

  /* Nelder and Mead; LISP: GSLL:+SIMPLEX-NELDER-MEAD-ON2+*/
  T = gsl_multimin_fminimizer_nmsimplex;

  /* LISP: GSLL:MAKE-MULTI-DIMENSIONAL-MINIMIZER-F */
  s = gsl_multimin_fminimizer_alloc(T, 2);

  /* Needed if fminimize instead of fdfminimize. The size of the
     initial trial steps, from point x, is given in vector step_size.
     The precise meaning of this parameter depends on the method
     used. */
  gsl_vector *step_size;

  step_size = gsl_vector_alloc (2);
  gsl_vector_set(step_size, 0, 0.01);
  gsl_vector_set(step_size, 1, 0.01);

   /* LISP: (REINITIALIZE-INSTANCE GSLL:MULTI-DIMENSIONAL-MINIMIZER-F) */
  gsl_multimin_fminimizer_set(s, &my_func, x, step_size);


  size_t iter = 0;
  int status;
  do{
    iter++;

    /* perform a single iteration of the minimizer s. The minimizer
       maintains a current best estimate of the minimum at all times.
       This information can be accessed with auxiliary functions, */
    status = gsl_multimin_fminimizer_iterate(s);  /* LISP: GSLL:ITERATE */

    /* In case of error code */
    if(status)
      break;

    /* A minimization procedure should stop when one of the following conditions is true:
     - A minimum has been found to within the user-specified precision.
     - A user-specified maximum number of iterations has been reached.
     - An error has occurred.  */

    /* Size of the simplex thingy in space, contracts closer to minimum */
    /* LISP: GSLL:SIZE */
    double some_size = gsl_multimin_fminimizer_size(s);

    /* The test returns GSL_SUCCESS if the following condition is
       achieved, LISP: GSLL:MIN-TEST-SIZE */
    status = gsl_multimin_test_size(some_size, 1e-3);

    if(status == GSL_SUCCESS)   /* LISP: NIL */
      printf("Minimum found at:\n");

    printf("%5d %.5f %.5f %10.5f\n", iter,
            gsl_vector_get(s->x, 0),
            gsl_vector_get(s->x, 1),
            s->f);

  }while(status == GSL_CONTINUE && iter < 100);

  gsl_multimin_fminimizer_free(s); /* LISP: NIL */
  gsl_vector_free(x);              /* LISP: NIL */

  return 0;
}

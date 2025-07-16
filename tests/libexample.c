// C code example (libexample.c)
#include <stdio.h>

int my_global_variable = 123;

int my_function (int x)
{
  printf ("C: my_function called with %d\n", x);
  printf ("C: Current my_global_variable = %d\n", my_global_variable);
  my_global_variable += x;
  printf ("C: New my_global_variable = %d\n", my_global_variable);
  return x * x;
}

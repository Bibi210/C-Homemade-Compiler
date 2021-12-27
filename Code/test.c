#include <stdio.h>
#include <stdlib.h>
int add_aux(int a, int b) {
  printf("A = ");
  printf("%d\n", a);
  printf("B = ");
  printf("%d\n", b);
  return a + b;
}

int add(int n) {
  printf("add: n = ");
  printf("%d\n", n);
  return (add_aux(4, n) + add_aux(1, n));
}

int main() {
  printf("%d\n", add( 10));
  printf("Wtf");
  printf("\n");
  return 0;
}
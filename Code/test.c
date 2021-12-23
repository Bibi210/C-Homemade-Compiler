#include <stdio.h>
#include <stdlib.h>
int main() {
label:
  switch (10) {
  case 0:
    0;
    int t;
    goto label;
  case abs(1):
  default:
    t = 1;
    printf("%d\n", t);
    break;
  }
  return 0;
}
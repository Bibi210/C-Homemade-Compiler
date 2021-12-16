#include <stdio.h>
int main() {
  int a = 20;
  while (a) {
    if (a % 2)
      printf("A est impair car ");
    else
      printf("A est pair car ");
    printf("A %% 2 = ");
    printf("%d",a % 2);
    printf("\n");
    a = a - 1;
  }
  return 0;
}
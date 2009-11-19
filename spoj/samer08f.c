// https://www.spoj.pl/problems/SAMER08F/

#include <stdio.h>

int f(int n) {
  int k;
  int sum = 0;
  for (k = 0 ; k<n ; ++k) {
    sum += (n-k) * (n-k);
  }
  return sum;
}

int read_int() {
  char s[20];
  return atoi(gets(s));
}

int main(int argc, char** argv) {
  for(;;) {
    int n = read_int();
    if (n == 0) {
      break;
    }
    printf("%d\n", f(n));
  }
  
  return 0;
}

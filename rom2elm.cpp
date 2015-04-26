#include <stdio.h>

int main(int argc, char ** argv) {
  if (argc != 2) {
    fprintf(stderr, "No ROM file\n");
    return 1;
  }
  freopen(argv[1], "rb", stdin);
  printf("words = List.map fromDecOct decoctal\n");
  printf("\n");
  printf("fromDecOct : Int -> Int\n");
  printf("fromDecOct n = if n == 0 then 0 else n %% 8 + 8 * (fromDecOct (n // 10))\n");
  printf("\n");
  printf("decoctal = ");
  bool first = true;
  while (true) {
    int c1 = getchar();
    int c2 = getchar();
    if (c2 < 0) break;
    int w16 = c2 * 256 + c1;
    if (first) {
      printf("[ ");
      first = false;
    } else {
      printf("\n  , ");
    }
    printf("%o", w16);
  }
  printf(" ]\n");
  return 0;
}

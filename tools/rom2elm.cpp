#include <stdio.h>
#include <string>

int main(int argc, char ** argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <ROM-file without .rom extension>\n", argv[0]);
    return 1;
  }
  std::string filename = argv[1];
  filename += ".rom";
  freopen(filename.c_str(), "rb", stdin);
  printf("module %s where\n", argv[1]);
  printf("\n");
  printf("import Util\n");
  printf("\n");
  printf("words = List.map Util.oct decoctal\n");
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
      printf("\n           , ");
    }
    printf("%o", w16);
  }
  printf(" ]\n");
  return 0;
}

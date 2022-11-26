#include <stdio.h>
#include <string.h>
#include "text_manipulation.h"

/******************************************************** 
 * Prints a pass or fail message for the specified test.* 
 ********************************************************/
void test_assert(int test_result, const char *test_name, int test_number) {
   if (test_result)  {
      printf("pass %s %d\n", test_name, test_number);
   } else {
      printf("fail %s %d\n", test_name, test_number);
   }
}

int main() {
   char result[MAX_STR_LEN + 1]; 
   char test[MAX_STR_LEN + 1];
   char test2[MAX_STR_LEN + 1];
   char test3[MAX_STR_LEN + 1];
   char test4[MAX_STR_LEN + 1];
   char test5[MAX_STR_LEN + 1];
   int spaces_removed;
   int spaces_removed2;

   center("terps", 7, result);
   printf("%s", result);
   printf("END\n");

   center("hello", 10, test);
   printf("%s", test);
   printf("END\n");

   center("hi", 3, test2);
   printf("%s", test2);
   printf("END\n");

   remove_spaces("   hey do  gs     ", test3, NULL);
   printf("%sEND\n", test3);
   
   remove_spaces("hey dog   ", test4, &spaces_removed);
   printf("%sEND\n", test4);
   printf("SPACES REMOVED: %d\n", spaces_removed);

   remove_spaces("    hey dog", test5, &spaces_removed2);
   printf("%sEND\n", test5);
   printf("SPACES REMOVED 2: %d\n", spaces_removed2);

   return 0;
}

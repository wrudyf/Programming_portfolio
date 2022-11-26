#include <stdio.h>
#include <string.h>

#define MAX 80

int main() {
   char name[MAX] = "The House is Blue";
   char *p = name, *q;
   int i, length;

   /* You can add and subtract integer values from pointers.   */
   /* For example, if you add one to a pointer to a character  */
   /* array, the pointer will now be referring to the next     */
   /* character.  You can add any integer value (not just one) */

   /* Printing the string using pointer arithmetic */
   while (*p != '\0') {
      printf("%c", *p);
      p = p + 1;
   }
   
   /* Printing the string in reverse order */
   printf("\nreversed\n");
   p = name + strlen(name);

   while (*p != 'T') {
      p = p - 1;
      printf("%c", *p);
   }
   printf("\n");

   /* You can tell how many elements are between two pointers */   
   /* by subtracting pointers */
   p = name + 1;   
   q = &name[5];
   printf("Elements #1: %ld\n", q - p);
   printf("Elements #2: %ld\n", p - q);

   /* Indexing is a pointer operation */
   printf("Indexing as pointer operation\n");
   p = name;
   length = strlen(name);
   for (i = 0; i < length; i++) {
      printf("%c\n", p[i]);
   }

   p[4] = '\0';
   printf("Final value: %s\n", p);

   return 0;
}

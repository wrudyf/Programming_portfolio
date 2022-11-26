#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>
#include <err.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

int main () {
  
  char line[1024], command[1024], args[1024];
  int check = 0;

  while (check == 0){
    command[0] = '\0';
    args[0] = '\0';
    fgets(line, 1024, stdin);
    sscanf(line, " %s %s", command, args);  
    printf("COMMAND: %s ", command);
    if (strlen(args) == 0){
      printf("NULL\n");
    }
    else{
      printf("| %s |\n", args);
    }

    fflush(stdin);
    fflush(stdin);
  }  

  return 0;
}

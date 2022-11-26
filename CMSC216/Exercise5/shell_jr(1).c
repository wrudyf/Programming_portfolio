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
    printf("shell_jr: ");
    fflush(stdout);
    /*set command and args to strlen = 0 at the start if in case previous 
      data was saved in either these char arrays and user does not overwrite 
      both things
    */
    command[0] = '\0';
    args[0] = '\0';

    if ( fgets(line, 1024, stdin) == NULL){
      break;
    }
    else{
      /*get command and args*/
      sscanf(line, " %s %s", command, args);

      /*handle exit and hastalavista commands*/
      if (strcmp(command, "exit") == 0 || 
	  strcmp(command, "hastalavista") == 0){
	printf("See you\n");
	exit(0);
      }

      /*handle cd command*/
      else if (strcmp(command, "cd") == 0){
	if (chdir(args) != 0){
	  printf("Cannot change to directory %s\n", args);
	}
      }

      /*handle one arg linux command*/
      else{
	pid_t process_id;
	/*get array of args*/
	char* arg_list[3];
	arg_list[0] = command;
	if (strlen(args) == 0){
	  arg_list[1] = NULL;
	}
	else{
	  arg_list[1] = args;
	}
	arg_list[2] = NULL;
	
	if ( (process_id = fork()) < 0){
	  err(EX_OSERR, "fork error\n");
	}

	if (process_id){
	  wait(NULL);
	}
	else{
	  execvp(arg_list[0], arg_list);

	  
	  printf("Failed to execute %s\n", command);
	  
	  fflush(stdout);
	  /*
	  err(EX_OSERR, "");
	  */


	  
	  exit(0);
	  
	}

      }
    }
    
  }  

  return 0;
}

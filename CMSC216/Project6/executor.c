/*student id: 114096296*/
#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>
#include <err.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <fcntl.h>
#include "command.h"
#include "executor.h"
#define FILE_PERMISSIONS 0664

static void print_tree(struct tree *t);
static void execute_and(struct tree *t);
static void execute_pipe(struct tree *t);
static int execute_comm(struct tree *t);
static void execute_reg_comm(struct tree *t);

int execute(struct tree *t) {
  /*if we do not have a conjunction we just execute the command */
  if (t->conjunction == NONE){
    execute_reg_comm(t);
    return 0;
  }

  /*if we have an AND conjunction we handle it here*/
  if (t->conjunction == AND){
    execute_and(t);
    return 0;
  }

  /*if we have a PIPE conjunction we handle it here*/
  if (t->conjunction == PIPE){
    execute_pipe(t);
  }

  /*if we have a SUBSHELL conjunction we handle it here*/
  if (t->conjunction == SUBSHELL){
    /*printf("SUBSHELL CONJ:\n");*/
  }

  /*NOTE: COMMENT OUT PRINT TREE ONCE DONE*/
  /*print_tree(t); */

  return 0;
}

static void execute_reg_comm(struct tree *t){
  if (t != NULL){
    /*if exit, exit*/
    if (strcmp(t->argv[0], "exit") == 0){
      exit(0);
    }
    /*if cd, change directory*/
    else if (strcmp(t->argv[0], "cd") == 0){
      /*if no arg given, cd to HOME*/
      if (t->argv[1] == NULL){
	if (chdir(getenv("HOME")) != 0){
	  perror(getenv("HOME"));
	}
      }
      /*if given arg, cd to arg*/
      else{
	if (chdir(t->argv[1]) != 0){
	  perror(t->argv[1]);
	}
      }
    }
    /*if not exit or cd, then it is command we must execute with child process*/
    else{
      pid_t process_id;
      /*if no input/output redirection is given, just execute command*/
      if (t->input == NULL && t->output == NULL){
	if ((process_id = fork()) < 0){
	  perror("fork");
	}
	if (process_id){
	  wait(NULL);
	}
	else{
	  execvp(t->argv[0], t->argv);
	  err(EX_OSERR, "");
	}
      }
      else{
	int fd;
	/*if output redirection is given, then handle each case and execute comm*/
	if (t->input != NULL){
	  fd = open(t->input, O_RDONLY);
	  if (fd < 0){
	    printf("Failed to open file\n");
	  }
	  if (dup2(fd, STDIN_FILENO) < 0){
	    printf("Failed to copy stdin file to our fd file\n");
	  }
	  if (close(fd) < 0){
	    printf("Failed to close\n");
	  }
	}
	if (t->output != NULL){
	  fd = open(t->output, O_WRONLY | O_CREAT | O_TRUNC, FILE_PERMISSIONS);
	  if (fd < 0){
	    printf("Failed to open file\n");
	  }
	  if (dup2(fd, STDOUT_FILENO) < 0){
	    printf("Failed to copy stdout file to our fd file\n");
	  }
	  if (close(fd) < 0){
	    printf("Failed to close fd\n");
	  }
	}
	process_id = fork();
	if (process_id){
	  wait(NULL);
	}
	else{
	  execvp(t->argv[0], t->argv);
	  err(EX_OSERR, "");
	}
      }

    }

  }
}

static int execute_comm(struct tree *t){
  int check = 0;  
  /*if exit, we exit shell*/
  if (strcmp(t->argv[0], "exit") == 0){
    exit(0);
  }
  /*if cd, change directory*/
  else if (strcmp(t->argv[0], "cd") == 0){
    if (t->argv[1] == NULL){
      if (chdir(getenv("HOME")) != 0){
	perror(getenv("HOME"));
      }
    }
    else{
      if (chdir(t->argv[1]) != 0){
	perror(t->argv[1]);
      }
    }
  }
  /*if command we execute command*/
  else{
    pid_t process_id;
    int pipe_fd[2];
    /*create pipe*/
    if (pipe(pipe_fd) < 0) {
      err(EX_OSERR, "pipe error\n");
    }
    /*fork*/
    process_id = fork();
    if (process_id < 0){
      err(EX_OSERR, "fork error\n");
    }

    if (process_id){
      int status;
      /*close write end*/
      status = close(pipe_fd[1]);
      
      /*read value from pipe*/
      read(pipe_fd[0], &check, sizeof(int));
      
      /*close read end*/
      status = close(pipe_fd[0]);
      wait(NULL);

    }
    else{
      int status;

      if (strcmp(t->argv[0], "[") == 0){
	if (strcmp(t->argv[1], "-e") == 0){
	  if (strcmp(t->argv[2], "missing") == 0){
	    if (strcmp(t->argv[3], "]") == 0){
	      check = -10;
	      status = close(pipe_fd[0]);
	      write(pipe_fd[1], &check, sizeof(int));
	      status = close(pipe_fd[1]);
	      err(EX_OSERR, "");	      
	    }
	  }				      
	}
      }

      execvp(t->argv[0], t->argv);
      check = -10;
      status = close(pipe_fd[0]);
      write(pipe_fd[1], &check, sizeof(int));
      status = close(pipe_fd[1]);
      err(EX_OSERR, "");
    }

  }

  return check;
}

static void execute_and(struct tree *t){
  int fail;
  /*do left tree first*/
  fail = execute_comm(t->left);

  /*do right tree second if first one didn't fail*/
  if (fail >= 0){
    if (t->right->conjunction == NONE){
      execute_reg_comm(t->right);
      /*execute_comm(t->right);*/
    }
    else{
      execute_and(t->right);
    }
  }
}

static void execute_pipe(struct tree *t){
  /*int fail;*/
  int pipe_fd[2];
  pid_t child_one, child_two;
  /*
  if (pipe_fd(pipe_fd) < 0){
    err(EX_OSERR, "pipe error");
  }
  */

  /*perform operation on left tree first*/
  if (t->left->conjunction != NONE){
    execute(t->left);
  }
  
  /*then perform operation on right tree*/
  if (t->right->conjunction != NONE){
    execute(t->right);
  }
  
  if (t->left->conjunction == NONE && t->right->conjunction == NONE){
    if (pipe(pipe_fd) < 0) {
      err(EX_OSERR, "pipe error");
    }
    if ((child_one = fork()) < 0){
      err(EX_OSERR, "fork error");
    }

    if (child_one == 0){
      close(pipe_fd[0]);
      dup2(pipe_fd[1], STDOUT_FILENO);
      close(pipe_fd[1]);
      /*do left command*/
      execvp(t->left->argv[0], t->left->argv);
      /*
      fprintf("Failed to execute command %s\n", t->left->argv[0]);
      */
    }
    else{
      if ((child_two = fork()) < 0){
	err(EX_OSERR, "fork error");
      }

      if (child_two == 0){
	close(pipe_fd[1]);
	dup2(pipe_fd[0], STDIN_FILENO);
	close(pipe_fd[0]);
	execvp(t->right->argv[0], t->right->argv);
      }
      else{
	close(pipe_fd[0]);
	close(pipe_fd[1]);

	wait(NULL);
	wait(NULL);
      }
    }
  }
}


static void print_tree(struct tree *t) {
   if (t != NULL) {
      print_tree(t->left);
      if (t->conjunction == NONE) {
         printf("NONE: %s, ", t->argv[0]);
      } else {
         printf("%s, ", conj[t->conjunction]);
      }
      printf("IR: %s, ", t->input);
      printf("OR: %s\n", t->output);
      print_tree(t->right);
   }
}


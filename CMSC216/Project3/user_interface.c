/*
Name: Rudy Fuentes
UID: 114096296
UMD Directory ID: rfuente5
*/
#include <stdio.h>
#include <string.h>
#include <sysexits.h>
#include <stdlib.h>
#include <ctype.h>
#include "document.h"

/*prototypes for functions*/
int is_comment(char *str);
int is_command(char *str, const char *command);

int main(int argc, char *argv[]) {
  /*INPUT FILE IF WE NEED IT*/
  FILE *input, *output;

  /*DOCUMENT STRUCTURE FOR US TO MAKE ADJUSTMENTS TO*/
  Document doc;

  /*LINE FOR US TO GET INPUT FROM FILE OR STDIN*/
  char line[1024];
  int check = 0;
  int h, i, paragraphs;
  char paragraph_string[2];
  init_document(&doc, "main_document");

  if (argc > 2){
    fprintf(stderr, "Usage: user_interface\nUsage: user_interface <filename>");
  }
  if (argc == 1){
    /*GET INPUT FROM STDIN, LIKE USING SCANF FROM P1*/
    
    while (check == 0){
      printf("> ");
      fgets(line, 1014, stdin);

      if (!is_comment(line)){
	continue;
      }     
      if (!is_command(line, "add_paragraph_after")){
	int paragraph_number = -1, j, length = strlen(line);
	char num[2] = "  ";
	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    num[0] = line[j];
	    if (isdigit(line[j + 1])){
	      num[1] = line[j + 1];
	      break;
	    }
	    break;
	  }
	}
	paragraph_number = atoi(num);
	add_paragraph_after(&doc, paragraph_number);
	continue;
      }
      if (!is_command(line, "add_line_after")){
	int  paragraph_number = -1, line_number = -1, j, length = strlen(line);
	int string_index = -1;
	char par_num[2] = "  ";
	char line_num[2] = "  ";
	char string[MAX_STR_SIZE + 1];
	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    if (par_num[0] == ' '){
	      par_num[0] = line[j];
	      if (isdigit(line[j + 1])){
		par_num[1] = line[j + 1];
	      }
	    }
	    else{
	      line_num[0] = line[j];
	      if (isdigit(line[j + 1])){
		line_num[1] = line[j + 1];
	      }
	    }
	  } 
	  if (line[j] == '*'){
	    string_index = 0;
	    continue;
	  }
	  if (string_index != -1){
	    string[string_index] = line[j];
	    string_index ++;
	  }
	}
	string[string_index - 1] = '\0';
	paragraph_number = atoi(par_num);
	line_number = atoi(line_num);
	add_line_after(&doc, paragraph_number, line_number, string);
	continue;
      }
      if (!is_command(line, "print_document")){

	print_document(&doc);

	continue;
      }
      if (!is_command(line, "quit")){
	break;
      }
      if (!is_command(line, "exit")){
	break;
      }
      if (!is_command(line, "append_line")){
	int j, paragraph_number, length = strlen(line), string_index = -1;
	char par_num[2];
	char string_a[MAX_STR_SIZE + 1];

	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    par_num[0] = line[j];
	    if (isdigit(line[j + 1])){
	      par_num[1] = line[j + 1];
	    }
	  }
	  if (line[j]  == '*'){
	    string_index = 0;
	    continue;
	  }
	  if (string_index != -1){
	    string_a[string_index] = line[j];
	    string_index++;
	  }
	}
	string_a[string_index - 1] = '\0';
	paragraph_number = atoi(par_num);
	append_line(&doc, paragraph_number, string_a);
	continue;
      }
      if (!is_command(line, "remove_line")){
	int j, paragraph_number, line_number, length = strlen(line);
	char par_num[2] = "  ";
	char line_num[2] = "  ";

	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    if (par_num[0] == ' '){
	      par_num[0] = line[j];
	      if (isdigit(line[j+1])){
		par_num[1] = line[j+1];
	      }
	    }
	    else {
	      line_num[0] = line[j];
	      if (isdigit(line[j+1])){
		line_num[1] = line[j+1];
	      }
	    }
	  }
	}
	paragraph_number = atoi(par_num);
	line_number = atoi(line_num);
	remove_line(&doc, paragraph_number, line_number);
	continue;
      }

      if (!is_command(line, "load_file")){
	int j = 0, k, c_index = 0, count = 0, length = strlen(line);
	int start_index = -1;
	char command[] = "load_file";
	char filename[80];
	int target_count = strlen(command);
	while (line[j]){
	  if (line[j] == command[c_index]){
	    count ++;
	    c_index++;
	    if (count == target_count){
	      start_index = j;
	      c_index = 0;
	      break;
	    }
	  }
	  j++;
	}
	
	for (k = start_index + 1; k < length; k++){
	  if (line[k] != ' '){
	    filename[c_index] = line[k];
	    c_index++;
	  }
	}
	filename[c_index - 1] = '\0';
	load_file(&doc, filename);
	continue;
      }

      if (!is_command(line, "replace_text")){
	int j, index = 0, length = strlen(line);
	int idx1 = -1, idx2 = -1, idx3 = -1, idx4 = -1;
	char string_tar[MAX_STR_SIZE];
	char string_rep[MAX_STR_SIZE];
	for (j = 0; j < length; j++){
	  if (line[j] == '"'){
	    if (idx1 == -1){
	      idx1 = j;
	      continue;
	    }
	    if (idx2 == -1){
	      idx2 = j;
	      continue;
	    }
	    if (idx3 == -1){
	      idx3 = j;
	      continue;
	    }
	    if (idx4 == -1){
	      idx4 = j;
	      break;
	    }
	  }
	}
	for (j = idx1 + 1; j < idx2; j++){
	  string_tar[index] = line[j];
	  index ++;
	}
	string_tar[index] = '\0';
	index = 0;
	for (j = idx3 + 1; j < idx4; j++){
	  string_rep[index] = line[j];
	  index ++;
	}
	string_rep[index] = '\0';
	replace_text(&doc, string_tar, string_rep);
	continue;
      }
      if (!is_command(line, "highlight_text")){
	int j, index = 0, length = strlen(line), idx1 = -1, idx2 = -1;
	char highlight[MAX_STR_SIZE];
	for (j = 0; j < length; j++){
	  if (line[j] == '"'){
	    if (idx1 == -1){
	      idx1 = j;
	      continue;
	    }
	    if (idx2 == -1){
	      idx2 = j;
	      break;
	    }
	  }
	}
	for (j = idx1 + 1; j < idx2; j++){
	  highlight[index] = line[j];
	  index++;
	}
	highlight[index] = '\0';
	highlight_text(&doc, highlight);
	continue;
      }
      if (!is_command(line, "remove_text")){
	int j, index = 0, length = strlen(line), idx1 = -1, idx2 = -1;
	char replace[MAX_STR_SIZE];
	for (j = 0; j < length; j++){
	  if (line[j] == '"'){
	    if (idx1 == -1){
	      idx1 = j;
	      continue;
	    }
	    if (idx2 == -1){
	      idx2 = j;
	      break;
	    }
	  }
	}
	for (j = idx1 + 1; j < idx2; j++){
	  replace[index] = line[j];
	  index++;
	}
	replace[index] = '\0';
	remove_text(&doc, replace);
	continue;
      }
      if (!is_command(line, "save_document")){
	int j = 0, k, c_index = 0, count = 0, length = strlen(line);
	int start_index = -1;
	char command[] = "save_document";
	char filename[80];
	int target_count = strlen(command);

	while (line[j]){
	  if (line[j] == command[c_index]){
	    count ++;
	    c_index++;
	    if (count == target_count){
	      start_index = j;
	      c_index = 0;
	      break;
	    }
	  }
	  j++;
	}
	
	for (k = start_index + 1; k < length; k++){
	  if (line[k] != ' '){
	    filename[c_index] = line[k];
	    c_index++;
	  }
	}
	filename[c_index - 1] = '\0';
	
	save_document(&doc, filename);
	continue;
      }
      if (!is_command(line, "reset_document")){
	reset_document(&doc);
	continue;
      }
    }
    
  }

  if (argc == 2){
    /*GET INPUT FROM READING A FILE*/
    
    /*CHECK TO SEE IF WE CAN OPEN FILE, IF WE CAN'T THEN WE EXIT*/
    input = fopen(argv[1], "r");
    if (input  == NULL){
      fprintf(stderr, "%s cannot be opened.", argv[1]);
      exit(EX_OSERR);
    }
    
    else{
      while (fgets(line, 1024, input) != NULL){

/*START OF WHILE LOOP HERE*/
      if (!is_comment(line)){
	continue;
      }     
      if (!is_command(line, "add_paragraph_after")){
	int paragraph_number = -1, j, length = strlen(line);
	char num[2] = "  ";
	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    num[0] = line[j];
	    if (isdigit(line[j + 1])){
	      num[1] = line[j + 1];
	      break;
	    }
	    break;
	  }
	}
	paragraph_number = atoi(num);
	add_paragraph_after(&doc, paragraph_number);
	continue;
      }
      if (!is_command(line, "add_line_after")){
	int  paragraph_number = -1, line_number = -1, j, length = strlen(line);
	int string_index = -1;
	char par_num[2] = "  ";
	char line_num[2] = "  ";
	char string[MAX_STR_SIZE + 1];
	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    if (par_num[0] == ' '){
	      par_num[0] = line[j];
	      if (isdigit(line[j + 1])){
		par_num[1] = line[j + 1];
	      }
	    }
	    else{
	      line_num[0] = line[j];
	      if (isdigit(line[j + 1])){
		line_num[1] = line[j + 1];
	      }
	    }
	  } 
	  if (line[j] == '*'){
	    string_index = 0;
	    continue;
	  }
	  if (string_index != -1){
	    string[string_index] = line[j];
	    string_index ++;
	  }
	}
	string[string_index - 1] = '\0';
	paragraph_number = atoi(par_num);
	line_number = atoi(line_num);
	add_line_after(&doc, paragraph_number, line_number, string);
	continue;
      }
      if (!is_command(line, "print_document")){
	int j, c_index = 0, end_index, length = strlen(line);
	char command[] = "print_document";
	int check = -1;
	/* ///////////////////////////////////////////////////////////*/


	while (line[j]){
	  if (line[j] == command[c_index]){
	    count ++;
	    c_index++;
	    if (count == target_count){
	      end_index = j;
	      c_index = 0;
	      
	      break;
	    }
	  }
	  j++;
	}
	
	for (j = end_index + 1; j < length + 1; j++){
	  if (line[j] != ' '){
	    check = 0;
	    fprintf(stdout, "Invalid Command");
	    break;
	  }
	}
	
	if (check == -1){
	
	print_document(&doc);
	}
	continue;
      }
      if (!is_command(line, "quit")){
	break;
      }
      if (!is_command(line, "exit")){
	break;
      }
      if (!is_command(line, "append_line")){
	int j, paragraph_number, length = strlen(line), string_index = -1;
	char par_num[2];
	char string_a[MAX_STR_SIZE + 1];

	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    par_num[0] = line[j];
	    if (isdigit(line[j + 1])){
	      par_num[1] = line[j + 1];
	    }
	  }
	  if (line[j]  == '*'){
	    string_index = 0;
	    continue;
	  }
	  if (string_index != -1){
	    string_a[string_index] = line[j];
	    string_index++;
	  }
	}
	string_a[string_index - 1] = '\0';
	paragraph_number = atoi(par_num);
	append_line(&doc, paragraph_number, string_a);
	continue;
      }
      if (!is_command(line, "remove_line")){
	int j, paragraph_number, line_number, length = strlen(line);
	char par_num[2] = "  ";
	char line_num[2] = "  ";

	for (j = 0; j < length; j++){
	  if (isdigit(line[j])){
	    if (par_num[0] == ' '){
	      par_num[0] = line[j];
	      if (isdigit(line[j+1])){
		par_num[1] = line[j+1];
	      }
	    }
	    else {
	      line_num[0] = line[j];
	      if (isdigit(line[j+1])){
		line_num[1] = line[j+1];
	      }
	    }
	  }
	}
	paragraph_number = atoi(par_num);
	line_number = atoi(line_num);
	remove_line(&doc, paragraph_number, line_number);
	continue;
      }

      if (!is_command(line, "load_file")){
	int j = 0, k, c_index = 0, count = 0, length = strlen(line);
	int start_index = -1;
	char command[] = "load_file";
	char filename[80];
	int target_count = strlen(command);
	while (line[j]){
	  if (line[j] == command[c_index]){
	    count ++;
	    c_index++;
	    if (count == target_count){
	      start_index = j;
	      c_index = 0;
	      break;
	    }
	  }
	  j++;
	}
	
	for (k = start_index + 1; k < length; k++){
	  if (line[k] != ' '){
	    filename[c_index] = line[k];
	    c_index++;
	  }
	}
	filename[c_index - 1] = '\0';
	load_file(&doc, filename);



	continue;

      }

      if (!is_command(line, "replace_text")){
	int j, index = 0, length = strlen(line);
	int idx1 = -1, idx2 = -1, idx3 = -1, idx4 = -1;
	char string_tar[MAX_STR_SIZE];
	char string_rep[MAX_STR_SIZE];
	for (j = 0; j < length; j++){
	  if (line[j] == '"'){
	    if (idx1 == -1){
	      idx1 = j;
	      continue;
	    }
	    if (idx2 == -1){
	      idx2 = j;
	      continue;
	    }
	    if (idx3 == -1){
	      idx3 = j;
	      continue;
	    }
	    if (idx4 == -1){
	      idx4 = j;
	      break;
	    }
	  }
	}
	for (j = idx1 + 1; j < idx2; j++){
	  string_tar[index] = line[j];
	  index ++;
	}
	string_tar[index] = '\0';
	index = 0;
	for (j = idx3 + 1; j < idx4; j++){
	  string_rep[index] = line[j];
	  index ++;
	}
	string_rep[index] = '\0';
	replace_text(&doc, string_tar, string_rep);
	continue;
      }
      if (!is_command(line, "highlight_text")){
	int j, index = 0, length = strlen(line), idx1 = -1, idx2 = -1;
	char highlight[MAX_STR_SIZE];
	for (j = 0; j < length; j++){
	  if (line[j] == '"'){
	    if (idx1 == -1){
	      idx1 = j;
	      continue;
	    }
	    if (idx2 == -1){
	      idx2 = j;
	      break;
	    }
	  }
	}
	for (j = idx1 + 1; j < idx2; j++){
	  highlight[index] = line[j];
	  index++;
	}
	highlight[index] = '\0';
	highlight_text(&doc, highlight);
	continue;
      }
      if (!is_command(line, "remove_text")){
	int j, index = 0, length = strlen(line), idx1 = -1, idx2 = -1;
	char replace[MAX_STR_SIZE];
	for (j = 0; j < length; j++){
	  if (line[j] == '"'){
	    if (idx1 == -1){
	      idx1 = j;
	      continue;
	    }
	    if (idx2 == -1){
	      idx2 = j;
	      break;
	    }
	  }
	}
	for (j = idx1 + 1; j < idx2; j++){
	  replace[index] = line[j];
	  index++;
	}
	replace[index] = '\0';
	remove_text(&doc, replace);
	continue;
      }
      if (!is_command(line, "save_document")){
	int j = 0, k, c_index = 0, count = 0, length = strlen(line);
	int start_index = -1;
	char command[] = "save_document";
	char filename[80];
	int target_count = strlen(command);

	while (line[j]){
	  if (line[j] == command[c_index]){
	    count ++;
	    c_index++;
	    if (count == target_count){
	      start_index = j;
	      c_index = 0;
	      break;
	    }
	  }
	  j++;
	}
	
	for (k = start_index + 1; k < length; k++){
	  if (line[k] != ' '){
	    filename[c_index] = line[k];
	    c_index++;
	  }
	}
	filename[c_index - 1] = '\0';
	
	save_document(&doc, filename);
	continue;
      }
      if (!is_command(line, "reset_document")){
	reset_document(&doc);
	continue;
      }

/*end of while loop here*/}
    }

    
  }

  /*INITIALIZE DOCUMENT CALLED main_document*/
  output = fopen("main_document", "w");
  paragraphs = doc.number_of_paragraphs;
  
  fputs("Document name: \"main_document\"\n", output);
  fputs("Number of Paragraphs: ", output);
  sprintf(paragraph_string, "%d", paragraphs); 
  fputs(paragraph_string, output);
  fputs("\n", output);
  for (h = 0; h < paragraphs; h++){
    for (i = 1; i < doc.paragraphs[h + 1].number_of_lines + 1; i++){
      fputs(doc.paragraphs[h + 1].lines[i], output);
      fputs("\n", output);
    }
  }
  return 0;
}

int is_comment(char *str){
  int i = 0;
  while (str[i]){
    if (str[i] != ' '){ 
      if (str[i] == '#'){
	return SUCCESS;
      }
      else{
	return FAILURE;
      }
    }
    i++;
  }
  return FAILURE;
}

int is_command(char *str, const char *command){
  int i = 0;
  int c_index = 0;
  int count = 0;
  int target_count = strlen(command);
  
  while (str[i]){
    if (str[i] != command[c_index]) {
      if (count != 0){
	count = 0;
	c_index = 0;
      }
    }
    if (str[i] != ' '){
      if (str[i] == command[c_index]){
	count ++;
	c_index ++;
	if (count == target_count){
	  return SUCCESS;
	}
      }
      else{
	return FAILURE;
      }
    }
    i++;
  }
  return FAILURE;
}

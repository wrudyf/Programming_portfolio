/*
Name: Rudy Fuentes
University ID number: 114096296
UMD Directory ID: rfuente5
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "document.h"

int init_document(Document *doc, const char *name){
  int name_length = strlen(name);
  int i = 0;
  if (doc == NULL || name == NULL || name_length < 1 || 
      name_length > MAX_STR_SIZE){
    return FAILURE;
  }

  doc->number_of_paragraphs = 0;
  while (name[i]){
    doc->name[i] = name[i];
    i++;
  }

  doc->name[i] = '\0';

  return SUCCESS;
}

int reset_document(Document *doc){
  if (doc == NULL){
    return FAILURE;
  }
  doc->number_of_paragraphs = 0;
  return SUCCESS;
}

int print_document(Document *doc){
  int i, j;
  int paragraphs;

  if (doc == NULL){
    return FAILURE;
  }
  paragraphs = doc->number_of_paragraphs;
  printf("Document name: \"%s\"\n", doc->name);
  printf("Number of Paragraphs: %d\n", paragraphs);
  
  for (i = 1; i < paragraphs + 1; i++){
    
    for (j = 1; j < doc->paragraphs[i].number_of_lines + 1; j++){
      printf("%s\n", doc->paragraphs[i].lines[j]);
      
      if (j == doc->paragraphs[i].number_of_lines ){
	if (i != paragraphs){
	printf("\n");
	}
      }
    }
    /*
    if (i != paragraphs ){
    printf("\n");
    }
    */
  }
  
  return SUCCESS;
}

int add_paragraph_after(Document *doc, int paragraph_number){
  int paragraphs = doc->number_of_paragraphs;

  if (doc == NULL || paragraphs == MAX_PARAGRAPHS || paragraph_number < 0 ||
      paragraph_number > MAX_PARAGRAPHS){
    return FAILURE;
  }

  doc->number_of_paragraphs += 1;
  doc->paragraphs[paragraph_number + 1].number_of_lines = 0;

  return SUCCESS;
}

int add_line_after(Document *doc, int paragraph_number, int line_number, 
		   const char *new_line){
  int paragraphs, paragraph_lines;
  if (doc == NULL || new_line == NULL){
    return FAILURE;
  }

  paragraphs = doc->number_of_paragraphs;

  if (paragraph_number > paragraphs || paragraph_number < 0){
    return FAILURE;
  }

  paragraph_lines = doc->paragraphs[paragraph_number].number_of_lines;

  if (line_number > MAX_PARAGRAPH_LINES || line_number < 0 || 
      paragraph_lines == MAX_PARAGRAPH_LINES){
    return FAILURE;
  }

  strcpy(doc->paragraphs[paragraph_number].lines[line_number + 1], new_line);
  doc->paragraphs[paragraph_number].number_of_lines += 1;

  return SUCCESS;
}

int get_number_lines_paragraph(Document *doc, int paragraph_number, 
			       int *number_of_lines){
  if (doc == NULL || number_of_lines == NULL || paragraph_number < 0 ||
     paragraph_number > doc->number_of_paragraphs ){
    return FAILURE;
  }

  *number_of_lines = doc->paragraphs[paragraph_number].number_of_lines;

  return SUCCESS;
}

int append_line(Document *doc, int paragraph_number, const char *new_line){
  int paragraphs, paragraph_lines, end_line;
  if (doc == NULL || new_line == NULL){
    return FAILURE;
  }

  paragraphs = doc->number_of_paragraphs;

  if (paragraph_number > paragraphs || paragraph_number < 0 || 
      paragraph_number > MAX_PARAGRAPHS){
    return FAILURE;
  }

  paragraph_lines = doc->paragraphs[paragraph_number].number_of_lines;

  if (paragraph_lines == MAX_PARAGRAPH_LINES){
    return FAILURE;
  }
  end_line = doc->paragraphs[paragraph_number].number_of_lines + 1;
  strcpy(doc->paragraphs[paragraph_number].lines[end_line], new_line);
  doc->paragraphs[paragraph_number].number_of_lines += 1;

  return SUCCESS;
}

int remove_line(Document *doc, int paragraph_number, int line_number){
  int i, lines;
  if (doc == NULL ||  paragraph_number > MAX_PARAGRAPHS || paragraph_number < 0
      || paragraph_number > doc->number_of_paragraphs){
    return FAILURE;
  }

  if (line_number > doc->paragraphs[paragraph_number].number_of_lines ||
      line_number < 0){
    return FAILURE;
  }

  lines = doc->paragraphs[paragraph_number].number_of_lines;

  for (i = line_number; i < lines; i++){
    strcpy(doc->paragraphs[paragraph_number].lines[i], 
	   doc->paragraphs[paragraph_number].lines[i+1]);
  }

  
  doc->paragraphs[paragraph_number].number_of_lines -= 1;
  return SUCCESS;
}

int load_document(Document *doc, char data[][MAX_STR_SIZE + 1], 
		  int data_lines){
  int i; 
  int max_paragraph_count = 0; 
  int max_line_count = 0;
  if (doc == NULL || data == NULL || data_lines <= 0){
    return FAILURE;
  }
  /*check to see if we get over max paragraphs or over max lines
    in any paragraph */
  for (i = 0; i < data_lines; i++){
    if (i == 0){
      if (strcmp(data[i], "")){
	/*if first empty is not empty string new paragraph*/
	max_paragraph_count += 1;
	max_line_count ++;
	continue;
      }
      if (!strcmp(data[i], "")){
	/*if first is empty string new paragraph*/
	max_paragraph_count += 1;
	continue;
      }
    }
    if (!strcmp(data[i], "")){
      /*if line is empty string new paragraph*/
      
	max_paragraph_count += 1;
	max_line_count = 0;
	if (max_paragraph_count > MAX_PARAGRAPHS){
	  return FAILURE;
	}
	continue;
    }
    max_line_count ++;
    if (max_line_count > MAX_PARAGRAPH_LINES){
      return FAILURE;
    }
  }
  max_paragraph_count = 0;
  max_line_count = 0;
  /*if we didn't return failure, then we are good to start adding 
    all the data to the doc*/
  for (i = 0; i < data_lines; i++){
    if (i == 0){
      if (strcmp(data[i], "")){
	/*if first is not empty string, then new paragraph*/
	max_paragraph_count += 1;
	max_line_count ++;
	doc->number_of_paragraphs += 1;
	doc->paragraphs[max_paragraph_count].number_of_lines += 1;
	strcpy(doc->paragraphs[max_paragraph_count].lines[max_line_count],
	       data[i]);

	continue;
      }
    }
    if (!strcmp(data[i], "")){
      /*if first is empty string, then new paragraph*/
      max_paragraph_count += 1;
      doc->number_of_paragraphs += 1;
      doc->paragraphs[max_paragraph_count].number_of_lines = 0;
      continue;
    }
    max_line_count ++;
    doc->paragraphs[max_paragraph_count].number_of_lines += 1;
    strcpy(doc->paragraphs[max_paragraph_count].lines[max_line_count],
	   data[i]);

  }
  return SUCCESS;
}

void check_and_do_replacement(char *string, const char *target,
			      const char* replacement){
  int i,  k, j, h;
  int target_index = 0;
  int original_size = strlen(target), target_size = strlen(replacement);
  int match_count = 0;
  for (i = 0; i < MAX_STR_SIZE; i++){
    /*if match_count is set, but we do not match then we reset conditions*/ 
    if (string[i] != target[target_index]){
	if (match_count != 0){
	  match_count = 0;
	  target_index = 0;
	}
    }
    /*if i matches first target index we increase match count and index */
    if (string[i] == target[target_index]){
      match_count += 1;
      target_index += 1;
      
      /*if size of what we want to replace  and match count is equal 
	we found target and reset conditions and do replacement, and 
      set i equal to 0 again so we can start all over again until we 
      make all necessary replacements*/
     
      if (match_count == original_size){
	int start = i;
	int difference;
	match_count = 0;
	target_index = 0;
	i = i - original_size + 1;
	difference = (int)abs(original_size - target_size);

	if (original_size > target_size){
	  start = i + target_size;
	  for (k = 0; k < target_size; k++){
	    string[i + k] = replacement[k];
	  }
	  
	  for (j = 0; j < difference; j++){
	    for (h = start; h < MAX_STR_SIZE; h++){
	      string[h] = string[h + 1];
	    }
	  }
	  
	}
	/**/
	else if (original_size < target_size){
	  start = i + target_size - 1;
	  /*make space first*/
	  for (j = 0; j < difference; j++){
	    for (h = MAX_STR_SIZE; h > start; h--){
	      string[h] = string[h -1];
	    }
	  }

	  /*then replace*/
	  for (k = 0; k < target_size; k++){
	    string[i + k] = replacement[k];
	  }

	}

	else{
	  for (k = 0; k < target_size; k++){
	    string[i + k] = replacement[k];
	  }
	}
	i = 0;

      }
    }
  }
  
}


int replace_text(Document *doc, const char *target, const char *replacement){
  int i, j, paragraphs, lines;
  if (doc == NULL || target == NULL || replacement == NULL){
    return FAILURE;
  }
  paragraphs = doc->number_of_paragraphs + 1;
  for (i = 1; i < paragraphs; i++){
    lines = doc->paragraphs[i].number_of_lines + 1;
    for (j = 1; j < lines; j++){
      check_and_do_replacement(doc->paragraphs[i].lines[j], target,
			       replacement);
    }
  }
  return SUCCESS;
}

void check_and_highlight(char *string, const char *target){
  int i, k = 0, length = strlen(string), new_length;
  int match_count = 0, target_size = strlen(target), target_index = 0;
  int counter = 0, counter_index = 0, end_counter_index = 0;
  char start[1] = HIGHLIGHT_START_STR;
  char end[1] = HIGHLIGHT_END_STR;
  int indices[MAX_STR_SIZE];
  int end_indices[MAX_STR_SIZE];
  char highlighted_text[MAX_STR_SIZE + 1];
  for (i = 0; i < length; i++){
    /*if match_count is set, but we do not match then we reset conditions*/ 
    if (string[i] != target[target_index]){
	if (match_count != 0){
	  match_count = 0;
	  target_index = 0;
	}
    }    
    /*if i matches first target index we increase match count and index */
    if (string[i] == target[target_index]){
      match_count += 1;
      target_index += 1;
      /*if size of what we want to replace  and match count is equal 
	we found target and reset conditions*/
      if (match_count == target_size){
	match_count = 0;
	target_index = 0;
	indices[counter] = i - target_size + 1;
	end_indices[counter] = i + 1;
	counter ++;
      }
    }
  }
  new_length = length + (counter * 2);
  /*replacement string with highlights*/

  for (i = 0; i < new_length; i++){
    
    if (k == indices[counter_index]){
      highlighted_text[i] = start[0];
      i++;
      counter_index++;
    }
    if (k == end_indices[end_counter_index]){
      
      highlighted_text[i] = end[0];
      i++;
      
      end_counter_index++;
    }
    
    highlighted_text[i] = string[k];
    k++;
  }

  if (counter != 0){
  
  for (i = 0; i < new_length; i++){
    string[i] = highlighted_text[i];
  }
  }
}
/*
NOTE: I THINK I GOT HIGHLIGHT TEXT WRONG BECAUSE I JUST DID INDEX 0 OF 
START AND END WHEN I SHOULD HAVE DONE LOOP IF IN CASE START AND END WERE
MORE THAN 1 CHARACTER LONG, AND EACH WERE DIFFERENT, SO CHECK THAT OVER
 */
int highlight_text(Document *doc, const char *target){
  int i, j, paragraphs, lines;
  if (doc == NULL || target == NULL){
    return FAILURE;
  }
  paragraphs = doc->number_of_paragraphs + 1;
  for (i = 1; i < paragraphs; i++){
    lines = doc->paragraphs[i].number_of_lines + 1;
    for (j = 1; j < lines; j++){
      check_and_highlight(doc->paragraphs[i].lines[j], target);
    }
  }
  return SUCCESS;
}

void do_removal(char *string, const char *target){
  int i, j, k, length = strlen(string);
  int match_count = 0, target_size = strlen(target), target_index = 0;

  for (i = 0; i < length; i++){
    /*if match_count is set, but we do not match then we reset conditions*/ 
    if (string[i] != target[target_index]){
	if (match_count != 0){
	  match_count = 0;
	  target_index = 0;
	}
    }    
    /*if i matches first target index we increase match count and index */
    if (string[i] == target[target_index]){
      match_count += 1;
      target_index += 1;
      /*if size of what we want to replace  and match count is equal 
	we found target and reset conditions*/
      if (match_count == target_size){
	match_count = 0;
	target_index = 0;
	i = i - target_size + 1;
	for (j = 0; j < target_size; j++){
	  for (k = i; k < length; k++){
	    string[k] = string[k+1];
	  }
	}
	i = 0;
      }
    }
  }

}

int remove_text(Document *doc, const char *target){
  int i, j, paragraphs, lines;
  if (doc == NULL || target == NULL){
    return FAILURE;
  }
  paragraphs = doc->number_of_paragraphs + 1;
  for (i = 1; i < paragraphs; i++){
    lines = doc->paragraphs[i].number_of_lines + 1;
    for (j = 1; j < lines; j++){
      do_removal(doc->paragraphs[i].lines[j], target);
    }
  }
  return SUCCESS;
}

int load_file(Document *doc, const char *filename){
  FILE *input;
  int paragraph_count = 1, line_count = 0,  h, i, empty;
  int existing_paragraphs = doc->number_of_paragraphs;
  int line_length;
  char line[100];
  char formatted[100];

  if (doc == NULL || filename == NULL){
    return FAILURE;
  }
  input = fopen(filename, "r");
  if (input == NULL){
    return FAILURE;
  }
  
  /*if doc already has data, shift it down necessary amount*/
  if (existing_paragraphs != 0){
    Paragraph append_paragraphs[15];
    int paragraphs_size;
    int j;
    int append_index = 0;
    char tmp[200];
    /*copy doc's paragraphs to our temporary paragraphs*/
    for (i = 0; i < existing_paragraphs + 1; i++){
      append_paragraphs[i] = doc->paragraphs[i];
    }
    
    /*check to see how many paragraphs we need shifted*/
    doc->number_of_paragraphs = 0;
    doc->number_of_paragraphs += 1;
    doc->paragraphs[paragraph_count].number_of_lines = 0;
    
  
    while (fgets(line, 1024, input)){
      empty = 0;
      h = 0;
      strcpy(formatted, line);
      while (formatted[h]){
	if (formatted[h] != ' '){
	  if (formatted[h] == '\n'){
	    empty = 1;
	    break;
	  }
	  else{
	    break;
	  }
	} 
	h++;
      }
      if (empty == 1){
	paragraph_count ++;
	line_count = 0;
	doc->number_of_paragraphs += 1;
	doc->paragraphs[paragraph_count].number_of_lines = 0;
	continue;
      }

      line_length = strlen(formatted);
      for (i = 0; i < line_length; i++){
	if (formatted[i] == '\n'){
	  formatted[i] = '\0';
	}
      }
      line_count ++;
      doc->paragraphs[paragraph_count].number_of_lines += 1;
      strcpy(doc->paragraphs[paragraph_count].lines[line_count], formatted);
    }
    paragraphs_size = paragraph_count + existing_paragraphs;
    for (i = paragraph_count; i < paragraphs_size + 1; i++){
      doc->paragraphs[i] = append_paragraphs[append_index];
      append_index ++;
      doc->number_of_paragraphs += 1;
    }
    
    
    return SUCCESS;
  }

  /*copy data from file to document NORMALLY IF DOC IS EMPTY*/
  doc->number_of_paragraphs = 0;
  doc->number_of_paragraphs += 1;
  doc->paragraphs[paragraph_count].number_of_lines = 0;
  
  while (fgets(line, 1024, input) != NULL){
    empty = 0;
    h = 0;
    strcpy(formatted, line);
    while (formatted[h]){
      if (formatted[h] != ' '){
	if (formatted[h] == '\n'){
	  empty = 1;
	  break;
	}
	else{
	  break;
	}
      } 
      h++;
    }
    if (empty == 1){
      paragraph_count ++;
      line_count = 0;
      doc->number_of_paragraphs += 1;
      doc->paragraphs[paragraph_count].number_of_lines = 0;
      continue;
    }

    line_length = strlen(formatted);
    for (i = 0; i < line_length; i++){
      if (formatted[i] == '\n'){
	formatted[i] = '\0';
      }
    }
    line_count ++;
    doc->paragraphs[paragraph_count].number_of_lines += 1;
    strcpy(doc->paragraphs[paragraph_count].lines[line_count], formatted);
    
  }
  
  return SUCCESS;
}


int save_document(Document *doc, const char *filename){
  FILE *output;
  int paragraphs, h, i;
  char paragraph_string[2];
  if (doc == NULL || filename == NULL){
    return FAILURE;
  }
  output = fopen(filename, "w");
  if (output  == NULL){
    return FAILURE;
  }
  paragraphs= doc->number_of_paragraphs;
  fputs("Document name: ", output);
  fputs(doc->name, output);
  fputs("\n", output);
  fputs("Number of Paragraphs: ", output);
  sprintf(paragraph_string, "%d", paragraphs);
  fputs(paragraph_string, output);
  fputs("\n", output);

  for (h = 0; h < paragraphs; h++){
    for (i = 1; i < doc->paragraphs[h + 1].number_of_lines + 1; i++){
      fputs(doc->paragraphs[h + 1].lines[i], output);
      fputs("\n", output);
    }
  }
  
  return SUCCESS;  
}

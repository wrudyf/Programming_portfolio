#include <stdio.h>
#include <string.h>
#include "text_manipulation.h"


int remove_spaces(const char *source, char *result, int *num_spaces_removed) { 
  
  int length = strlen(source);
  int i = 0, spaces_removed = 0;
  int lead_start = -1, lead_end = -1, lead_end_met = 0;
  int temp_lead_start = -1, temp_lead_end = -1;
  int trail_start = -1, trail_end = -1;
  int lead_spaces = 0;
  int trail_spaces = 0;
  /*do initial check to determine if input is valid*/
  if (source == NULL || length == 0){
    return FAILURE;
  }
  
  /*loop to check for leading and trailing spaces*/

  while (source[i]) {
    /*start loop at point 0 and check to see if start has space*/
    if (source[i] == ' '){
      
      /*get lead spaces*/
      if (lead_start == -1){
	lead_start = i;
	lead_end = i;
      }

      if (source[i+1] != ' '){

	if (lead_end_met == 0){
	  lead_end = i;
	  lead_end_met = 1;
	}
	if (lead_start != 0){
	  lead_end_met = 0;
	  lead_start = -1;
	}
      } 
      
      
      /*get trail spaces*/
      if (lead_end_met != 0 && lead_start != -1){
	if (trail_start == -1){
	  trail_start = i;
	  trail_end = i;
	}
	
	if (source[i+1] != ' '){
	  if (source[i+1] == '\0'){	
	    trail_end = i;
	  }
	  else{
	    trail_start = -1;
	  }
	}
      } 
    }
    i++;
  }

  if (trail_start == -1){
    if (source[0] == ' '){
      spaces_removed = lead_end - lead_start + 1;
      i = spaces_removed;
      while (i < length){
	result[i - spaces_removed] = source[i];
	i++;	
      }
    }
    else{
      i = 0;
      while (i < length){
	if (source[i] == ' '){
	  if(temp_lead_start == -1){
	    temp_lead_start = i;
	    temp_lead_end = i;
	  }
	  if (source[i+1] != ' '){
	    if (source[i+1] == '\0'){
	      temp_lead_end = i;
	    }
	    else{
	      temp_lead_start = -1;
	    }
	  }
	  
	}
	i++;
      }
      
      spaces_removed = temp_lead_end - temp_lead_start +1;
      i = 0;
      while (i < temp_lead_start){
	result[i] = source[i];
	i++;
      }
    }

  }
  
  else{

  lead_spaces = lead_end - lead_start + 1;
  trail_spaces = trail_end - trail_start + 1;
  
  spaces_removed = lead_spaces + trail_spaces;
  
  i = lead_end + 1;
  
  while (i < trail_start){
    result[i - lead_spaces] = source[i];
    i++;
    if (i == trail_start) {
      result[i - lead_spaces] = '\0';
    }
  }

  }  

  /*check to see if num spaces removed is different than null to set
  the number of spaces removed */

  if (num_spaces_removed != NULL){
    *num_spaces_removed = spaces_removed;
  }

  
  return SUCCESS; 
}

int center(const char *source, int width, char *result) {       
  int length = strlen(source);
  int space =  (width - length) / 2;
  int lead_space_end = 0 + space;
  
  int trail_space_start = length + space;

  int new_length = length + (space * 2); 
  int i = 0;
  
  /*do initial check to see if input is valid*/
  if (source == NULL || length == 0 || width < length){
    return FAILURE;
  }

  /*loop to center string*/
  while (i < new_length){
    if (i < lead_space_end){
      result[i] = ' ';
    }
    
    else if (i > trail_space_start - 1){
      result[i] = ' ';
    }

    else{
      result[i] = source[i - space];
    }
    i++;
  }
  result[i] = '\0';

  return SUCCESS;
}

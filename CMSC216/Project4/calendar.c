/*
Name: Rudy F.
univerity ID number: 114096296
UMD Directory ID: rfuente5
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calendar.h"

int init_calendar(const char *name, int days, 
		  int (*comp_func) (const void *ptr1, const void *ptr2), 
		  void (*free_info_func)(void *ptr), Calendar ** calendar){  
  int length;
  if (calendar == NULL || name == NULL || days < 1){
    return FAILURE;
  }

  /*allocate memory for calendar struct, and assigning our struct pointer 
    to point to the dynamically allocated memory for the calendar struct
   */
  (*calendar) = malloc(sizeof(Calendar));
  if ((*calendar) == NULL){
    return FAILURE;
  }
  
  /*allocate memory for calendar's name field*/
  length = strlen(name);
  (*calendar)->name = malloc(length + 1);
  if ((*calendar)->name == NULL){
    return FAILURE;
  }
  /*copy name to calendar's name field*/
  strcpy((*calendar)->name, name);

  /*allocate memory for calendar's events field, use calloc to make 
   all the stuff in the memory 0 for the start*/
  (*calendar)->events = calloc(days, sizeof(Event *));
  if ((*calendar)->events == NULL){
    return FAILURE;
  }

  /*assign the other variables to our calendar*/
  (*calendar)->days = days;
  (*calendar)->total_events = 0;
  (*calendar)->comp_func = comp_func;
  (*calendar)->free_info_func = free_info_func;

  return SUCCESS;
}

int print_calendar(Calendar *calendar, FILE *output_stream, int print_all){
  int i = 0;
  char days[3];
  char total_events[3];
  char day_num[3];
  char start_time[4];
  char duration[4];
  Event *print;
  if (calendar == NULL || output_stream == NULL){
    return FAILURE;
  }

  if (print_all != 0){
  fputs("Calendar's Name: \"", output_stream);
  fputs(calendar->name, output_stream);
  fputs("\"\n", output_stream);
  fputs("Days: ", output_stream);
  sprintf(days, "%d", calendar->days);
  fputs(days, output_stream);
  fputs("\n", output_stream);
  fputs("Total Events: ", output_stream);
  sprintf(total_events, "%d", calendar->total_events);
  fputs(total_events, output_stream);
  fputs("\n\n", output_stream);
  }
  fputs("**** Events ****\n", output_stream);
  if (calendar->total_events != 0){
    /*print individual events here*/
    while (i < calendar->days){
      fputs("Day ", output_stream);
      sprintf(day_num, "%d", i + 1);
      fputs(day_num, output_stream);
      fputs("\n", output_stream);
      print = calendar->events[i];
      while (print != NULL){
	fputs("Event's Name: \"", output_stream);
	fputs(print->name, output_stream);
	fputs("\", Start_time: ", output_stream);
	sprintf(start_time, "%d", print->start_time);
	fputs(start_time, output_stream);
	fputs(", Duration: ", output_stream);	
	sprintf(duration, "%d", print->duration_minutes);
	fputs(duration, output_stream);
	fputs("\n", output_stream);	
	print = print->next;
      }  
      i++;
    }
  }
  return SUCCESS;
}



int add_event(Calendar *calendar, const char *name, int start_time, 
	      int duration_minutes, void *info, int day){
  Event *head, *prev = NULL, *new_event, *index;
  int length, found = 0, i = 0;
  if (calendar == NULL || name == NULL || start_time < 0 || start_time > 2400
      || duration_minutes <= 0 || day < 1){
    return FAILURE;
  }
  if (day > calendar->days || calendar->comp_func == NULL){
    return FAILURE;
  }

  /*check to see if event is already in calendar*/
  while (i < calendar->days){
    index = calendar->events[i];
    while (index != NULL){
      if (strcmp(index->name, name) == 0){
	found = -1;
	break;
      }
      index = index->next;
    }
    if (found == -1){
      break;
    }
    i++;
  }
  if (found == -1){
    return FAILURE;
  }

  /*if we didn't return failure then we proceed to add*/
  head = calendar->events[day - 1];
  new_event = malloc(sizeof(Event));
  if (new_event == NULL){
    return FAILURE;
  }
  
  length = strlen(name);
  new_event->name = malloc(length + 1);
  if (new_event->name == NULL){
    return FAILURE;
  }
  strcpy(new_event->name, name);
  new_event->start_time = start_time;
  new_event->duration_minutes = duration_minutes;
  new_event->info = info;
  
  while (head != NULL && calendar->comp_func(head, new_event) <= 0){
    if (calendar->comp_func(head, new_event) == 0){
      break;
    }
    prev = head;
    head = head->next;
  }
  new_event->next = head;

  if (prev == NULL){
    calendar->events[day - 1] = new_event;
  }

  else{
    prev->next = new_event;
  }

  calendar->total_events += 1;
  return SUCCESS;
}

int find_event(Calendar *calendar, const char *name, Event **event){
  int i = 0, found = 0;
  Event *current;
  if (calendar == NULL || name == NULL){
    return FAILURE;
  }
  while (i < calendar->days){
      current = calendar->events[i];
      while (current != NULL){
	if (strcmp(current->name, name) == 0){
	  if (event != NULL){
	  *event = current;
	  }
	  return SUCCESS;
	}
	current = current->next;
      }
    i++;
  }
  if (found == 0){
    return FAILURE;
  }
  return SUCCESS;
}

int find_event_in_day(Calendar *calendar, const char *name, int day,
		      Event **event){
  int found = 0;
  Event *current;
  if (calendar == NULL || name == NULL || day < 1 || day > calendar->days){
    return FAILURE;
  }

  current = calendar->events[day - 1];
  while (current != NULL){
    if (strcmp(current->name, name) == 0){
      if (event != NULL){
      *event = current;
      }
      return SUCCESS;
    }
    current = current->next;
  }
          
  if (found == 0){
    return FAILURE;
  }
  return SUCCESS;
}


int remove_event(Calendar *calendar, const char *name){
  int i = 0, found = 0;
  Event *current, *previous = NULL, *head;
  if (calendar == NULL || name == NULL){
    return FAILURE;
  }
  while (i < calendar->days){
      head = calendar->events[i];
      current = head;
      while (current != NULL){	
	if (strcmp(current->name, name) == 0){
	  found = -1;
	}
	if (found == -1){
	  break;
	}
	
	previous = current;
	current = current->next;
      }
      
      if (found == -1){
	break;
      }  
      i++;
  }
  if (found == 0){
    return FAILURE;
  }

  if (previous == NULL){
    head = current->next;
    calendar->events[i] = head;    
    free(current->name);
    free(current);
    calendar->total_events -= 1;
  }
  else{
    previous->next = current->next;
    free(current->name);
    free(current);
    calendar->total_events -= 1;
  }
  return SUCCESS;
}

void *get_event_info(Calendar *calendar, const char *name){
  int i = 0, found = 0;
  Event *current;
  while (i < calendar->days){
    current = calendar->events[i];
    while (current != NULL){
      if (strcmp(current->name, name) == 0){
	return current->info;
      }
      current = current->next;
    }
    i++;
  }
  return NULL;
}

int clear_calendar(Calendar *calendar){
  int i = 0;
  Event *current, *previous = NULL;
  if (calendar == NULL){
    return FAILURE;
  }
  while (i < calendar->days){
    current = calendar->events[i];
    while (current != NULL){
      previous = current;
      current = current->next;
      free(previous->name);
      free(previous);
    }
    calendar->events[i] = NULL;
    i++;
  }
  calendar->total_events = 0;
  return SUCCESS;
}

int clear_day (Calendar *calendar, int day){
  Event *current, *previous = NULL;
  if (calendar == NULL){
    return FAILURE;
  }
  if (day < 1 || day > calendar->days){
    return FAILURE;
  }  
  current = calendar->events[day - 1];
  while (current != NULL){
    previous = current;
    current = current->next;
    free(previous->name);
    free(previous);
    calendar->total_events -= 1;
  }
  calendar->events[day - 1] = NULL; 
  return SUCCESS;
}

int destroy_calendar(Calendar *calendar){
  int i = 0;
  Event *current, *previous = NULL;
  if (calendar == NULL){
    return FAILURE;
  }
  if (calendar->total_events == 0){ 
    free(calendar->events);  
    free(calendar->name);
    free(calendar);
  }
  else{
    /*free events linked lists*/
    /*while here is outer loop to iterate through each day*/
    while (i < calendar->days){
      current = calendar->events[i];
      /*while here is inner loop to iterate through each event in linked
	list and free their memory. we free from start to end. 
      */
      while (current != NULL){
	previous = current;
	current = current->next;
	free(previous->name);
	free(previous);	
      }   
      i++;
    }
    free(calendar->events);
    free(calendar->name);
    free(calendar);
  }
  return SUCCESS;
}

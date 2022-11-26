/*
Name: Rudy F.
university ID number: 114096296
UMD Directory ID: rfuente5
*/
#include <stdio.h>
#include <stdlib.h>
#include "event.h"
#include "calendar.h"
#include "my_memory_checker_216.h"

/*****************************************************/
/* In this file you will provide tests for your      */
/* calendar application.  Each test should be named  */
/* test1(), test2(), etc. Each test must have a      */
/* brief description of what it is testing (this     */
/* description is important).                        */
/*                                                   */
/* You can tell whether any test failed if after     */
/* executing the students tests, you executed        */
/* "echo $?" on the command line and you get a value */
/* other than 0.  "echo $?" prints the status of     */
/* the last command executed by the shell.           */ 
/*                                                   */
/* Notice that main just calls test1(), test2(), etc.*/
/* and once one fails, the program eventually        */
/* return EXIT_FAILURE; otherwise EXIT_SUCCESS will  */
/* be returned.                                      */
/*****************************************************/

static int comp_minutes(const void *ptr1, const void *ptr2) {
   return ((Event *)ptr1)->duration_minutes - ((Event *)ptr2)->duration_minutes;
}

/* Description here: This test checks all the conditions for init_calendar
 */
static int test1() {
   Calendar *calendar;

   printf("TEST 1 starts here ------------>\n");
   printf("testing init calendar with NULL for name\n");
   
   if (init_calendar(NULL, 4, comp_minutes, NULL, &calendar) == FAILURE){
     printf("initialization failed with NULL, 4, comp_minutes, NULL, &cal\n");
   }
   
   if (init_calendar("yellow", 4, comp_minutes, NULL, NULL) == FAILURE){
     printf("initialization failed with yellow, 4, comp_minutes, NULL,NULL\n");
   }
   
   if (init_calendar("yellow", 0, comp_minutes, NULL, &calendar) == FAILURE){
     printf("initialization failed with yellow, 0, comp_minutes, NULL,&cal\n");
   }

   if (init_calendar("yellow", 2, comp_minutes, NULL, &calendar) == SUCCESS){
     printf("calendar successfully initialized\n");
     print_calendar(calendar, stdout, 1);
     destroy_calendar(calendar);
   }
   
   printf("\n");
   return FAILURE;
}
/*This test here is meant to test print calendar with different 
  parameters
 */
static int test2() {
  Calendar *calendar;
  printf("TEST 2 starts here ------------>\n");
  init_calendar("yellow", 2, comp_minutes, NULL, &calendar);
  printf("printing calendar with print all being true(anything besides 0)\n");
  print_calendar(calendar, stdout, 1);
  printf("printing calendar with print all being false (0)\n");
  print_calendar(calendar, stdout, 0);
  destroy_calendar(calendar);
  

  printf("\n");
   return SUCCESS;
}
/*This test here is meant to test add_events with different circumstances
 */

static int test3() {
  Calendar *calendar;
  printf("TEST 3 STARTS HERE ------------>\n");
  init_calendar("yellow", 4, comp_minutes, NULL, &calendar);
  add_event(calendar, "wake up, brush teeth and shower", 730, 45, NULL, 1);
  add_event(calendar, "wake up, brush teeth and shower", 730, 20, NULL, 1);
  add_event(calendar, "eat breakfast", 730, 45, NULL, 1);
  print_calendar(calendar, stdout, 1);
  destroy_calendar(calendar);
  printf("\n");
  return SUCCESS;
}

/*This test here is meant to test find_event, find_event in day, and remove 
  event 
 */

static int test4() {
  Calendar *calendar;
  printf("TEST 4 STARTS HERE ------------>\n");
  init_calendar("yellow", 4, comp_minutes, NULL, &calendar);
  add_event(calendar, "wake up, brush teeth and shower", 730, 10, NULL, 1);
  add_event(calendar, "eat breakfast", 730, 15, NULL, 1);
  add_event(calendar, "go to lecture or discussion",900, 30, NULL, 1);
  add_event(calendar, "do stuff", 500, 45, NULL, 3);
  print_calendar(calendar, stdout, 1);
  printf("can we find event eat breakfast in day 2?\n");
  if (find_event_in_day(calendar, "eat breakfast", 2, NULL) == SUCCESS){
    printf("yes\n");
  }
  else{
    printf("no\n");
  }
  printf("can we find event eat breakfast in the whole calendar?\n");
  if (find_event(calendar, "eat breakfast", NULL) == SUCCESS){
    printf("yes\n");
  }
  else{
    printf("no\n");
  }

  printf("can we find event eat breakfast in day 6?\n");
  if (find_event_in_day(calendar, "eat breakfast", 6, NULL) == SUCCESS){
    printf("yes\n");
  }
  else{
    printf("no, day exceeds day in calendar\n");
  }
  
  printf("printing calendar after removing eat breakfast, do some, and ");
  printf("wake up, brush teeth and shower\n");
  remove_event(calendar, "eat breakfast");
  remove_event(calendar, "do some");
  remove_event(calendar, "wake up, brush teeth and shower");
  print_calendar(calendar, stdout, 1);


  destroy_calendar(calendar);
  printf("\n");
  return SUCCESS;
}


/*this test here is meant to test out clear day and clear calendar functions

 */
static int test5(){

  Calendar *calendar;
  printf("TEST 5 STARTS HERE ------------>\n");
  init_calendar("yellow", 4, comp_minutes, NULL, &calendar);
  add_event(calendar, "wake up, brush teeth and shower", 730, 45, NULL, 1);
  add_event(calendar, "shark teeth", 730, 20, NULL, 1);
  add_event(calendar, "eat breakfast", 730, 45, NULL, 1);
  
  add_event(calendar, "wake up, brush teeth and shower", 730, 45, NULL, 2);
  add_event(calendar, "animal teeth", 730, 20, NULL, 2);
  add_event(calendar, "eat something", 730, 45, NULL, 2);
  
  add_event(calendar, "baking baking baking soda", 730, 45, NULL, 3);
  add_event(calendar, "shark teeth", 730, 20, NULL, 3);
  add_event(calendar, "listen to stuff", 730, 45, NULL, 3);
  print_calendar(calendar, stdout, 1);
  printf("printing calendar AFTER CLEARING OUT DAY 1\n");
  clear_day(calendar, 1);
  print_calendar(calendar, stdout, 1);
  
  printf("printing CALENDAR AFTER CLEARING IT ALL OUT\n");
  clear_calendar(calendar);
  print_calendar(calendar, stdout, 1);
  

  destroy_calendar(calendar);
  printf("\n");
  return SUCCESS;

}


int main() {
   int result = SUCCESS;

   /***** Starting memory checking *****/
   start_memory_check();
   /***** Starting memory checking *****/

   if (test1() == FAILURE) result = FAILURE;
   if (test2() == FAILURE) result = FAILURE;
   if (test3() == FAILURE) result = FAILURE;
   if (test4() == FAILURE) result = FAILURE;
   if (test5() == FAILURE) result = FAILURE;
   /****** Gathering memory checking info *****/
   stop_memory_check();
   /****** Gathering memory checking info *****/
   
   if (result == FAILURE) {
      exit(EXIT_FAILURE);
   }

   return EXIT_SUCCESS;
}

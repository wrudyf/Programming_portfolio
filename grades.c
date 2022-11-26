/*
Name: Rudy F.
UID: 114096296
UMD Directory ID: rfuente5
 */

#include <stdio.h>
#include <math.h>
#define MAX_ASSIGNMENTS 50

/*computes numeric score*/
float compute_score(int size, int drops, int penalty, int scores[], 
		    int weights[], int days_late[]);

/*computes statistics such as mean and standard deviation*/
int compute_stats(int size, int scores[], int points_penalty, int days_late[]);

int main(){
  /*user input part 1 declaration*/
  int points_penalty = -1;
  int assignments_to_drop = -1;
  char stat_choice = 'c';
  int number_of_assignments = -1;
  
  /*user assignment input part 2 declaration*/
  int i = 0;
  int order_index = 1;
  int number = -1;
  int score = -1;
  int weight = -1;
  int days_late = -1;
  
  /*user input arrays for assignments */
  int assignment_number[MAX_ASSIGNMENTS];
  int assignment_score[MAX_ASSIGNMENTS];
  int assignment_weight[MAX_ASSIGNMENTS];
  int assignment_days_late[MAX_ASSIGNMENTS];
  int weight_sum = 0;
  float numeric_score = -1;

  /*getting user input part 1*/
  scanf(" %d %d %c", &points_penalty, &assignments_to_drop, &stat_choice);
  scanf(" %d", &number_of_assignments);
  
  while (i < number_of_assignments){
    
    /*get user input for each assignment*/
    scanf(" %d, %d, %d, %d", &number, &score, &weight, &days_late);
    assignment_number[i] = number;
    assignment_score[i] = score;
    assignment_weight[i] = weight;
    assignment_days_late[i] = days_late;
    
    i++;
  }
  i = 0;

  /*check to see if all weights add up to 100*/
  while (i < number_of_assignments){
    weight_sum += assignment_weight[i];
    i++;
  }
  

  if(weight_sum == 100){
    i = 0;
    numeric_score = compute_score(number_of_assignments, assignments_to_drop, 
				  points_penalty, assignment_score, 
				  assignment_weight, assignment_days_late);
  
    printf("Numeric Score: %5.4f\n", numeric_score);
    printf("Points Penalty Per Day Late: %d\n", points_penalty);
    printf("Number of Assignments Dropped: %d\n", assignments_to_drop);
    printf("Values Provided: \n");
    printf("Assignment, Score, Weight, Days Late\n");
    while (i < number_of_assignments){

      if (assignment_number[i] == order_index){
	printf("%d, %d, ", assignment_number[i], assignment_score[i]);
	printf("%d, %d\n", assignment_weight[i], assignment_days_late[i]);
	order_index++;
      }
      i++;
      if (i == number_of_assignments && order_index < number_of_assignments+1){
	i = 0;
      }
    }
    if (stat_choice == 'Y' || stat_choice == 'y'){
      compute_stats(number_of_assignments, assignment_score, 
		    points_penalty, assignment_days_late);
    }

  }

  else{
    printf("ERROR: Invalid values provided\n");
  }

  return 0;
}

float compute_score(int size, int drops, int penalty, int scores[], 
		    int weights[], int days_late[]){
  int i = 0, j;
  
  int temp = 0;
  int temp_weight = 0;

  int adjusted_scores[MAX_ASSIGNMENTS];
  int sorted_weights[MAX_ASSIGNMENTS];

  float score = 0;
  float weight = 0;
  
  /*get adjusted scores and weights  here*/
  while (i < size){
    adjusted_scores[i] = scores[i] - (penalty * days_late[i]);
    
    if (adjusted_scores[i] < 0){
      adjusted_scores[i] = 0;
    }
    
    sorted_weights[i] = weights[i];
    i++;
  }
  i = 0;
  
  /*sort scores and weights here*/
  while (i < size){

    for (j = i + 1; j < size; j++){
      
      if (adjusted_scores[i] > adjusted_scores[j]){
      /*sort scores*/
      temp = adjusted_scores[i];
      adjusted_scores[i] = adjusted_scores[j];
      adjusted_scores[j] = temp;
      /*sort weights*/
      temp_weight = sorted_weights[i];
      sorted_weights[i] = sorted_weights[j];
      sorted_weights[j] = temp_weight;
      }

    }
    i++;

  }

  
  i = drops;

  /*calculate numeric score and return it as a float*/
  while (i < size){
    score += adjusted_scores[i] * sorted_weights[i];
    weight += sorted_weights[i];
    i++;
  }

  score = score / weight;
  

  return score;
}

int compute_stats(int size, int scores[], int points_penalty, 
		  int days_late[]){
  int i = 0;
  float mean = 0;
  float standard_dev = 0;
  float num_calculator = 0;
  /*calculate mean in this loop*/
  while (i < size){
   mean += scores[i] - (points_penalty * days_late[i]);
   i++;
  }
  mean = mean / size;
  i = 0;

  /*start calculation for standard deviation in this loop*/
  while (i < size){
    
    num_calculator = (scores[i] - (points_penalty * days_late[i])) - mean;
    num_calculator = num_calculator * num_calculator;

    standard_dev += num_calculator;
    num_calculator = 0;
    i++;
  }
  standard_dev = standard_dev / size;
  standard_dev = sqrt(standard_dev);
  
  /*print mean and standard deviation*/
  printf("Mean: %5.4f, Standard Deviation: %5.4f\n", mean, standard_dev);
  return 0;
}

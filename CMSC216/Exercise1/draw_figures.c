#include <stdio.h>

int valid_character(char character);
int draw_rectangle(char character, int width, int length);
int draw_triangle(char character, int size);


int main() {

  int selection = -1;
  char character = 't';
  int width = -1;
  int length = -1;
  int size = -1;

  while (selection != 0) {
    printf("Enter 1(rectangle), 2(triangle), 3(other), 0(quit): ");
    scanf(" %d", &selection);
    if (selection == 0) {

      break;
    
    }

    else if (selection == 1) {
      
      printf("Enter character, width and length: ");
      
      scanf(" %c %d %d", &character, &width, &length);

      if (valid_character(character)){
	printf("Invalid data provided.\n");
	continue;
      }

      else{
      draw_rectangle(character, width, length);
      }
    }

    else if (selection == 2) {
      
      printf("Enter character and size: ");

      scanf(" %c %d", &character, &size);
      
      if (valid_character(character)){
	printf("Invalid data provided.\n");
	continue;
      }
      else{
      draw_triangle(character, size);
      }
    }

    else if(selection == 3) {
      printf("printing something else\n");

    }

    else {
      printf("Invalid choice.\n");
      continue;
    }


  }

  printf("Bye Bye.\n");

  return 0;
}

int valid_character(char character){

  if (character == '*' || character == '%' || character == '#'){
    return 0;
  }

  else{
    return -1;
  }

}

int draw_rectangle(char character, int width, int length){
  int i, j;
  if (width <= 0 || length <= 0){
    printf("Invalid data provided.\n");
    return 0;
  }

  else{

  for (i = 0; i < width; i++){

    for (j = 0; j < length; j++){
      printf("%c", character);
    }
    printf("\n");
  }
  return 1;
  }

}

int draw_triangle(char character, int size){
  int i, j, k, space;
  if (size <= 0){
    printf("Invalid data provided.\n");
    return 0;
  }
  else{

    for (i = 0; i < size; i++){
      
      for (space = i; space < size; space++){
	printf(" ");
      }

      for (j = 1; j < i+2; j++){
	printf("%c", character);
      }

      for (k = 1; k < i + 1; k++){
	printf("%c", character);
      }

      printf("\n");
    }

    return 1;
  }
}

#include <stdio.h>
#include <string.h>
#include "document.h"

int main() {
  Document doc;
  const char* doc_name = "Test get_number_lines";
  int paragraph_number = 0;
  int number_of_lines = 0;


  init_document(&doc, doc_name);
  
  add_paragraph_after(&doc, paragraph_number);
  add_line_after(&doc, 1, 0, "First line");
  add_line_after(&doc, 1, 1, "Second line");
  append_line(&doc, 1, "Third line");
  print_document(&doc);
  get_number_lines_paragraph(&doc, 1, &number_of_lines);
  
  printf("number of lines for paragraph 1: %d\n", number_of_lines);

  return 0;
}

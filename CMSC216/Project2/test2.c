#include <stdio.h>
#include <string.h>
#include "document.h"

int main() {
  Document doc;
  const char* doc_name = "Test replace, highlight, and remove";
  int paragraph_number = 0;



  init_document(&doc, doc_name);
  
  add_paragraph_after(&doc, paragraph_number);
  /*
  add_line_after(&doc, 1, 0, "tooezerlsz");
  
  add_line_after(&doc, 1, 1, "tezrrolls ezroll");
  append_line(&doc, 1, "Third line");
  */

  add_line_after(&doc, 1, 0, "yrtorh");
  add_line_after(&doc, 1, 1, "yrtorhtorrtlo");
  print_document(&doc);
  
  replace_text(&doc, "z", "lz");



  printf("AFTER REPLACEMENT\n");
 
  
  

  print_document(&doc);

  return 0;
}

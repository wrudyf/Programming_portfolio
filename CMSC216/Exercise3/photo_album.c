#include <stdio.h>
#include <stdlib.h>
#include "photo_album.h"

Photo *create_photo(int id, const char *description){
  Photo *photo;
  int length;

  photo = malloc(sizeof(Photo));
  if (photo == NULL){
    return NULL;
  }
  if (description == NULL){
    photo->description = NULL;
  }
  else{
    length = strlen(description);
    photo->description = malloc(length + 1);
    strcpy(photo->description, description);
  }
  
  photo->id = id;
  return photo;
}

void print_photo(Photo *photo){
  if (photo != NULL){
    if (photo->description != NULL){
      printf("Photo Id: %d, Description: %s\n", photo->id, photo->description);
    }
    else{
      printf("Photo Id: %d, Description: None\n", photo->id);
    }
  }
}


void destroy_photo(Photo *photo){
  if (photo != NULL){
    free(photo->description);
    free(photo);
  }
}

void initialize_album(Album *album){
  if (album != NULL){
    album->size = 0;
  }
}

void print_album(const Album *album){
  if (album != NULL){
    if (album->size == 0){
      printf("Album has no photos.\n");
    }
    else{
      int i;
      for (i = 0; i < album->size; i++){
	print_photo(album->all_photos[i]);
      }
    }
  }
}

void destroy_album(Album *album){
  if (album != NULL){
    if (album->size != 0){
      int i;
      for (i = 0; i < album->size; i++){
	destroy_photo(album->all_photos[i]);
      }
    }
  }
}

void add_photo_to_album(Album *album, int id, const char *description){
  if (album != NULL){
    if (album->size < MAX_ALBUM_SIZE - 1){
      
      int length = strlen(description);
      album->all_photos[album->size] = malloc(sizeof(Photo));
      
      album->all_photos[album->size]->id = id;
      
      album->all_photos[album->size]->description = malloc(length + 1);
      strcpy(album->all_photos[album->size]->description, description);
      
      album->size += 1;      
    }
  }
}

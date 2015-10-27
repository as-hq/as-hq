#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>
#include "hiredis.h"
#include <math.h>

#define LOCATION_LEN 256
#define EXPVAL_LEN 10000
#define HOST "localhost"
#define PORT 6379
#define MAX_COMMITS 40
#define MAX_COMMIT_LEN 10000

/*************************************************************************************************************************/
// c <-> Haskell type system 

/*************************************************************************************************************************/
// Helper functions

const char* msgPartDelimiter = "`";

void freeRedis(redisContext *c, redisReply *r){
  freeReplyObject(r);
  redisFree(c);
}

int determineBatchSize(int length){
  assert(length>=0);
  return (int)sqrt(length);
}

void printArray(char** strs) {
  int i;
  for (i=0; strs[i]; ++i) {
    char *pos = strs[i];
    while (*pos != '\0') {
        printf("%c", *(pos++));
    }
    printf("\n");
  }
}

char **strsplit(const char* str, const char* delim) {
    // copy the original string so that we don't overwrite parts of it
    // (don't do this if you don't need to keep the old line,
    // as this is less efficient)
    char *s = strdup(str);

    // these three variables are part of a very common idiom to
    // implement a dynamically-growing array
    size_t tokens_alloc = 1;
    size_t tokens_used = 0;
    char **tokens = calloc(tokens_alloc, sizeof(char*));

    char *token, *strtok_ctx;
    for (token = strtok_r(s, delim, &strtok_ctx);
            token != NULL;
            token = strtok_r(NULL, delim, &strtok_ctx)) {
        // check if we need to allocate more space for tokens
        if (tokens_used == tokens_alloc) {
            tokens_alloc *= 2;
            tokens = realloc(tokens, tokens_alloc * sizeof(char*));
        }
        tokens[tokens_used++] = strdup(token);
    }

    // cleanup
    if (tokens_used == 0) {
        free(tokens);
        tokens = NULL;
    } else {
        tokens = realloc(tokens, tokens_used * sizeof(char*));
    }
    free(s);

    return tokens;
}

/*************************************************************************************************************************/

/* Haskell will free the mallocs here */
char** getCells(char* msg, int length){
  redisContext *c;
  redisReply *reply;
  c = redisConnect((const char*)HOST, PORT);
  clock_t connect = clock(); 

  // removes first and last quotes from string (part of Bytestring's show). See setCells
  // for more information.  
  char *pmsg = msg; pmsg++; // removes first char '"'
  pmsg[strlen(pmsg) - 1] = 0; // removes last char '"'

  char** locs = strsplit(pmsg, msgPartDelimiter);
  char** cells = malloc(length * sizeof(char*));

  int i,j,k; 
  int count = 0; 
  int batch = determineBatchSize(length);
  int nBatches = length/batch;


  if (batch != 0){
    for (i = 0; i < nBatches; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = locs[i*batch+j];
        // printf("getting key: %s\n", key);
        redisAppendCommand(c,"GET %s",key);
      }
      for (j = 0; j < batch; ++j) {
        redisGetReply(c,(void **) &reply);
        if (reply->type == REDIS_REPLY_STRING) {
          // printf("got redis reply: %s", reply->str);
          cells[count] = strdup(reply->str);
          count++;
        } else {
          // puts("got nothing!\n");
          cells[count] = "Nothing";
          count++;
        }
        freeReplyObject(reply);
      }
    }
    // handle remainder after batching
    for (k = count; k < length; ++k) {
      char* key = locs[k];
      // printf("getting key: %s\n", key);
      redisAppendCommand(c, "GET %s", key);
    }
    for (k = count; k < length; ++k) {
      redisGetReply(c,(void **) &reply);
      if (reply->type == REDIS_REPLY_STRING) {
        // printf("got redis reply: %s", reply->str);
        cells[count] = strdup(reply->str);
        count++;
      } else {
        // puts("got nothing!");
        cells[count] = "Nothing";
        count++;
      }
      freeReplyObject(reply);
    }
  }
  redisFree(c);
  assert(length == count);
  clock_t end = clock();
  printf("Get cells time: %f seconds\n", (double)(end - connect) / CLOCKS_PER_SEC);
  return cells; 
}

// The format of the message passed in is an escaped string, with its different components
// delimited by msgPartDelimiter. Since it's escaped, it'll start and end with " characters, which we 
// need to remove at the beginning. 
void setCells(char* msg, int length){
  clock_t begin = clock(); 
  redisContext *c;
  redisReply *reply;
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  clock_t connect = clock(); 

  // removes first and last quotes from string (artifact of ByteString show)
  char *pmsg = msg; pmsg++; // removes first char '"'
  pmsg[strlen(pmsg) - 1] = 0; // removes last char '"'

  // printf("Set cells input: %s \n",pmsg);

  char** lstMsg = strsplit(pmsg, msgPartDelimiter);

  int i,j,k; 
  int batch = determineBatchSize(length);

  if (batch !=0) {
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        int index = i * batch + j; 
        // printf("current index: %d\n", index);
        char* key = lstMsg[index];
        char* val = lstMsg[index + length];
        redisAppendCommand(c,"SET %s %s",key,val); 
      }
      for (j = 0; j < batch; ++j){
        redisGetReply(c, (void **) &reply);
        freeReplyObject(reply);
      }
    }
    // handle remainder after batching
    for (k = (length/batch)*batch; k<length; ++k) {
      char* key = lstMsg[k];
      char* val = lstMsg[k + length];
      redisAppendCommand(c,"SET %s %s",key,val); 
    }
    for (k = (length/batch)*batch; k<length; ++k) {
      redisGetReply(c, (void **) &reply);
      freeReplyObject(reply);
    }
  }
  redisFree(c);
  clock_t end = clock();
  printf("Set cells time: %f seconds\n", (double)(end - connect) / CLOCKS_PER_SEC);

}


/*************************************************************************************************************************/
// Clear

void clear(){
  redisContext *c;
  redisReply *reply;
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  redisAppendCommand(c,"flushall");
  int r = redisGetReply(c, (void **) &reply );
  freeRedis(c,reply);
}

/*************************************************************************************************************************/
// Commits

void pushCommit(char* commit){
  redisContext *c;
  redisReply *reply;
  int i,j;
  int numCommits; 
  printf("\t Going to insert commit %s\n", commit);
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  redisAppendCommand(c,"RPUSH commits %s",commit);
  int r = redisGetReply(c, (void **) &reply );
  freeReplyObject(reply);
  redisAppendCommand(c,"INCRBYFLOAT numCommits 1"); 
  r = redisGetReply(c, (void **) &reply );
  freeReplyObject(reply);
  redisAppendCommand(c,"GET numCommits");
  r = redisGetReply(c, (void **) &reply );
  if (reply->integer){
    numCommits = reply->integer;
  }
  printf("\t Num commits so far: %d\n",numCommits);
  freeReplyObject(reply);
  if (numCommits > MAX_COMMITS){
    redisAppendCommand(c,"LPOP commits"); 
    r = redisGetReply(c, (void **) &reply );
    freeReplyObject(reply);
    redisAppendCommand(c,"INCRBYFLOAT numCommits -1"); 
    r = redisGetReply(c, (void **) &reply );
    freeReplyObject(reply);
  }
  redisFree(c);
}

char* undo(){
  redisContext *c;
  redisReply *reply;
  int i,j;
  char* commit = malloc(MAX_COMMIT_LEN * sizeof(char));
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  redisAppendCommand(c,"RPOP commits");
  int r = redisGetReply(c, (void **) &reply );
  if (reply->str){
    strcpy(commit,reply->str);
  }
  freeReplyObject(reply);
  redisAppendCommand(c,"LPUSH commitsPopped %s",commit);
  r = redisGetReply(c, (void **) &reply );
  freeRedis(c,reply);
  return commit; 
}

char* redo(){
  redisContext *c;
  redisReply *reply;
  int i,j;
  char* commit = malloc(MAX_COMMIT_LEN * sizeof(char));
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  redisAppendCommand(c,"LPOP commitsPopped");
  int r = redisGetReply(c, (void **) &reply );
  if (reply->str){
    strcpy(commit,reply->str);
  }
  freeReplyObject(reply);
  redisAppendCommand(c,"RPUSH commits %s",commit);
  r = redisGetReply(c, (void **) &reply );
  freeRedis(c,reply);
  return commit; 
}


/*************************************************************************************************************************/

/*
int main(){
  char** foo = strsplit("HEllo@Hello@Hello@@@World","@@@");
  printf("%s", foo[0]);
}
*/
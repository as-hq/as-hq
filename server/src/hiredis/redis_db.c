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

void freeRedis(redisContext *c, redisReply *r){
  freeReplyObject(r);
  redisFree(c);
}

int determineBatchSize(int length){
  assert(length>0);
  return (int)sqrt(length);
}

char** str_split(char* a_str, const char a_delim){
    char** result    = 0;
    size_t count     = 0;
    char* tmp        = a_str;
    char* last_comma = 0;
    char delim[2];
    delim[0] = a_delim;
    delim[1] = 0;
    /* Count how many elements will be extracted. */
    while (*tmp){
        if (a_delim == *tmp){
            count++;
            last_comma = tmp;
        }
        tmp++;
    }
    /* Add space for trailing token. */
    count += last_comma < (a_str + strlen(a_str) - 1);
    /* Add space for terminating null string so caller
       knows where the list of returned strings ends. */
    count++;
    result = malloc(sizeof(char*) * count);
    if (result){
        size_t idx  = 0;
        char* token = strtok(a_str, delim);

        while (token){
            assert(idx < count);
            *(result + idx++) = strdup(token);
            token = strtok(0, delim);
        }
        assert(idx == count - 1);
        *(result + idx) = 0;
    }
    return result;
}

/*************************************************************************************************************************/

/* Haskell will free the mallocs here */
char** getCells(char** locs, int length){
  redisContext *c;
  redisReply *reply;
  int i,j; 
  int count = 0; 
  c = redisConnect((const char*)HOST, PORT);

  char** cells = malloc(length * sizeof(char*));
  int batch = determineBatchSize(length);

  if (batch != 0){
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = locs[i*batch+j];
        redisAppendCommand(c,"GET %s",key);
      }
      for (j = 0; j < batch; ++j) {
        redisGetReply(c,(void **) &reply);
        if (reply->type == REDIS_REPLY_STRING) {
          cells[count] = reply->str;
          count++;
        } else if (reply->type == REDIS_REPLY_ERROR) {
          cells[count] = "Nothing";
        }
        freeReplyObject(reply);
      }
    }
  }
  
  redisFree(c);
  assert(length == count);
  return cells; 
}

void setCells(char* locs, char* cells, int length){
  clock_t begin = clock(); 
  redisContext *c;
  redisReply *reply;
  int i,j; 
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  clock_t connect = clock(); 
  printf("Set cells connecting: %f seconds\n", (double)(connect - begin) / CLOCKS_PER_SEC);

  printf("Set cells input: %s %s\n",locs,cells);

  char *plocs = locs; plocs++; // removes first characcter '['
  plocs[strlen(plocs) - 1] = 0; // removes last char ']'
  char *pcells = cells; pcells++;
  pcells[strlen(pcells) - 1] = 0;

  char** lstLocs = str_split(plocs,',');
  char** lstCells = str_split(pcells,',');

  int batch = determineBatchSize(length);
  if (batch !=0) {
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        int index = i * batch + j; 
        char* key = lstLocs[index];
        char* val = lstCells[index];
        redisAppendCommand(c,"SET %s %s",key,val); 
      }
      for (j = 0; j < batch; ++j){
        redisGetReply(c, (void **) &reply);
        freeReplyObject(reply);
      }
    }
  }
  redisFree(c);
  clock_t end = clock();
  printf("Set cells setting time: %f seconds\n", (double)(end - connect) / CLOCKS_PER_SEC);

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


/*int main(){
  int N = 1000000; 
  C_ASCell* cells = malloc(N * sizeof(C_ASCell)); 
  char ** locs = malloc(N * sizeof(char*)); 
  
  int i;  
  for (i = 0 ; i < N; ++i){
    char temp[LOCATION_LEN];  
    cells[i].cLocation = malloc(LOCATION_LEN * sizeof(char));
    sprintf(temp,"Location %d",i);
    strcpy(cells[i].cLocation,temp);
    cells[i].cExpression = "Expression";
    cells[i].cValue = "Value";
    cells[i].cTags = "Tags";
    locs[i] = malloc(LOCATION_LEN * sizeof(char));
    sprintf(locs[i],"Location %d", i);
  }


  clock_t begin = clock(); 
  setCells(cells,N);
  //C_ASCells* recv = getCells(locs,N);
  clock_t end = clock(); 
  printf("Elapsed: %f seconds\n", (double)(end - begin) / CLOCKS_PER_SEC);


  free(cells);
  //free(recv->cells);
  //free(recv); 

} */

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

typedef struct {
  char* cLocation; 
  char* cExpression;
  char* cValue; 
  char* cTags; 
} C_ASCell; 

typedef struct {
  C_ASCell* cells; 
  int numCells; 
} C_ASCells; 


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
C_ASCells* getCells(char** locs, int length){
  redisContext *c;
  redisReply *reply;
  int i,j; 
  int count = 0; 
  c = redisConnect((const char*)HOST, PORT);

  C_ASCell* cells = malloc(length * sizeof(C_ASCell));
  int batch = determineBatchSize(length);

  if (batch != 0){
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = locs[i*batch+j];
        redisAppendCommand(c,"HMGET %s cellExpression cellValue cellTags",key);
      }
      for (j = 0; j < batch; ++j) {
        redisGetReply(c,(void **) &reply);
        if (reply->type == REDIS_REPLY_ARRAY) {
            cells[count].cLocation = strdup(locs[i*batch+j]);
            cells[count].cExpression = strdup(reply->element[0]->str);
            cells[count].cValue = strdup(reply->element[1]->str);
            cells[count].cTags = strdup(reply->element[2]->str);
            count++;
        }
        freeReplyObject(reply);
      }
    }
  }
  
  C_ASCells* cCells = malloc(sizeof(C_ASCells));
  cCells->cells = cells;
  cCells->numCells = count; 
  redisFree(c);
  return cCells; 
}

void setCells(C_ASCell* cells, int length){
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

  int batch = determineBatchSize(length);
  if (batch !=0) {
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        int index = i * batch + j; 
        char* key = cells[index].cLocation;
        char* xp = cells[index].cExpression;
        char* val = cells[index].cValue;
        char* tags = cells[index].cTags;
        redisAppendCommand(c,"HMSET %s cellExpression %s cellValue %s cellTags %s",key,xp,val,tags); 
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

void setCells2(char* locs, char* cells, int length){
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
  char** lstLocs = str_split(locs,'|');
  char** lstCells = str_split(cells,'|');

  int batch = determineBatchSize(length);
  if (batch !=0) {
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        int index = i * batch + j; 
        char* key = lstLocs[index];
        char* val = lstCells[index];
        redisAppendCommand(c,"HSET %s %s",key,val); 
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


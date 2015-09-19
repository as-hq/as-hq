#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>
#include "hiredis.h"



#define LOCATION_LEN 256
#define EXPVAL_LEN 256
#define HOST "localhost"
#define PORT 6379
#define MAX_COMMITS 40
#define MAX_COMMIT_LEN 10000

/*************************************************************************************************************************/
// c <-> Haskell type system 

typedef struct {
  char* location; 
  char* expVal;  
} C_ASCell; 

typedef struct {
  C_ASCell* cells; 
  int numCells; 
} C_ASCells; 

typedef struct {
    char* fromLoc; 
    char* toLoc; 
} C_ASRelation; 

typedef struct {
  C_ASRelation* dag; 
  int numEdges;
} C_ASDAG; 


/*************************************************************************************************************************/
// Helper functions

void freeRedis(redisContext *c, redisReply *r){
  freeReplyObject(r);
  redisFree(c);
}

int determineBatchSize(int length){
  if (length>10000)
    return 1000; 
  else 
    return length; 
}

/*************************************************************************************************************************/

C_ASCells* getCells(char** Locs, int length){
  redisContext *c;
  redisReply *reply;
  int i,j; 
  int count = 0; 
  c = redisConnect((const char*)HOST, PORT);
  for (i = 0 ; i < length; ++i){
    printf("\tgetCells input %s\n",Locs[i]);
  }
  C_ASCell* cCell = malloc(length * sizeof(C_ASCell));
  int batch = determineBatchSize(length);
  if (batch != 0){
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = Locs[i*batch+j];
        //printf("Key %s\n",key);
        redisAppendCommand(c,"HGET 1 %s",key);
        }
        for (j = 0; j < batch; ++j) {
          int r = redisGetReply(c,(void **) &reply);
          if (reply->str){
            //printf("Redis string %s %s\n",reply->str,Locs[i*batch+j]);
            cCell[count].location = strdup(Locs[i*batch+j]);
            cCell[count].expVal = strdup(reply->str);
            count++;
          }
          freeReplyObject(reply);
        }
    }
  }
  for (i = 0 ; i < count; ++i){
    printf("get test: %s %s \n",cCell[i].location,cCell[i].expVal); 
  }
  C_ASCells* cCells = malloc(sizeof(C_ASCells));
  cCells->cells = cCell;
  cCells->numCells = count; 
  redisFree(c);
  return cCells; 
}

void setCells(char** Locs, char** ExpVal, int length){
  redisContext *c;
  redisReply *reply;
  int i,j; 
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  int batch = determineBatchSize(length);
  if (batch !=0) {
    for (i = 0; i < length/batch; ++i) {
      unsigned int cmd = 0;
      for (j = 0; j < batch; ++j) {
        char* key = Locs[i*batch+j];
        char* val = ExpVal[i*batch+j];
        redisAppendCommand(c,"HSET 1 %s %s",key,val);
        ++cmd;
        }
      while (cmd-- > 0){
        int r = redisGetReply(c, (void **) &reply );
        freeReplyObject(reply);
      }
    }
  }
  redisFree(c);
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
// ToLoc -> SET {FromLoc 1, FromLoc 2 etc.}


void deleteEdges(char** ToLocs, int length){
  redisContext *c;
  redisReply *reply;
  int i,j;
  int numDelEdges = 0; 
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  int batch = determineBatchSize(length);
  if (batch !=0){
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = ToLocs[i*batch+j];
        redisAppendCommand(c,"SCARD %s",key);
        redisAppendCommand(c,"DEL %s",key);
        redisAppendCommand(c,"SREM toLocSet %s",key);
        }
      for (j = 0; j < 3*batch; ++j){
        int r = redisGetReply(c, (void **) &reply );
        if (reply->integer && j%3 == 0){
          //printf("dec count %llu\n",reply->integer);
          numDelEdges += reply->integer; 
        }
        freeReplyObject(reply);
      }
    }
  }
  printf("\tNumber of edges deleted: %d\n",numDelEdges);
  redisAppendCommand(c,"INCRBYFLOAT numEdges %d",-numDelEdges);
  int r = redisGetReply(c, (void **) &reply );
  freeRedis(c,reply);
}

void insertEdges(char** FromLocs, char** ToLocs, int length){
  redisContext *c;
  redisReply *reply;
  int i,j; 
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
    return; 
  }
  int batch = determineBatchSize(length);
  if (batch !=0){
    for (i = 0; i < length/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = ToLocs[i*batch+j];
        char* val = FromLocs[i*batch+j];
        //printf("Adding %s %s\n",key,val); 
        redisAppendCommand(c,"SADD %s %s",key,val);
        redisAppendCommand(c,"SADD toLocSet %s",key);
      }
      for (j = 0; j < 2*batch; ++j){
        int r = redisGetReply(c, (void **) &reply );
        freeReplyObject(reply);
      }
    }
  }
  redisAppendCommand(c,"INCRBYFLOAT numEdges %d",length);
  int r = redisGetReply(c, (void **) &reply );
  if (reply->str){
    //printf("num edges is now %s\n",reply->str);
  }
  freeReplyObject(reply);
  redisFree(c);
}

C_ASDAG* getEdges(){
  redisContext *c;
  redisReply *reply;
  int i,j,k; 
  c = redisConnect((const char*)HOST, PORT);
  if (c->err) {
    freeRedis(c,reply); 
  }
  printf("\tconnected\n");

  // get number of locations and num edges
  int numToLocs = 0; int numEdges = 0; 
  redisAppendCommand(c,"SCARD toLocSet");
  int r = redisGetReply(c, (void **) &reply );
  if (reply->integer){
    printf("\tnum locs int %llu\n",reply->integer);
    numToLocs = (int)reply->integer; 
  }
  if (reply->str){
    printf("\tnum locs str %s\n",reply->str);
    numToLocs = atoi(reply->str); 
  }
  freeReplyObject(reply);

  redisAppendCommand(c,"GET numEdges");
  r = redisGetReply(c, (void **) &reply );
  if (reply->str){
    printf("\tnum edges %s\n",reply->str);
    numEdges = atoi(reply->str); 
    printf("\tnum edges %d\n",numEdges);
  }
  freeReplyObject(reply);

  // get the actual locations
  char* ToLocs[numToLocs]; // = malloc(numToLocs*LOCATION_LEN*sizeof(char)); //[numToLocs]; 
  for (i = 0 ; i < numToLocs; ++i){
    ToLocs[i] = malloc(LOCATION_LEN*sizeof(char));
  }
  redisAppendCommand(c,"SMEMBERS toLocSet");
  r = redisGetReply(c, (void **) &reply );
  if (reply->type == REDIS_REPLY_ARRAY) {
    //printf("num in toLocSet: %zd\n",reply->elements);
    for (j = 0; j < reply->elements; j++) {
      strcpy(ToLocs[j],reply->element[j]->str);
    }
  }
  freeReplyObject(reply);

  for (i = 0 ; i < numToLocs ; ++i){
    printf("tolocs elem: %s\n",ToLocs[i]);
  }

  // now get the edges
  C_ASRelation* rels = malloc(numEdges * sizeof(C_ASRelation));
  int batch = determineBatchSize(numToLocs);
  int count = 0; 
  if (batch !=0) {
    for (i = 0; i < numToLocs/batch; ++i) {
      for (j = 0; j < batch; ++j) {
        char* key = ToLocs[i*batch+j];
        redisAppendCommand(c,"SMEMBERS %s",key);
        //printf("command: SMEMBERS %s with index %d \n",key, i*batch+j);
      }
      for (j = 0; j < batch; ++j){
        int r = redisGetReply(c, (void **) &reply );
        if (reply->type == REDIS_REPLY_ARRAY) {
          //printf("array with elements: %zd \n", reply->elements);
          for (k = 0; k < reply->elements; k++) {
            rels[count].fromLoc = strdup(reply->element[k]->str);
            rels[count].toLoc = strdup(ToLocs[i*batch+j]);
            //printf("\tfromloc, toloc %d %s %s\n",count,rels[count].fromLoc,rels[count].toLoc);
            count++; 
          }
        }
        freeReplyObject(reply);
      }
    }
  }
  redisFree(c);

  // Free toLocs
  for (i = 0 ; i < numToLocs ; ++i){
    free(ToLocs[i]);
  }

  for (i = 0 ; i < numEdges; ++i){
    printf("\tfromloc, toloc %d %s %s\n",i,rels[i].fromLoc,rels[i].toLoc);
  }

  // return a dag
  C_ASDAG* d = malloc(sizeof(C_ASDAG));
  d->dag = rels; 
  d->numEdges = numEdges; 
  printf("returned from c: get edges\n");
  return d; 
}

int main(){
  int N = 1000000; 
  char ** locs = malloc(N * sizeof(char*)); 
  char ** ev = malloc(N * sizeof(char*)); 
  int i;  
  for (i = 0 ; i < N; ++i){
    locs[i] = malloc(LOCATION_LEN * sizeof(char));
    ev[i] = malloc(EXPVAL_LEN * sizeof(char));
    sprintf(locs[i], "Location %d", i);
    sprintf(ev[i], "ExpVal %d", i);
  }

  clock_t begin = clock(); 
  setCells(locs,ev,N);
  clock_t end = clock(); 
  printf("Elapsed: %f seconds\n", (double)(end - begin) / CLOCKS_PER_SEC);



  for (i = 0 ; i < N; ++i){
    free(locs[i]); 
    free(ev[i]);
  }
  free(locs);
  free(ev);

}


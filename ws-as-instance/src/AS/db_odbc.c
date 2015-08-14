#include <stdio.h>
#include <stdlib.h>
#include <sql.h>
#include <sqlext.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#define LOCATION_LEN  256
#define EXPRESSION_LEN 256
#define VALUE_LEN 256
#define FALSE 0

//=======================================================================================================================================
// Define the DB structs

// C-version of ASCell
typedef struct {
    char* location; 
    char* expression; 
    char* value; 
} C_ASCell; 

// C-version of Relation
typedef struct {
    char* fromLoc; 
    char* toLoc; 
} C_ASRelation;  

// wrapper struct around C_ASCell to include number of cells, so that Haskell knows where to stop
typedef struct {
    C_ASCell* cells; 
    int numCells; 
} C_ASCells; 

// wrapper struct around C_Relation to include number of edges, so that Haskell knows where to stop
typedef struct {
    C_ASRelation* dag; 
    int numEdges;
} C_ASDAG; 


//=======================================================================================================================================
// Utility functions

// Frees various SQL handles
void freeHandles(SQLHENV *henv, SQLHDBC *hdbc, SQLHSTMT *hstmt){
    if (*hstmt != SQL_NULL_HSTMT)
        SQLFreeHandle(SQL_HANDLE_STMT, *hstmt);
    if (*hdbc != SQL_NULL_HDBC) {
        SQLDisconnect(*hdbc);
        SQLFreeHandle(SQL_HANDLE_DBC, *hdbc);
    }
    if (*henv != SQL_NULL_HENV)
        SQLFreeHandle(SQL_HANDLE_ENV, *henv);
}

// Initializes the essential SQL handles
void initHandles(SQLHENV *henv, SQLHDBC *hdbc, SQLHSTMT *hstmt, SQLRETURN *retcode){
    // Allocate environment handle
    *retcode = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, henv);
    // Set the ODBC version environment attribute
    *retcode = SQLSetEnvAttr(*henv, SQL_ATTR_ODBC_VERSION,
                            (SQLPOINTER*)SQL_OV_ODBC3, 0);
    // Allocate connection handle
    *retcode = SQLAllocHandle(SQL_HANDLE_DBC, *henv, hdbc);
    // Set login timeout to 5 seconds
    SQLSetConnectAttr(*hdbc, SQL_LOGIN_TIMEOUT, (SQLPOINTER)5, 0);
    // Connect to data source
    *retcode = SQLConnect(*hdbc, (SQLCHAR*) "AlphaSheets", SQL_NTS,(SQLCHAR*) NULL, 0, NULL, 0);
    // Allocate statement handle
    *retcode = SQLAllocHandle(SQL_HANDLE_STMT, *hdbc, hstmt);
}

// Initializes handles and parameter arrays (when there's an array input)
void initHandlesAndParam(SQLHENV *henv, SQLHDBC *hdbc, SQLHSTMT *hstmt, SQLRETURN *retcode, 
    SQLUSMALLINT ParamStatusArray[], SQLLEN ParamsProcessed, int PARAM_ARRAY_SIZE){
    initHandles(henv,hdbc,hstmt,retcode);
    // Set parameter set size, status array and params processed pointers
    *retcode = SQLSetStmtAttr (*hstmt, SQL_ATTR_PARAM_BIND_TYPE,SQL_PARAM_BIND_BY_COLUMN, 0);
    *retcode = SQLSetStmtAttr (*hstmt, SQL_ATTR_PARAMSET_SIZE,(SQLPOINTER) PARAM_ARRAY_SIZE, 0);
    *retcode = SQLSetStmtAttr (*hstmt, SQL_ATTR_PARAM_STATUS_PTR,ParamStatusArray, PARAM_ARRAY_SIZE);
    *retcode = SQLSetStmtAttr (*hstmt, SQL_ATTR_PARAMS_PROCESSED_PTR, &ParamsProcessed, 0);
}

//=======================================================================================================================================


C_ASCells* getCells(char * x[], int length) {
    
    int k; 
    char Locs[length][LOCATION_LEN];
    for (k= 0 ; k < length; k++)
       strcpy(Locs[k],x[k]);

    // Initialize handles
    SQLHENV   henv  = SQL_NULL_HENV;   // Environment
    SQLHDBC   hdbc  = SQL_NULL_HDBC;   // Connection handle
    SQLHSTMT  hstmt = SQL_NULL_HSTMT;  // Statement handle
    SQLRETURN retcode;

    // Initialize parameters
    SQLCHAR Location[LOCATION_LEN]; 
    SQLCHAR Expression[EXPRESSION_LEN], Value[VALUE_LEN];
    SQLLEN Location_l=0, Expression_l=0, Value_l=0;
    int PARAM_ARRAY_SIZE = length; 
    SQLUSMALLINT ParamStatusArray[PARAM_ARRAY_SIZE];
    SQLLEN       ParamsProcessed=0;

    initHandlesAndParam(&henv,&hdbc,&hstmt,&retcode,ParamStatusArray,ParamsProcessed,PARAM_ARRAY_SIZE);

    // Bind array values of location parameter
    retcode = SQLBindParameter(hstmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, LOCATION_LEN, 0, Locs, LOCATION_LEN, NULL);
    // Bind columns to the Record_num Field of the first row in the array 
    retcode = SQLBindCol(hstmt, 1, SQL_C_CHAR,  Location, LOCATION_LEN, &Location_l);    
    retcode = SQLBindCol(hstmt, 2, SQL_C_CHAR,  Expression, EXPRESSION_LEN, &Expression_l);
    retcode = SQLBindCol(hstmt, 3, SQL_C_CHAR,  Value, VALUE_LEN, &Value_l);

    char* stmt = "SELECT Location,Expression,Value FROM a_s_cells WHERE Location=?";
    clock_t s = clock(); 
    retcode = SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT,(SQLPOINTER)FALSE, 0);    
    retcode = SQLExecDirect(hstmt,(SQLCHAR*) stmt, SQL_NTS);
    retcode = SQLEndTran (SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
    clock_t end = clock(); 
    printf("\tGetting cells: %f seconds\n", (double)(end - s) / CLOCKS_PER_SEC);


    C_ASCell* cells; 
    cells=malloc(PARAM_ARRAY_SIZE*sizeof(C_ASCell));

    // Fetch and print each row of data until SQL_NO_DATA returned.
    int i = 0; int c = 0; 
    for (i = 0; ; i++) {
        retcode = SQLFetch(hstmt);
        if (retcode == SQL_SUCCESS || retcode == SQL_SUCCESS_WITH_INFO) {
            cells[c].location = strdup(Location); cells[c].expression = strdup(Expression); cells[c].value = strdup(Value); 
            c++; 

        } else {
            if (retcode != SQL_NO_DATA) {
            } else {
                break;
            }
        }
    }

    C_ASCells* ret; 
    ret = malloc(sizeof(C_ASCells)); 
    ret->cells=cells; 
    ret->numCells=c; 
    freeHandles(&henv,&hdbc,&hstmt);
    clock_t rEnd = clock(); 
    printf("\tGetting cells processing: %f seconds\n", (double)(rEnd - end) / CLOCKS_PER_SEC);

    return ret; 
}

void setCells (char *x[], char *y[], char *z[], int length) {
    
    int k; 
    char Locs[length][LOCATION_LEN];
    char Exprs[length][EXPRESSION_LEN];
    char Vals[length][VALUE_LEN];
    for (k = 0 ; k < length; k++){
       strcpy(Locs[k],x[k]);
       strcpy(Exprs[k],y[k]);
       strcpy(Vals[k],z[k]);
    }

    // Initialize handles
    SQLHENV   henv  = SQL_NULL_HENV;   // Environment
    SQLHDBC   hdbc  = SQL_NULL_HDBC;   // Connection handle
    SQLHSTMT  hstmt = SQL_NULL_HSTMT;  // Statement handle
    SQLRETURN retcode;
    // Initialize parameters
    int PARAM_ARRAY_SIZE = length; 
    SQLUSMALLINT ParamStatusArray[PARAM_ARRAY_SIZE];
    SQLLEN       ParamsProcessed=0;
    initHandlesAndParam(&henv,&hdbc,&hstmt,&retcode,ParamStatusArray,ParamsProcessed,PARAM_ARRAY_SIZE);

    // Bind array values of location,expression,value parameters
    retcode = SQLBindParameter(hstmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, LOCATION_LEN, 0, Locs, LOCATION_LEN, NULL);
    retcode = SQLBindParameter(hstmt, 2, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, EXPRESSION_LEN, 0, Exprs, EXPRESSION_LEN, NULL);
    retcode = SQLBindParameter(hstmt, 3, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, VALUE_LEN, 0, Vals, VALUE_LEN, NULL);
    retcode = SQLBindParameter(hstmt, 4, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, LOCATION_LEN, 0, Locs, LOCATION_LEN, NULL);
    retcode = SQLBindParameter(hstmt, 5, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, EXPRESSION_LEN, 0, Exprs, EXPRESSION_LEN, NULL);
    retcode = SQLBindParameter(hstmt, 6, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, VALUE_LEN, 0, Vals, VALUE_LEN, NULL);

    char* stmt = "INSERT INTO a_s_cells(Location,Expression,Value) values(?,?,?) ON DUPLICATE KEY UPDATE Location=?,Expression=?,Value=?";
    clock_t s = clock(); 
    retcode = SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT,(SQLPOINTER)FALSE, 0);    
    retcode = SQLExecDirect(hstmt,(SQLCHAR*) stmt, SQL_NTS);
    retcode = SQLEndTran (SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
    clock_t end = clock(); 
    printf("\tSetting cells: %f seconds\n", (double)(end - s) / CLOCKS_PER_SEC);

    freeHandles(&henv,&hdbc,&hstmt);
}

C_ASDAG* getEdges() {

    // Initialize handles
    SQLHENV   henv  = SQL_NULL_HENV;   // Environment
    SQLHDBC   hdbc  = SQL_NULL_HDBC;   // Connection handle
    SQLHSTMT  hstmt = SQL_NULL_HSTMT;  // Statement handle
    SQLRETURN retcode;
    int INIT_NUM_EDGES = 2; 

    SQLCHAR FromLoc[LOCATION_LEN]; 
    SQLCHAR ToLoc[LOCATION_LEN];
    SQLLEN FromLoc_l=0; SQLLEN ToLoc_l=0;
    initHandles(&henv,&hdbc,&hstmt,&retcode);

    retcode = SQLBindCol(hstmt, 1, SQL_C_CHAR,  FromLoc, LOCATION_LEN, &FromLoc_l);    
    retcode = SQLBindCol(hstmt, 2, SQL_C_CHAR,  ToLoc, LOCATION_LEN, &ToLoc_l);

    char* stmt = "SELECT FromLoc,ToLoc FROM a_s_relations";
    clock_t s = clock();
    retcode = SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT,(SQLPOINTER)FALSE, 0);    
    retcode = SQLExecDirect(hstmt,(SQLCHAR*) stmt, SQL_NTS);
    retcode = SQLEndTran (SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
    clock_t end = clock(); 
    printf("\tGetting edges: %f seconds\n", (double)(end - s) / CLOCKS_PER_SEC);


    C_ASRelation* relations; 
    relations=malloc(INIT_NUM_EDGES*sizeof(C_ASRelation));

    // Fetch and print each row of data until SQL_NO_DATA returned.
    int i = 0; int r = 0; 
    for (i = 0; ; i++) {
        retcode = SQLFetch(hstmt);
        if (retcode == SQL_SUCCESS || retcode == SQL_SUCCESS_WITH_INFO) {
            if (r < INIT_NUM_EDGES) {
                relations[r].fromLoc = strdup(FromLoc); relations[r].toLoc = strdup(ToLoc);
            }
            else{
                INIT_NUM_EDGES +=10; 
                relations = (C_ASRelation*)realloc(relations, sizeof(C_ASRelation) * INIT_NUM_EDGES);
                relations[r].fromLoc = strdup(FromLoc); relations[r].toLoc = strdup(ToLoc);
            }
            r++; 

        } else {
            if (retcode != SQL_NO_DATA) {
            } else {
                break;
            }
        }
    }

    relations = (C_ASRelation*)realloc(relations,sizeof(C_ASRelation)*r);
    C_ASDAG* graph; 
    graph = malloc(sizeof(C_ASDAG));
    assert(graph != NULL);
    graph->dag = relations; 
    graph->numEdges = r;
    freeHandles(&henv,&hdbc,&hstmt);
    clock_t rEnd = clock();
    printf("\tGetting edges processing: %f seconds\n", (double)(rEnd - end) / CLOCKS_PER_SEC);
    return graph; 
}


void deleteEdges (char *x[], int length) {

    int k; 
    char ToLocs[length][LOCATION_LEN];
    for (k = 0 ; k < length; k++){
       strcpy(ToLocs[k],x[k]);
    }

    // Initialize handles
    SQLHENV   henv  = SQL_NULL_HENV;   // Environment
    SQLHDBC   hdbc  = SQL_NULL_HDBC;   // Connection handle
    SQLHSTMT  hstmt = SQL_NULL_HSTMT;  // Statement handle
    SQLRETURN retcode;
    // Initialize parameters
    int PARAM_ARRAY_SIZE = length; 
    SQLUSMALLINT ParamStatusArray[PARAM_ARRAY_SIZE];
    SQLLEN       ParamsProcessed=0;
    initHandlesAndParam(&henv,&hdbc,&hstmt,&retcode,ParamStatusArray,ParamsProcessed,PARAM_ARRAY_SIZE);

    // Bind array values of location,expression,value parameters
    retcode = SQLBindParameter(hstmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, LOCATION_LEN, 0, ToLocs, LOCATION_LEN, NULL);

    char* stmt = "DELETE FROM a_s_relations WHERE ToLoc=?;";
    clock_t begin = clock(); 
    retcode = SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT,(SQLPOINTER)FALSE, 0);    
    retcode = SQLExecDirect(hstmt,(SQLCHAR*) stmt, SQL_NTS);
    retcode = SQLEndTran (SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
    clock_t end = clock();
    printf("\tDeletion of edges: %f seconds\n", (double)(end - begin) / CLOCKS_PER_SEC);


    freeHandles(&henv,&hdbc,&hstmt);
}


void insertEdges (char *x[], char *y[], int length) {

    int k; 
    char FromLocs[length][LOCATION_LEN];
    char ToLocs[length][EXPRESSION_LEN];
    for (k = 0 ; k < length; k++){
       strcpy(FromLocs[k],x[k]);
       strcpy(ToLocs[k],y[k]);
    }


    // Initialize handles
    SQLHENV   henv  = SQL_NULL_HENV;   // Environment
    SQLHDBC   hdbc  = SQL_NULL_HDBC;   // Connection handle
    SQLHSTMT  hstmt = SQL_NULL_HSTMT;  // Statement handle
    SQLRETURN retcode;
    // Initialize parameters
    int PARAM_ARRAY_SIZE = length; 
    SQLUSMALLINT ParamStatusArray[PARAM_ARRAY_SIZE];
    SQLLEN       ParamsProcessed=0;
    initHandlesAndParam(&henv,&hdbc,&hstmt,&retcode,ParamStatusArray,ParamsProcessed,PARAM_ARRAY_SIZE);

    // Bind array values of location,expression,value parameters
    retcode = SQLBindParameter(hstmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, LOCATION_LEN, 0, FromLocs, LOCATION_LEN, NULL);
    retcode = SQLBindParameter(hstmt, 2, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, LOCATION_LEN, 0, ToLocs, LOCATION_LEN, NULL);

    char* stmt = "INSERT INTO a_s_relations(FromLoc,ToLoc) VALUES(?,?)";

    clock_t begin = clock(); 
    retcode = SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT,(SQLPOINTER)FALSE, 0);    
    retcode = SQLExecDirect(hstmt,(SQLCHAR*) stmt, SQL_NTS);
    retcode = SQLEndTran (SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
    clock_t end = clock();
    printf("\tInsertion of edges: %f seconds\n", (double)(end -  begin) / CLOCKS_PER_SEC);

    freeHandles(&henv,&hdbc,&hstmt);
}

//=======================================================================================================================================

void testGetEdges(){
    C_ASDAG * d; 
    d = malloc(sizeof(C_ASDAG)); 
    clock_t s = clock(); 
    d = getEdges();
    clock_t end = clock(); 
    printf("Getting relations: %f seconds\n", (double)(end - s) / CLOCKS_PER_SEC);
    printf("%i\n",d->numEdges);
}

void testGetCells(char *rels[], int N){
    C_ASCells* c; 
    c = malloc(sizeof(C_ASCells));
    clock_t s = clock();
    c = getCells(rels,N); 
    clock_t end = clock(); 
    printf("Getting cells: %f seconds\n", (double)(end - s) / CLOCKS_PER_SEC);
}


// sudo apt-get libmyodbc
// sudo chmod a+w+r odbc.ini in etc folder, edit config to add database, test with isql -v <database name>
// sudo apt-get install unixodbc-dev

// in mysql, create a_s_cells with primary key location, expression, value and a_s_relations with fromloc and toloc

// gcc -o db_odbc db_odbc.c -lodbc
// gcc -c db_odbc.c -fPIC -lodbc (to create .o file) 
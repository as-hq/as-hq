#ifndef _SENDAO_SUPPORT_H
#define _SENDAO_SUPPORT_H

typedef signed char sint8_t;
typedef signed short sint16_t;
typedef signed long int sint32_t;
typedef signed long long int sint64_t;

typedef void *pointer_t;

// System Includes
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <malloc.h>
#include <stdint.h>
#include <signal.h>



// Macro Definitions
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE !TRUE
#endif

#ifdef WIN32
#include <windows.h>
#include <signal.h>

#define U_WINDOWS true
#define USE_WINDOWS true

#define DLL_EXPORT __declspec(dllexport)
#ifdef DLL_LIB
#define DLL_HOOK __declspec(dllexport)
#else
#define DLL_HOOK __declspec(dllimport)
#endif

typedef CRITICAL_SECTION timestop_v;
typedef DWORD thread_v;

#else

#define USE_LINUX true

//#define DLL_HOOK
//#define DLL_EXPORT

#include <pthread.h>
typedef pthread_mutex_t timestop_v;
typedef pthread_t thread_v;

#endif



typedef unsigned int u32;

// Support Includes

#include "senlist.h"
#include "senarray.h"
#include "senattribs.h"
#include "senexpr.h"

#include "objects.h"
#include "lists.h"
#include "tables.h"
#include "maps.h"
#include "ptr.h"

class stringbuf
{
	public:
	char *p;
	uint16_t len;
	uint16_t alloced;
	uint16_t alloc_chunk;
	uint16_t maxlen;

	public:
	stringbuf(uint16_t alloc_chunk=1024);
	stringbuf(const char*, uint16_t alloc_chunk=1024);
	~stringbuf();
	void append(const char*);
	void append(char c);
	void reset(void);
	void expand();
	void printf(const char *src,...);
	void clear(void);
};

class linebuf
{
	public:
	char **buffer;
	int lines, used;
	linebuf(const char *src=NULL);
	~linebuf();
	void addline(const char *str);
	void setline(int lineno, const char *str);
	void append( const char *str );
	void expand(void);
	void clear(void);
};

class bitbuf
{
	public:
	char *p;
	uint16_t len;
	uint16_t alloced;
	uint16_t alloc_chunk;

	public:
	bitbuf(uint16_t alloc_chunk=1024);
	~bitbuf();
	void append(char*, int len);
	void clear(void);
};


void tsInit( timestop_v *);
void tsEnter( timestop_v *);
void tsExit( timestop_v *);
void tsDelete( timestop_v *);
thread_v tsGetCurrentThreadId(void);


// util.cpp

char *readFile( const char *fileName );
uint32_t charkey( char c );
int strkey( const char * );
u32 crc32(const char *pin);
int number_range(int low, int high);
int rrand(int low, int high);
float frand(float low, float high);
void memadd( char **pdest, char *psrc, int len );
void memaddstr( char **pdest, char *psrc );
void memsub( char **psrc, char *pdest, int len );
void memsubstr( char **psrc, char *pdest );

// string.cpp
char *str_dup( const char *p );
char *strndup(const char *str, int len);
int str_c_cmp( const char *a, const char *b );
int str_cmp( const char *, const char * );
const char *str_r_str(const char *src, const char *search);
void Arg(const char *args, int n, char *argn);
void strexpand( char **buf, const char *add );
tlist *split( const char *str, const char *sep );
tlist *split( const char *str, tlist *seps );
void strip_newline( char *str );
void strcpysafe( char *dst, char *src );

// matching.cpp
//int do_match( const char *pattern, const char *sample );
//int sencmp( const char *pattern, const char *sample, bool matchcase=true );

// io.cpp
extern bool random_initted;
const char *read_to_letter(FILE *fp, char letter, bool bLines);
char *readwholefile(const char *fn);
char *readwholefile(FILE *fp);
const char *getstaticword(FILE *fp);
const char *getstaticline(FILE *fp);
char *getword(FILE *fp);
char *getline(FILE *fp);
char *getstring(FILE *fp);
int getnumber(FILE *fp);
void mkfulldir(char *str);

#define UPPER(x)		( ( ( (x) >= 'a' ) && ( (x) <= 'z' ) ) ? (x)-('a'-'A') : (x) )
#define LOWER(x)		( ( ( (x) >= 'A' ) && ( (x) <= 'Z' ) ) ? (x)-('A'-'a') : (x) )
#define MAX(x, y)		((x >= y) ? x : y)


#endif

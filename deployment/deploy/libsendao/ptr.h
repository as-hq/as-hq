#ifndef __GLUTON_PTR_H
#define __GLUTON_PTR_H


typedef class Pointer Pointer;
typedef struct _mapstring mapstring;

// api facing types

class ptrmap
{
public:
	ptrmap();
	~ptrmap();
	DMap *byAddr;
	SMap *byName;
	//NameList *namekey;

public:
	Pointer *Get(void *);
	Pointer *Get(const char * );
	void Del( void * );
	Pointer *Save(void *, sint32_t type=0, const char *name=NULL);
	void *Alloc(int sz);
	void *Realloc(void *, int);

	static uint32_t addrkey(void*);

};


/*
 * strmap API
 *
 * Dynamic string deduplication
 *
 * newptr = GS->Inter( "Some Static String" );
 * // note: no need to Free(), Inter'd strings stick around
 *
 * newptr = GS->Inter
 *
 * newptr = GS->Read( oldptr );
 * GS->Free( newptr );
 *
 * newptr = GS->Read( "Some Static String" );
 * GS->Free( newptr ); // dereference after using a copy
 *
 *
 *
 */
class strmap
{
public:
	strmap();
	~strmap();
	SMap *byName;

public:
	/* Read/Copy pull existing strings into the map , returning a pointer to the new reference to use. */
	// Read uses a static memory buffer, no refcounters, no need to Free.
	char *Inter( const char *Src );
	char *Read( char* );
	char *Read( const char * );

	// Copy uses a dynamic memory buffer, with refcounters, so free your references and they won't leak.
	char *Copy( char* );
	char *Copy( const char* );
	void Free( char* );
	bool Compare( const char *, const char * );
};




// internal types

class Pointer
{
public:
	Pointer();
	Pointer(sint32_t,void*);
	~Pointer();
	sint32_t type;
	void *addr;
	char *name;
	int sz, count;

public:
	void SetName(const char *);
	// manage object parameters
//	Pointer *Get(const char *);
//	void Set(const char *, Pointer *);
};


struct _mapstring
{
	char *str;
	int count;
	int len;
};

#endif

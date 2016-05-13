#include "sendao.h"

strmap *GS = NULL;

strmap::strmap()
{
	byName = new SMap(128);
}

strmap::~strmap()
{
	delete byName;
}

char *strmap::Read( char *Src )
{
	if( !Src ) return NULL;
	mapstring *ms = (mapstring*)byName->Get(Src);
	if( !ms ) {
		ms = (mapstring*)malloc(sizeof(mapstring));
		ms->str = Src;
		ms->len = strlen(Src);
		ms->count = 1;
		byName->Set(ms->str,(void*)ms);
	} else if( ms->count > 0 ) {
		ms->count++;
		free(Src);
	}
	return ms->str;
}
char *strmap::Copy( char *Src )
{
	if( !Src ) return NULL;
	mapstring *ms = (mapstring*)byName->Get(Src);
	if( !ms ) {
		ms = (mapstring*)malloc(sizeof(mapstring));
		ms->str = (char*)malloc(strlen(Src)+1);
		strcpy( ms->str, Src );
		ms->count = 1;
		byName->Set(ms->str,(void*)ms);
	} else if( ms->count > 0 ) {
		ms->count++;
	}
	return ms->str;
}
char *strmap::Copy( const char *Src )
{
	if( !Src ) return NULL;
	mapstring *ms = (mapstring*)byName->Get(Src);
	if( !ms ) {
		ms = (mapstring*)malloc(sizeof(mapstring));
		ms->str = (char*)malloc(strlen(Src)+1);
		strcpy( ms->str, Src );
		ms->count = 1;
		byName->Set(ms->str,(void*)ms);
	} else if( ms->count > 0 ) {
		ms->count++;
	}
	return ms->str;
}
void strmap::Free( char *Addr )
{
	if( !Addr ) return;
	mapstring *ms = (mapstring*)byName->Get(Addr);
	if( !ms ) {
		//lprintf("strmap string not found(%s)", Addr);
		free(Addr);
		return;
	}
	if( ms->count > 0 ) {
		ms->count--;
		if( ms->count == 0 ) {
			//lprintf("Debug: strmap released too many of string %p (%s)", Addr, Addr);
			memset( Addr, 0, ms->len );
			free(Addr);
			byName->Del(Addr);
		}
	}
}
bool strmap::Compare( const char *A, const char *B )
{
	if( A == B ) return true;
	if( !A || !B ) return false;
	return ( Read((char*)A) == Read((char*)B) );
}

ptrmap *GP = NULL;

Pointer::Pointer(sint32_t itype, void *iptr)
{
	type = itype;
	addr = iptr;
	name = NULL;
	sz = 0;
	count = 1;
}

Pointer::~Pointer()
{
	GS->Free(this->name);
}
void Pointer::SetName(const char *newname)
{
	GS->Free(this->name);
	this->name = GS->Copy(newname);
	return;//true
}

/*
Pointer *Pointer::Get(const char *varname)
{
	classdef *c = LookupClass(type);
	vardef *v = LookupVar(c,varname);
	if( !c || !v ) return NULL;
	void *localaddr = (void*)( (char*)this->addr + v->offset );
	Pointer *pchild = GP->Save(localaddr, v->type, varname);
	return pchild;
}
*/


ptrmap::ptrmap()
{
//	byName = new NList(pow(36,3), 128);
//	byAddr = new NList(pow(2,sizeof(void*)), 256);
	byName = new SMap(128);
	byAddr = new DMap(128);
}

ptrmap::~ptrmap()
{
	//! clean up pointers

	delete byName; byName=NULL;
	delete byAddr; byAddr=NULL;
}
uint32_t ptrmap::addrkey(void*ptr)
{
	uint32_t x = (uint32_t)( (uint64_t)ptr % (uint64_t)pow(2,sizeof(void*))/*byAddr->maxsize*/ );
	return x;
}
Pointer *ptrmap::Get( void *addr )
{
	if( !addr ) return NULL;
	Pointer *x = (Pointer*)byAddr->Get( this->addrkey(addr) );
	return x;
}
Pointer *ptrmap::Get( const char *name )
{
	if( !name ) return NULL;
	Pointer *x = (Pointer*)byName->Get( name );
	return x;
}
void ptrmap::Del( void *addr )
{
	Pointer *x = (Pointer*)byAddr->Get( this->addrkey(addr) );
	if( !x ) {
		//lprintf("ptrmap ptr not found");
		free(addr);
		return;
	}
	x->count--;
	if( x->count > 0 )
		return;

	if( x->name )
		byName->Del( x->name );
	byAddr->Del( this->addrkey(addr) );
	delete x;
	free(addr);
}

Pointer *ptrmap::Save( void *addr, sint32_t type, const char *name )
{
	Pointer *y, *yFound;

	yFound = Get(addr);
	if( yFound ) {
		yFound->count++;
		y = yFound;
	} else {
		y = new Pointer(type,addr);
		byAddr->Set( this->addrkey(addr), y );
	}

	if( name ) {
		byName->Set( name, y );
		y->SetName(name);
	}

	return y;
}

void *ptrmap::Alloc( int sz )
{
	void *mem = malloc(sz);
	memset(mem,0,sz);

	Pointer *x = Save(mem);
	x->sz = sz;
	return mem;
}

void *ptrmap::Realloc( void *oldptr, int newsz )
{
	Pointer *x = Save(oldptr);

	void *mem = malloc(newsz);
	if( newsz > x->sz )
		memset((char*)mem+x->sz,0,newsz-x->sz);
	memcpy(mem,oldptr,x->sz);

	Del(oldptr);

	x = Save(mem);
	x->sz = newsz;
	return mem;
}

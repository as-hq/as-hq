#include "sendao.h"

/*

NList::~NList()
{
	Clear();
}
NList::NList(uint32_t elems)
{
	this->granularity = elems;
	this->count = 0;
	this->dat = (tlist**)malloc(sizeof(tlist*)*granularity);
	memset( this->dat, 0, sizeof(tlist*)*granularity );
}
NList::NList(NList *src)
{
	this->granularity = src->granularity;
	this->dat = (tlist**)malloc(sizeof(tlist*)*granularity);
	memset( this->dat, 0, sizeof(tlist*)*granularity );
	CopyFrom( src );
}
void NList::CopyFrom( NList *src )
{
	Clear();

	uint32_t i;
	void *p;
	tlist *src2;
	tnode *n;
	ivoid *px;

	for( i=0; i<granularity; i++ ) {
		src2 = src->dat[i];
		forTLIST( px, n, src2, ivoid* ) {
			Set( px->idc, px->ptr );
		}
	}
	count = src->count;
}
void NList::Clear( voidFunction *vf )
{
	uint32_t i;

	for( i=0; i<granularity; i++ ) {
		if( dat[i] == NULL ) continue;
		dat[i]->Clear(vf);
		delete dat[i];
		dat[i] = NULL;
	}
	count=0;
}

tlist *NList::Tolist( void )
{
	tlist *output = new tlist;
	uint32_t i;
	tlist *src;
	tnode *n;
	ivoid *px;

	for( i=0; i<granularity; i++ ) {
		src = dat[i];
		forTLIST( px, n, src, ivoid* ) {
			output->PushBack(px->ptr);
		}
	}
	return output;
}
void NList::Foreach( void cb(void *) )
{
	uint32_t i;
	tlist *src;
	tnode *n;
	ivoid *px;

	for( i=0; i<granularity; i++ ) {
		src = dat[i];
		forTLIST( px, n, src, ivoid* ) {
			cb(px->ptr);
		}
	}
}
bool NList::Has( uint32_t idc )
{
	tlist *src = dat[ idc % granularity ];
	ivoid *px;
	tnode *n;

	forTLIST( px, n, src, ivoid* ) {
		if( px->idc >= idc ) {
			if( px->idc == idc ) return true;
			break;
		}
	}
	return false;
}
void *NList::Get( uint32_t idc )
{
	tlist *src = dat[ idc % granularity ];
	ivoid *px;
	tnode *n;

	forTLIST( px, n, src, ivoid* ) {
		if( px->idc >= idc ) {
			if( px->idc == idc ) return px->ptr;
			return NULL;
		}
	}
	return NULL;
}
uint32_t NList::Add( uint32_t idc, void *v )
{
	while( Get(idc) ) idc++;
	Set(idc,v);
	return idc;
}
void NList::Set( uint32_t idc, void *v )
{
	tlist *src = dat[ idc % granularity ];
	if( !src ) {
		src = dat[idc%granularity] = new tlist;
		src->Push( (void*)new_ivoid(idc,v) );
		return;
	}
	ivoid *px;
	tnode *n;

	forTLIST( px, n, src, ivoid* ) {
		if( px->idc == idc ) {
			px->ptr = v;
			return;
		} else if( px->idc > idc ) {
			src->InsertBefore(n,new_ivoid(idc,v));
			count++;
			return;
		}
	}
	src->PushBack((void*)new_ivoid(idc,v));
	count++;
}
void *NList::Del( uint32_t idc )
{
	tlist *src = dat[ idc % granularity ];
	ivoid *px;
	tnode *n;
	void *v;

	forTLIST( px, n, src, ivoid* ) {
		if( px->idc >= idc ) {
			if( px->idc >  idc ) return NULL;
			src->Pull( px );
			v = px->ptr;
			free(px);
			count--;
			return v;
		}
	}
	return NULL;
}
uint32_t NList::strkey( const char *str )
{
	uint32_t key0=0;
	int i, len;

	len = strlen(str);
	if( len > 3 ) len = 3;

	for( i=0; i<len; i++ ) {
		key0 *= 36;
		key0 += NList::charkey( str[i] );
	}

	return key0;
}

uint32_t NList::charkey( char c )
{
	uint32_t a;
	if(( c >= 'a' && c <= 'z' ) || ( c >= 'A' && c <= 'Z' )){
		a = 1 + (LOWER(c) - 'a');
	} else if((c >= '0') && (c <= '9')) {
		a = 27 + (c - '0');
	} else {
		a = 38;
	}
	return a;
}

*/

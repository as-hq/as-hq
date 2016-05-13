#include "sendao.h"

/*
int charkey( const char c )
{
	int a;
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



HTable::HTable( uint32_t kmax )
{
	tab = (tlist**)malloc( kmax * sizeof(tlist*) );
	for( keymax = 0; keymax < kmax; keymax ++ ) {
		tab[keymax] = NULL;
	}
	usedkeys = 0;
}
HTable::~HTable()
{
	Clear();
	free(tab);
}

void HTable::Clear( voidFunction *vF )
{
	uint32_t i;

	for( i = 0; i < keymax; i++ ) {
		if( tab[i] ) {
			tab[i]->Clear(vF);
			delete tab[i];
			tab[i] = NULL;
		}
	}
	usedkeys = 0;

}

void HTable::CopyFrom( HTable *x, keyFunction *vK, cloneFunction *vC )
{
	tnode *n;
	void *p, *px;
	uint32_t i;
	tlist *y;

	for( i = 0; i < x->keymax; i++ ) {
		if( !x->tab[i] ) continue;
		y = x->tab[i];
		forTLIST( p, n, y, void* ) {
			px = vC(p);
			i = vK(px);
			Add(i,px);
		}
	}
}

void HTable::CopyFrom( tlist *x, keyFunction *vK, cloneFunction *vC )
{
	tnode *n;
	void *p, *px;
	uint32_t i;

	forTLIST( p, n, x, void* ) {
		px = vC(p);
		i = vK(px);
		Add(i,px);
	}
}

tlist *HTable::ToList( cloneFunction *vC, bool pointerClone )
{
	tlist *nx = new tlist;
	tnode *n;
	void *p;
	void *px;
	uint32_t i;

	for( i = 0; i < keymax; i++ ) {
		if( tab[i] ) {
			for( n = tab[i]->nodes; n; n = n->next ) {
				p = n->data;
				if( vC ) {
					nx->PushBack( vC(n->data) );
				} else if( pointerClone ) {
					px = malloc(sizeof(void*)); // basically we just dereference the pointer into a memory slot, so if it's an int, the int goes there. blah boring
					memcpy(px, &p, sizeof(void*));
					nx->PushBack(px);
				} else {
					nx->PushBack( n->data );
				}
			}
		}
	}

	return nx;
}


tlist *HTable::Search( uint32_t key )
{
	uint32_t ki = key % keymax;
	if( tab[ki] && tab[ki]->count == 0 ) {
		delete tab[ki];
		tab[ki]=NULL;
	}
	return tab[ki];
}
void HTable::Foreach( void cb(void *) )
{
	uint32_t i;
	tnode *n, *next;

	for( i=0; i<keymax; i++ ) {
		if( !tab[i] ) continue;
		for( n = tab[i]->nodes; n; n = next ) {
			next = n->next;
			cb( n->data );
		}
	}
}
void HTable::SetRange( uint32_t kmax )
{
	if( usedkeys != 0 ) {
		printf("SetRange on a non-empty HTable");
		Clear();
	}
	if(tab) free(tab);
	tab = (tlist**)malloc( kmax * sizeof(tlist*) );
	for( keymax = 0; keymax < kmax; keymax ++ ) {
		tab[keymax] = NULL;
	}
	usedkeys = 0;
}
void HTable::Remove( uint32_t key, void *ptr )
{
	uint32_t ki = key % keymax;
	tlist *l = tab[ki];
	if( !l ) return;
	tnode *n = l->Pull(ptr);
	delete n;
}
void HTable::Add( uint32_t key, void *ptr )
{
	uint32_t ki = key % keymax;
	tlist *l = tab[ki];
	if( !l ) {
		l = tab[ki] = new tlist();
		usedkeys++;
	}
	l->PushBack( ptr );
}
void HTable::AddList( uint32_t key, tlist *items )
{
	uint32_t ki = key % keymax;
	tlist *l = tab[ki];
	if( !l ) {
		l = tab[ki] = new tlist();
		usedkeys++;
	}
	l->Append(items);
}

/*

STable::STable( uint32_t kmax ) : HTable(kmax)
{
}
STable::~STable()
{
	Clear();
	free(tab); tab=NULL;
}

void STable::Clear()
{
	HTable::Clear();
}

void *STable::Get( const char *name )
{
	uint32_t key = NList::strkey(name), shortkey = (key % keymax);
	tlist *l = tab[shortkey];
	tnode *n;
	nvoid *nv;

	forTLIST( nv, n, l, nvoid* )
	{
		if( str_c_cmp(nv->name,name) == 0 ) {
			return nv->ptr;
		}
	}
	return NULL;
}
void STable::Set( const char *name, void *ptr )
{
	uint32_t key = NList::strkey(name), shortkey = (key % keymax);

	tlist *l = tab[shortkey];
	if( !l ) {
		l = tab[shortkey] = new tlist();
		usedkeys++;
	}

	nvoid *nv;
	tnode *n;

	forTLIST( nv, n, l, nvoid* )
	{
		if( str_c_cmp(nv->name, name) == 0 ) {
			nv->ptr = ptr;
			return;
		}
	}
	nv = (nvoid*)malloc(sizeof(nvoid));
	nv->name = str_dup(name);
	nv->ptr = ptr;
	l->PushBack( nv );
}


*/


#include "sendao.h"

tnode *node_(void *ptr, bool valid)
{
	tnode *n;
	
//	lprintf("New tnode for %p.", ptr);
	n = new tnode;
	n->data = ptr;
	n->valid = true;
	n->next = NULL;
	n->prev = NULL;
	
	return n;
}

tlist::tlist(void)
{
	nodes = NULL;
	last = NULL;
	iters = NULL;
	iter_count = 0;
	type = 0;
	count = 0;
}

tlist::tlist(int i)
{
	nodes = NULL;
	last = NULL;
	iters = NULL;
	iter_count = 0;
	type = i;
	count = 0;
}

tlist::tlist(tlist *src, cloneFunction *cf)
{
	tnode *n;
	void *p;

	tnode *nc;
	void *pc;

	nodes = NULL;
	last = NULL;
	iters = NULL;
	iter_count = 0;
	type = src->type;
	count = 0;

	for( n = src->nodes; n; n = n->next ) {
		p = n->data;
		if( cf )
			pc = cf(p);
		else
			pc = p;
		nc = node_(pc);
		Push(nc);
	}
}

tlist::tlist(tnode *vnode)
{
	nodes = NULL;
	last = NULL;
	iter_count = 0;
	count = 0;
	Push(vnode);
	return;
}
tlist::~tlist(void)
{
	if( count > 0 )
		this->Clear();
}
void tlist::Clear( voidFunction *vf )
{
	tnode *n, *next;

	for( n = nodes; n; n = next ) {
		if(vf)
			vf( n->data );
		next = n->next;
		n->prev = NULL;
		delete n;
	}
	nodes = NULL;
	last = NULL;
	count = 0;
}
void **tlist::ToArray(cloneFunction *cf)
{
	tnode *n;
	void **a = (void**)malloc( sizeof(void*) * count );
	void **p=a;

	for( n = nodes; n; n = n->next ) {
		if( cf )
			*p = cf(n->data);
		else
			*p = n->data;
		p++;
	}

	return a;
}
void tlist::ForEach( voidFunction *vf )
{
	tnode *n, *nn;

	for( n = nodes; n; n = nn ) {
		nn = n->next;
		vf( n->data );
	}
}
void tlist::Append( tlist *src, cloneFunction *cf )
{
	tnode *n;
	void *pc;

	for( n = src->nodes; n; n = n->next ) {
		if( cf )
			pc = cf(n->data);
		else
			pc = n->data;
		Push(node_(pc));
	}
}
void tlist::CleanEmpty( void )
{
	tnode *n, *n2;

	for( n = nodes; n; n = n2 ) {
		n2 = n->next;
		if( !n->data ) Pull(n);
	}
}
void tlist::And( tlist *src, voidFunction *clearEl )
{
	tnode *n, *n2, *m;

	for( n = nodes; n; n = n2 ) {
		n2 = n->next;
		m = src->Lookup( n->data );
		if( !m ) {
			if( clearEl )
				clearEl( n->data );
			Pull(n);
		}
	}
}
void tlist::Reverse(void)
{
	tnode *n, *pn, *nn;

	tnode *newhead = last;
	tnode *newlast = nodes;

	pn = nn = n = NULL;

	for( n = nodes; n; n = nn ) {
		nn = n->next;
		if( !pn ) {
			n->next = n->prev; // cyclical list? uhh oh well, support it :)
			n->prev = nn;
		} else {
			n->next = pn;
			n->prev = nn;
		}

		pn = n;
	}

	last = newlast;
	nodes = newhead;
}
void tlist::InsertAfter(tnode *spec, tnode *after)
{
	if( !spec )
		return;
	if( !after ) {
		Push(spec);
		return;
	}
	after->prev = spec;
	after->next = spec->next;
	if( after->next ) after->next->prev = after;
	spec->next = after;
	/*
	spec->prev = after;
	spec->next = after->next;
	after->next = spec;
	if( spec->next )
		spec->next->prev = spec;
	*/
	count++;
}
void tlist::InsertAfter(tnode *spec, void *after)
{
	InsertAfter( spec, node_(after) );
}
void tlist::InsertBefore(tnode *spec, tnode *before)
{
	if( !spec )
		return;
	if( !before ) {
		Push(spec);
		return;
	}
	before->prev = spec->prev;
	if( before->prev ) before->prev->next = before;
	before->next = spec;
	spec->prev = before;
	
	count++;
}
void tlist::InsertBefore(tnode *spec, void *before)
{
	InsertBefore( spec, node_(before) );
}
	
	
void tlist::Push(tnode *spec)
{
	if( nodes )
		nodes->prev = spec;
	
	spec->next = nodes;
	spec->prev = NULL;
	nodes = spec;
	
	if( !last )
		last = spec;
	count++;
}
void tlist::Push(void *spec)
{
	Push( node_(spec) );
}
void tlist::PushBack(tnode *spec)
{
	spec->prev = last;
	
	if( last ) {
		last->next = spec;
		spec->prev = last;
	} else {
		nodes = spec;
		spec->prev = NULL;
	}
	
	last = spec;
	count++;
}
void tlist::PushBack(void *spec)
{
	PushBack( node_(spec) );
}
tnode *tlist::Pop(void)
{
	tnode *n;
	
	if( !this || !nodes )
		return NULL;
	
	n = nodes;
	nodes = nodes->next;
	if( !nodes )
		last = NULL;

	count--;
	return n;
}
void *tlist::FullPop(void)
{
	tnode *n;
	void *dptr;
	
	if( !this || !nodes )
		return NULL;
	
	n = nodes;
	nodes = nodes->next;
	if( !nodes )
		last = NULL;
	
	dptr = n->data;
	delete n;
	
	count--;
	return dptr;
}
	
tnode *tlist::PopBack(void)
{
	tnode *n;
	
	if( !this || !last )
		return NULL;
	n = last;
	last = last->prev;
	if( !last )
		nodes = NULL;
	count--;
	return n;
}
void *tlist::FullPopBack(void)
{
	tnode *n;
	void *dptr;
	
	if( !this || !last )
		return NULL;
	
	n = last;
	last = last->prev;
	if( !last )
		nodes = NULL;
	
	dptr = n->data;
	delete n;
	
	count--;
	return dptr;
}

tnode *tlist::Find(int num)
{
	tnode *n;
	
	for( n = nodes; n; n = n->next ) {
		if( num == 0 )
			return n;
		num--;
	}
	return NULL;
}
void *tlist::FindData(int num)
{
	tnode *n;

	for( n = nodes; n; n = n->next ) {
		if( num == 0 )
			return n->data;
		num--;
	}
	return NULL;
}
	
	
tnode *tlist::Pull(char *spec)
{
	tnode *n, *nprev;
	
	if( !this )
		return NULL;
	
	nprev = NULL;
	for( n = nodes; n; n = n->next ) {
		if( str_cmp((char*)n->data, spec) == 0 ) {
			if( nprev )
				nprev->next = n->next;
			if( n == nodes )
				nodes = n->next;
			if( n == last )
				last = n->prev;
			if( n->next )
				n->next->prev = nprev;
			count--;
			return n;
		}
		nprev = n;
	}
	return NULL;
}
tnode *tlist::Pull(void *spec)
{
	tnode *n, *nprev, *nnext;
	
	if( !this )
		return NULL;
	
	nprev = NULL;
	for( n = nodes; n; n = nnext ) {
		nnext = n->next;
		if( n->data == spec || n == spec ) {
			if( nprev )
				nprev->next = n->next;
			if( n == nodes )
				nodes = n->next;
			if( n == last )
				last = n->prev;
			if( n->next )
				n->next->prev = nprev;
			count--;
			return n;
		}
		nprev = n;
	}
	return NULL;
}
void tlist::Pull(tnode *spec)
{
	tnode *np, *nn;
	int i;
	
	if( !spec || !this )
		return;
	
	nn = spec->next;
	np = spec->prev;
	if( nodes == spec ) {
		nodes = nn;
	}
	if( last == spec ) {
		last = np;
	}
	
	if( nn )
		nn->prev = np;
	if( np )
		np->next = nn;
	
	for( i = 0; i < iter_count; i++ ) {
		if( iters[i] == spec ) {
			iters[i] = nn;
		}
	}
	
	delete spec;
	count--;

	return;
}

tlist *tlist::Copy( cloneFunction *cf )
{
	if( !cf ) {
		return new tlist(this);
	}
	tnode *n;
	void *ptr;
	tlist *newlist = new tlist;

	for( n = nodes; n; n = n->next ) {
		ptr = cf(n->data);
		newlist->Push(ptr);
	}
	return newlist;
}
int tlist::Count( void )
{
	tnode *n;
	int count=0;
	
	if( !this || !nodes )
		return 0;
	
	for( n = nodes; n; n = n->next ) {
		count++;
	}
	this->count = count;
	return count;
}

tnode *tlist::Lookup( void *ptr )
{
	tnode *n;
	
	for( n = nodes; n; n = n->next ) {
		if( n->data == ptr )
			return n;
	}
	return NULL;
}

tnode **tlist::Start(void)
{
	tnode **iter;
	int i;
	
	if( iter_count == 0 ) {
		iters = (tnode**)malloc( 8 * sizeof(tnode*) );
	}
	for( i = 0; i < iter_count; i++ ) {
		if( iters[i] == NULL )
			break;
	}
	iter = &iters[i];
	if( i >= iter_count )
		iter_count++;
	
	*iter = nodes;
	return iter;
}
void tlist::Stop(tnode **iter)
{
	*iter = NULL;
}

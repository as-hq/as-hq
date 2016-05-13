#ifndef __SENDAO_LISTS_H
#define __SENDAO_LISTS_H

//! todo: HList ( HTable-backed NList/sparse list )


typedef class tlist			tlist;
typedef struct _tnode		tnode;
typedef struct _tnode		*titer;

typedef void voidFunction ( void * );
typedef void *cloneFunction	( void * );
typedef uint32_t keyFunction( void * );
typedef int cmpFunction ( void *, void * );
typedef void *reduceFunction ( void *, void * );




struct _tnode
{
	void *data;
	bool valid;
	tnode *next;
	tnode *prev;
};



class tlist
{
	public: // not protected: go ahead and use nodes/last, just update 'count', 'prev', and 'next'
	// Variables
	tnode *nodes;
	tnode *last;
	uint16_t count;
	signed long int type;

	protected:
	tnode **iters;
	uint16_t iter_count;

	public:
	// Functions
	tlist(tnode *vnode);
	tlist(tlist *src, cloneFunction *cf=NULL);
	tlist(int);
	tlist(void);
	virtual ~tlist(void);

	// info
	int Count(void); // updates 'count' variable in case you modify the list externally

	// adding nodes
	void Push(tnode *node);
	void Push(void *spec);
	void PushBack(tnode *node);
	void PushBack(void *spec);
	void InsertAfter(tnode *spec, tnode *after);
	void InsertAfter(tnode *spec, void *after);
	void InsertBefore(tnode *spec, tnode *after);
	void InsertBefore(tnode *spec, void *after);
	void Append(tlist*src, cloneFunction *af=NULL);

	// sorting
	void Reverse(void);

	// removing nodes
	void Clear( voidFunction *vf=NULL ); // remove all nodes
	void CleanEmpty( void ); // remove empty nodes
	tnode *Pop(void); // retrieve front node
	void *FullPop(void); // retrieve front data
	tnode *PopBack(void); // retrieve back node
	void *FullPopBack(void); // retrieve back data
	tnode *Pull(char *spec);
	tnode *Pull(void *);
	tnode *Pull(int n);
	void Pull(tnode *spec);
	void And(tlist *refine, voidFunction *vf=NULL);

	// copying
	tlist *Copy( cloneFunction *vf=NULL );
	void **ToArray( cloneFunction *cf=NULL );
	template<typename T> T *ToArray(T mapcopyfunc(void*)) {
		tnode *n;
		T *a = (T*)malloc( sizeof(T) * count );
		T *p=a;

		for( n = nodes; n; n = n->next ) {
			*p = mapcopyfunc(n->data);
			p++;
		}

		return a;
	};

	// tools
	template<typename T> T Reduce(T joinfunc(T init,void*next)) {
		tnode *n;
		T a = joinfunc(a,NULL);

		for( n = nodes; n; n = n->next ) {
			a=joinfunc(a,n->data);
		}

		return a;
	};
	void ForEach( voidFunction *vf );
	//void Sort( cmpFunction *if );

//searching
	tnode *Lookup( void *ptr );
	tnode *Find(int num);
	void *FindData(int num);

//io
	void Read( void );
	void Write( void );

// iteration
	titer *Start( void );
	void Stop( titer * );
};




// Utilities
/*
template<typename T> T *forArray( void **a, int len, T f(void*) )
{
	void **e = &a[len];
	void **p = a;
	T *buf = (T*)malloc( sizeof(T) * len );
	T *pBuf = buf;
	while( p != e ) {
		*pBuf = f(*p);
		p++;
	}
	return buf;
};*/
template<typename T> void withArray( T *a, int len, void f(T*) )
{
	T *p = a, *e = &a[len];
	while( p != e ) {
		f(p);
		p++;
	}
};
template<typename T> void withArray( T *a, int len, void f(int i, T*) )
{
	T *p = a, *e = &a[len];
	int j=0;
	while( p != e ) {
		f(j,p);
		p++;
		j++;
	}
};
template<typename T> void withArray( T *a, int off, int len, void f(int i, T*) )
{
	T *p = &a[off], *e = &a[len];
	int j=0;
	while( p != e ) {
		f(j,p);
		p++;
		j++;
	}
};
template<typename T> void withArray( T *a, int off, int len, void f(T*) )
{
	T *p = &a[off], *e = &a[len];
	int j=0;
	while( p != e ) {
		f(p);
		p++;
		j++;
	}
};
#define WITH_ARRAY( x, len, fx ) { \
	int _with_array_i = 0; \
	while( _with_array_i < (len) ) { \
		(fx)(_with_array_i,&((x)[_with_array_i])); \
		_with_array_i++; \
	} \
}
#define WITH_SUBARRAY( x, off, len, fx ) { \
	int _with_array_i = (off); \
	while( _with_array_i < (len) ) { \
		(fx)(_with_array_i,&((x)[_with_array_i])); \
		_with_array_i++; \
	} \
}
template<typename T, typename S> T *mapArray( S *a, int len, T f(S*) )
{
	//T *buf=..,*pBuf=buf; withArray<S>(a,len,T [](S p){ *pBuf = f(p); pBuf++; });
	S *p = a, *e = &a[len];
	T *buf = (T*)malloc( sizeof(T) * len ), *pBuf = buf;
	while( p != e ) {
		*pBuf = f(p);
		p++;
		pBuf++;
	}
	return buf;
};
/*
template<typename T> T reduceArray( void *a, int len, T f(T init, void *next) )
{
	void *e = &a[len];
	void *p = a;
	T x = NULL;
	while( p != e ) {
		x = f(x, p);
		p++;
	}
	return x;
};*/
template<typename T, typename S> T reduceArray( S *a, int len, T f(T init, S *next) )
{
	S *p = a, *e = &a[len];
	T x = (T)0;
	while( p != e ) {
		x = f(x, p);
		p++;
	}
	return x;
};



#define forLIST( var, listiter, list ) \
	for( (listiter) = (list)?(list)->nodes:NULL; (listiter); (listiter) = (listiter)->next ) \
		if( ( (void *)(var) = (void *)((listiter)->data) ) != NULL )
#define forPLIST( var, listiter, list ) \
	for( (listiter) = (list)?(list)->nodes:NULL; (listiter); (listiter) = (listiter)->next ) \
		if( ( (var) = (void *)((listiter)->data) ) != NULL )
#define forTLIST( var, listiter, list, type ) \
	for( (listiter) = (list)!=NULL?(list)->nodes:NULL; (listiter)!=NULL; (listiter) = (listiter)->next ) \
		if( ( (var) = (type)((listiter)->data) ) != NULL )

#define forTSLIST( var, listiter, list, type, listnext ) \
	for( (listiter) = (list)?(list)->nodes:NULL; (listiter); (listiter) = (listnext) ) \
		if( ( ((listnext) = (listiter)->next) || (!(listnext)) ) && \
			( ((var) = (type)((listiter)->data) ) != NULL ) )

#define forSLIST( var, listiter, list, listnext ) \
	for( (listiter) = (list)?(list)->nodes:NULL; (listiter); (listiter) = (listnext) ) \
		if( ( ((listnext) = (listiter)->next) || (!(listnext)) ) && \
			( ((void *)(var) = (void *)((listiter)->data)) != NULL ) )

#define forILIST( var, listnode, list, listiter ) \
	for( (listiter) = (list)?(list)->Start():NULL, (listnode) = (listiter)?*(listiter):NULL; \
				((listiter) && *(listiter)) || ((*(listiter)=NULL)!=NULL) ; \
				(*(listiter)) = (*(listiter))?(*(listiter))->next:NULL, (listnode) = *(listiter) ) \
		if( ( (void *)(var) = (void *)((listnode)->data) ) != NULL )

tnode *node_(void *ptr,bool valid=true);


#endif

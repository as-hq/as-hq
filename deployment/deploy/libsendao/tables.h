#ifndef __SENDAO_TABLES_H
#define __SENDAO_TABLES_H

uint32_t key3d( int x, int y, int z, int mx, int my, int mz );


class HTable {
	public:
//	static sint32_t Register();
//	static sint32_t registryid;
	HTable(uint32_t kmax);
	virtual ~HTable();
	void Clear( void );
	void Clear( voidFunction *vC );
	void CopyFrom( HTable *src, keyFunction *vK, cloneFunction *vC );
	void CopyFrom( tlist *src, keyFunction *vK, cloneFunction *vC );
	virtual tlist *ToList( cloneFunction *vC=NULL, bool pointerClone=false );

	tlist *Search(uint32_t key);

	void Remove(uint32_t key, void *ptr);
	void Add(uint32_t key, void *ptr);
	void AddList( uint32_t key, tlist *items );
	void SetRange(uint32_t kmax);

	void Foreach( void cb(void*) );

	public:
	tlist **tab;
	uint32_t keymax;
	uint32_t usedkeys;
};

#define forHTABLE( var, listiter, listctr, tbl, type ) \
	for( (listctr) = 0; (listctr) < (tbl)->keymax; (listctr)++ ) \
		if( (tbl)->tab[(listctr)] ) \
			for( (listiter) = (tbl)->tab[(listctr)]->nodes; (listiter); (listiter) = (listiter)->next ) \
				if( ( (var) = (type)((listiter)->data) ) == NULL )

#endif

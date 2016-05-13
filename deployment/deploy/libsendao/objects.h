#ifndef __SENDAO_OBJECTS_H
#define __SENDAO_OBJECTS_H


typedef struct _intvoid ivoid;
typedef struct _namevoid nvoid;
typedef struct _namekeyvoid nkvoid;
typedef struct _inttype itype;
typedef struct _nametype ntype;
typedef struct _namekeytype nktype;

struct _intvoid {
	uint32_t idc;
	void *ptr;
};
struct _namevoid {
	char *name;
	void *ptr;
};
struct _namekeyvoid {
	char *name;
	uint32_t idc;
	void *ptr;
};

struct _inttype {
	uint32_t idc;
	sint32_t type;
	void *ptr;
};
struct _nametype {
	char *name;
	sint32_t type;
	void *ptr;
};
struct _namekeytype {
	char *name;
	uint32_t idc;
	sint32_t type;
	void *ptr;
};


ivoid *new_ivoid(uint32_t idc=0, void *ptr=NULL);
nvoid *new_nvoid(const char *name=NULL, void *ptr=NULL);
nkvoid *new_nkvoid(const char *name=NULL, uint32_t idc=0, void *ptr=NULL);
void free_ivoid(ivoid *);
void free_nvoid(nvoid *);
void free_nkvoid(nkvoid *);

void *clone_ivoid( void *iv );
void *clone_nvoid( void *nv );
void *clone_nkvoid( void *nkv );

itype *new_itype(uint32_t idc=0, void *ptr=NULL, sint32_t type=0);
ntype *new_ntype(const char *name=NULL, void *ptr=NULL, sint32_t type=0);
nktype *new_nktype(const char *name=NULL, uint32_t idc=0, void *ptr=NULL, sint32_t type=0);

uint32_t keyof_ivoid(void *x);
uint32_t keyof_itype(void *x);
uint32_t keyof_nvoid(void *x);
uint32_t keyof_ntype(void *x);
uint32_t keyof_nkvoid(void *x);
uint32_t keyof_nktype(void *x);

void free_itype(itype *);
void free_ntype(ntype *);
void free_nktype(nktype *);

void *clone_itype( void *iv );
void *clone_ntype( void *nv );
void *clone_nktype( void *nkv );

uint32_t charkey( char c );
uint32_t namekey( const char *str );



template<typename T>T o_cast( void *ptr )
{
	return (T)ptr;
};

#endif

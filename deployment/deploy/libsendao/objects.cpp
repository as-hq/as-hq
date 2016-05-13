#include "sendao.h"


ivoid *new_ivoid(uint32_t idc, void *ptr)
{
	ivoid *p = (ivoid*)malloc(sizeof(ivoid));
	p->idc = idc;
	p->ptr = ptr;
	return p;
}
nvoid *new_nvoid(const char *name, void *ptr)
{
	nvoid *p = (nvoid*)malloc(sizeof(nvoid));
	p->name = name ? str_dup(name) : NULL;
	p->ptr = ptr;
	return p;
}

nkvoid *new_nkvoid(const char *name, uint32_t idc, void *ptr)
{
	nkvoid *p = (nkvoid*)malloc(sizeof(nkvoid));
	p->name = name ? str_dup(name) : NULL;
	p->idc = idc;
	p->ptr = ptr;
	return p;
}

void free_ivoid(ivoid *x)
{
	free(x);
}

void free_nvoid(nvoid *x)
{
	if( x->name )
		free(x->name);
	free(x);
}

void free_nkvoid(nkvoid *x)
{
	if( x->name )
		free(x->name);
	free(x);
}


itype *new_itype(uint32_t idc, void *ptr, sint32_t type)
{
	itype *p = (itype*)malloc(sizeof(itype));
	p->idc = idc;
	p->ptr = ptr;
	p->type = type;
	return p;
}
ntype *new_ntype(const char *name, void *ptr, sint32_t type)
{
	ntype *p = (ntype*)malloc(sizeof(ntype));
	p->name = name ? str_dup(name) : NULL;
	p->ptr = ptr;
	p->type = type;
	return p;
}

nktype *new_nktype(const char *name, uint32_t idc, void *ptr, sint32_t type)
{
	nktype *p = (nktype*)malloc(sizeof(nktype));
	p->name = name ? str_dup(name) : NULL;
	p->idc = idc;
	p->ptr = ptr;
	p->type = type;
	return p;
}

uint32_t keyof_ivoid(void *x)
{
	ivoid *y = (ivoid*)x;
	return y->idc;
}
uint32_t keyof_itype(void *x)
{
	itype *y = (itype*)x;
	return y->idc;
}
uint32_t keyof_nvoid(void *x)
{
	nvoid *y = (nvoid*)x;
	return namekey(y->name);
}
uint32_t keyof_ntype(void *x)
{
	ntype *y = (ntype*)x;
	return namekey(y->name);
}
uint32_t keyof_nkvoid(void *x)
{
	nkvoid *y = (nkvoid*)x;
	return y->idc;
}
uint32_t keyof_nktype(void *x)
{
	nktype *y = (nktype*)x;
	return y->idc;
}

void free_itype(itype *x)
{
	free(x);
}

void free_ntype(ntype *x)
{
	if( x->name )
		free(x->name);
	free(x);
}

void free_nktype(nktype *x)
{
	if( x->name )
		free(x->name);
	free(x);
}


void *clone_ivoid( void *x )
{
	ivoid *src = (ivoid*)x;
	return new_ivoid( src->idc, src->ptr );
}

void *clone_nvoid( void *x )
{
	nvoid *src = (nvoid*)x;
	return new_nvoid( src->name, src->ptr );
}

void *clone_nkvoid( void *x )
{
	nkvoid *src = (nkvoid*)x;
	return new_nkvoid( src->name, src->idc, src->ptr );
}


void *clone_itype( void *x )
{
	itype *src = (itype*)x;
	return new_itype( src->idc, src->ptr, src->type );
}

void *clone_ntype( void *x )
{
	ntype *src = (ntype*)x;
	return new_ntype( src->name, src->ptr, src->type );
}

void *clone_nktype( void *x )
{
	nktype *src = (nktype*)x;
	return new_nktype( src->name, src->idc, src->ptr, src->type );
}


uint32_t namekey( const char *str )
{
	uint32_t key0=0;
	int i, len;

	len = strlen(str);
	if( len > 3 ) len = 3;

	for( i=0; i<len; i++ ) {
		key0 *= 36;
		key0 += charkey( str[i] );
	}

	return key0;
}

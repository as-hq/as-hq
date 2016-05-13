#include "sendao.h"


tattribs::tattribs(void)
{
	data = new tarray( sizeof(tattrib), 4 );
}

tattribs::~tattribs(void)
{
	int i;
	tattrib *at;
	
	forTARRAY( at, i, data, tattrib* ) {
		free( at->name );
		if( at->mymemory ) {
			free( at->value );
		}
	}
	
	delete data;
}

int tattribs::nameindex( const char *name )
{
	int idx;
	
	if( name[0] == '\0' ) idx = 0;
	else if( name[1] == '\0' ) idx = (int)name[0];
	else idx = (int)name[0] | (int)(name[1]<<8); // just an alpha sort atm

	return idx;
}

tattrib *tattribs::locate( const char *name, int idx, int *rvi )
{
	tattrib *at;
	int i;
	
	if( idx == -1 )
		idx = nameindex(name);
	
	forTARRAY( at, i, data, tattrib* ) {
		if( at->index == idx && strcmp(at->name, name) == 0 ) {
			if( rvi )
				*rvi = i;
			return at;
		}
	}
	
	if( rvi )
		*rvi = -1;
	
	return NULL;
}

void *tattribs::get( const char *name )
{
	tattrib *at = locate(name, -1, NULL);
	
	printf("get(%s)=%p\n", name, at->value);
	
	if( !at )
		return NULL;
	
	return at->value;
}

tattrib *tattribs::set( const char *name, void *value )
{
	int idx = nameindex( name );
	tattrib *at = locate(name, idx);
	
	if( at ) {
		if( at->mymemory )
			free( at->value );
		at->value = value;
		
		return at;
	}
	
	at = (tattrib*)data->alloc();
	at->name = str_dup(name);
	at->value = value;
	at->index = idx;
	at->mymemory = false;
	printf("Set %s:%p\n", name, value);
	
	return at;
}

void tattribs::set( const char *name, int value )
{
	void *v;
	tattrib *at;
	
	v = malloc( sizeof(int) );
	*(int *)v = value;
	
	at = set( name, v );
	at->mymemory = true;
}

void tattribs::set( const char *name, char *value )
{
	void *v;
	tattrib *at;
	
	v = malloc( sizeof(char) * (1+strlen(value)) );
	strcpy( (char*)v, value );
	
	at = set( name, v );
	at->mymemory = true;
}

bool tattribs::remove( const char *name )
{
	int rv, idx;
	tattrib *at;
	
	
	idx = nameindex( name );
	at = locate( name, idx, &rv );
	
	if( at ) {
		data->remove( rv );
		return true;
	}
	
	return false;
}

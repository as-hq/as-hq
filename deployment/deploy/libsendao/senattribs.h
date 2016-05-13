#ifndef _SENDAO_ATTRIBS_H
#define _SENDAO_ATTRIBS_H

typedef struct _tattrib			tattrib;

/*

What we need:

Set/remove/check values in a dataset
Fast search/looking for checking


*/

class tattribs {
	public:
	tarray *data; // see struct tattrib
	
	public:
	tattribs();
	~tattribs();
	void *get( const char *name );
	tattrib *set( const char *name, void *value );
	void set( const char *name, int value );
	void set( const char *name, char *value );
	bool remove( const char *name );
	
	private:
	tattrib *locate( const char *name, int idx=1, int *rvi=NULL );
	int nameindex( const char *name );
};

struct _tattrib {
	int index; // quicker searching
	char *name;
	void *value;
	bool mymemory; //! replace this with more flags if needed
};


#endif

#include "sendao.h"

void tsInit( timestop_v *x )
{
#ifdef USE_WINDOWS
	InitializeCriticalSection( x );
#else
	pthread_mutexattr_t mAttr;

	pthread_mutexattr_settype( &mAttr, PTHREAD_MUTEX_RECURSIVE_NP );

	pthread_mutex_init(x, &mAttr);
	pthread_mutexattr_destroy(&mAttr);
#endif
}

void tsEnter( timestop_v *x )
{
#ifdef USE_WINDOWS
	EnterCriticalSection(x);
#else
	pthread_mutex_lock(x);
#endif
}

void tsExit( timestop_v *x )
{
#ifdef USE_WINDOWS
	LeaveCriticalSection(x);
#else
	pthread_mutex_unlock(x);
#endif
}

void tsDelete( timestop_v *x )
{
#ifdef USE_WINDOWS
	DeleteCriticalSection(x);
#else
	pthread_mutex_destroy(x);
#endif
}

thread_v tsGetCurrentThreadId(void)
{
#ifdef USE_WINDOWS
	return GetCurrentThreadId();
#else
	return pthread_self();
#endif
}

/*
thread_v tsSplit()
{
	thread_v threadid;

#ifdef USE_WINDOWS
	HANDLE hThread;

	hThread = CreateThread( NULL, 0, splithook, (void*)wu, 0, &threadid);
#else
	pthread_create(&threadid, NULL, splithook, (void*)wu);
#endif

	return threadid;
}
*/


/*
void UnloadLibrary( const char *filename )
{
	Module *m;
	int j;
	char *newname;

	j= str_str(filename, ".")-filename;
	newname = strndup(filename, j);
	j = strlen(newname);

	for( m = modules; m; m = m->next ) {
		if( (unsigned int)j == strlen(m->name) && str_cmp(newname,m->name) ) {
			//! unload library
			//! free library
			lprintf("Unloaded library (not really)");
			return;
		}
	}
}
void ReloadLibrary( const char *filename )
{
	UnloadLibrary(filename);
	AddLibrary(filename);
}

void AddLibrary( const char *filename )
{
	Module *m;
	int j;


#ifdef USE_WINDOWS
	HINSTANCE h;

	h = LoadLibrary(filename);
#else
	void *h;

	h = dlopen(filename, RTLD_LAZY);
#endif
	if( !h ) {
		lprintf("Couldn't load module '%s'", filename);
		return;
	}
	m = (Module*)calloc(sizeof(Module),1);
#ifdef USE_WINDOWS
	m->start = (_startupfunc*)GetProcAddress(h,"startup");
#else
	m->start = (_startupfunc*)dlsym(h,"startup");
#endif
	m->dir = new_dir();
	j=str_str(filename, ".")-filename;
	m->name = strndup(filename, j);
	m->next = modules;
	modules = m;

	lprintf("Module '%s' loaded, startup offset %p.", m->name, m->start);
	m->start(m);
//	lprintf("Module started.");
}
*/

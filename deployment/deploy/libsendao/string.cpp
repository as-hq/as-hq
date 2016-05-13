#include "sendao.h"

void strip_newline( char *str )
{
	char *pFrom, *pTo;

	for( pFrom=str, pTo=str; *pFrom; pFrom++ ) {
		if( *pFrom == '\n' || *pFrom == '\r' )
			continue;
		if( pFrom != pTo )
			*pTo = *pFrom;
		pTo++;
	}
	*pTo = '\0';
}

bool is_number(const char *str)
{
    const char *pStr;

    if( !(pStr=str) ) return FALSE;
	if( !*str ) return FALSE;
    if( *str == '-' )
        pStr++;

    while( *pStr )
        if( !isdigit(*pStr++) )
            return FALSE;
    return TRUE;
}
bool is_numeric(const char *c)
{
	return is_number(c);
}

bool strprefix( const char *longstr, const char *shortstr )
{
	const char *lp, *sp;

	for( lp = longstr, sp = shortstr; *sp; sp++, lp++ ) {
		if( *lp != *sp )
			return false;
	}
	return true;
}

void strexpand( char **buf, const char *add )
{
	char *tptr;

	if( !buf )
		return;

	if( !*buf ) {
		tptr = (char*)malloc( strlen(add) + 1 );
		strcpy(tptr, add);
	} else if( !**buf ) {
		tptr = (char*)malloc( strlen(add) + 1 );
		strcpy(tptr, add);
		free(*buf);
	} else {
		tptr = (char*)malloc( strlen(*buf) + strlen(add) + 1 );
		strcpy(tptr, *buf);
		strcat(tptr, add);
		free(*buf);
	}

	*buf = tptr;
}

void Arg(const char *args, int n, char *argn)
{
	const char *pArgs;
	char *pArgn;
	int i;
	bool bRemainder=false;

	if( n < 0 ) {
		bRemainder = true;
		n = (0 - n) + 1;
	}

	// Eliminate leading whitespace
	for( pArgs = args; isspace(*pArgs); pArgs++ );

	// Jump to correct word
	for( i=1; i<n && *pArgs; pArgs++  ) {
		if( isspace(*pArgs) )
			i++;
	}

	// Do remainders' write to argn
	if( bRemainder ) {
		if( !*pArgs )
			*argn = '\0';
		else
			strcpy(argn, pArgs);
		return;
	}

	// Do standard write to argn
	for( pArgn=argn; *pArgs && !isspace(*pArgs); pArgs++, pArgn++ ) {
		*pArgn = *pArgs;
	}
	*pArgn = '\0';

	return;
}


void strcpysafe( char *dst, char *src )
{
	char *pB=dst, *pS=src;
	while( *pS ) {
		*pB = *pS;
		++pS;
		++pB;
	}
	*pB = '\0';
}
char *str_dup( const char *p )
{
	char *px = (char*)malloc( strlen(p)+1 );
	strcpy(px,p);
	return px;
}


char *strndup(const char *str, int len)
{
	char *x=strncpy((char*)calloc(len+1, 1), str, len);
	x[len]=0;
	return x;
}


int str_c_cmp( const char *a, const char *b )
{
	const char *ax,*bx;
	char c,d;
	for( ax=a,bx=b; *ax || *bx; ax++, bx++ )
	{
		c = LOWER(*ax);
		d = LOWER(*bx);
		if( c != d ) return -1;
	}
	return 0;
}

int str_cmp( const char *a, const char *b )
{
	const char *ax,*bx;

	for( ax=a,bx=b; *ax && *bx; ax++, bx++ )
	{
		if( *ax != *bx ) return -1;
	}
	return 0;
}

const char *str_r_str(const char *src, const char *search)
{
	const char *index;
	const char *srcA, *srcB;

	for( index=src+strlen(src); index>=src; index-- ) {
		if( *index == *search ) {
			srcA=index;
			srcB=search;
			while( *srcA == *srcB ) {
				srcA++; srcB++;
				if( !*srcB ) return index;
			}
		}
	}
	return NULL;
}
const char *str_str(const char *src, const char *search)
{
	const char *index;
	const char *srcA, *srcB;

	for( index=src; *index; index++ ) {
		if( *index == *search ) {
			srcA=index;
			srcB=search;
			while( *srcA == *srcB ) {
				srcA++; srcB++;
				if( !*srcB ) return index;
			}
		}
	}
	return NULL;
}
const char *str_case_str(const char *src, const char *search)
{
	const char *index;
	const char *srcA, *srcB;

	for( index=src; *index; index++ ) {
		if( LOWER(*index) == LOWER(*search) ) {
			srcA=index;
			srcB=search;
			while( *srcA == *srcB ) {
				srcA++; srcB++;
				if( !*srcB ) return index;
			}
		}
	}
	return NULL;
}
const char *str_exp(const char *src, const char *search)
{
	const char *index;
	const char *srcA, *srcB;

	if( search[strlen(search)-1] == '@' ) { // tail end
		srcA=src+strlen(src)-strlen(search);
		srcB=search;
		while( LOWER(*srcA) == LOWER(*srcB) ) {
			srcA++; srcB++;
			if( *srcB ) return src+strlen(src)-strlen(search);
		}
		return NULL;
	}

	for( index=src; *index; index++ ) {
		if( *index == *search ) {
			srcA=index;
			srcB=search;
			while( *srcA == *srcB ) {
				srcA++; srcB++;
				if( !*srcB ) return index;
			}
		}
	}
	return NULL;
}

stringbuf::stringbuf(uint16_t chunk)
{
	len=0;
	alloc_chunk = chunk;
	alloced=0;
	maxlen=0;
	p = NULL;
}
stringbuf::stringbuf(const char *src, uint16_t chunk)
{
	len=strlen(src);
	while( maxlen!=0 && len > maxlen ) {
		len -= maxlen;
		src += maxlen;
	}
	alloc_chunk = chunk;
	alloced=0;
	while(alloced<=len)
		alloced += alloc_chunk;
	if( maxlen!=0 && alloced > maxlen )
		alloced=maxlen;
	p = (char*)calloc(alloced, 1);
	if( len > 0 )
		strcpy(p,src);
}

stringbuf::~stringbuf()
{
	free(p);
}

void stringbuf::reset(void)
{
	len=0;
	if(p)
		*p='\0';
}

void stringbuf::append(const char *src)
{
	uint16_t newlen = len+strlen(src);
	while(maxlen!=0 && newlen>=maxlen) { // maxlen buffers
		if( len > 0 ) {
			src += maxlen - len;
			len = 0;
		} else {
			src += maxlen;
		}
		newlen -= maxlen;
	}
	//bool new_string=(alloced==0);
	if( alloced <= newlen ) {
		char *tmp;
		while(alloced<=newlen)
			alloced += alloc_chunk;
		if( alloced >= maxlen )
			alloced=maxlen;
		tmp = (char*)calloc(alloced, 1);
		if( p ) {
			strcpy(tmp,p);
			strcat(tmp,src);
			free(p);
		} else {
			strcpy(tmp,src);
		}
		p=tmp;
	} else {
		strcat(p,src);
	}
	len=newlen;
}
void stringbuf::append(char c)
{
	uint16_t newlen = len+1;
	if( alloced <= newlen ) {
		char *tmp;
		while(alloced<=newlen)
			alloced += alloc_chunk;
		if( maxlen!=0 && alloced > maxlen )
			alloced = maxlen;
		tmp = p;
		p = (char*)calloc(alloced, 1);
		if( tmp ) {
			strcpy(p,tmp);
			free(tmp);
		}
		p[newlen-1]=c;
		p[newlen]=0;
	} else {
		p[newlen-1]=c;
		p[newlen]=0;
	}
	len=newlen;
}
void stringbuf::expand()
{
	alloced += alloc_chunk;
	if( maxlen!=0 && alloced > maxlen )
		alloced = maxlen;
	char *tmp = (char*)calloc(alloced, 1);
	if(p){
		strcpy(tmp,p);
		free(p);
	}
	p=tmp;
}
void stringbuf::printf(const char *src,...)
{
    char buf[204800];
    va_list args;

    va_start(args, src);
    vsprintf(buf, src, args);
    va_end(args);

	append(buf);
}
void stringbuf::clear(void)
{
	len=0;
	if(alloced>0)*p=0;
}

linebuf::linebuf(const char *src)
{
	lines=used=0;
	buffer=NULL;
	if( !src ) append("");
	else append(src);
}

void linebuf::setline( int lno, const char *str )
{
	while( lno >= lines ) expand();
	if( buffer[lno] ) free(buffer[lno]);
	buffer[lno] = strdup(str);
	if( lno >= used ) used = lno;
}

void linebuf::append( const char *str )
{
	tlist *lStr = split(str, "\n");

	while( used + lStr->count > lines ) {
		expand();
	}

	char *onestr;
	tnode *n;

	forTLIST( onestr, n, lStr, char* ) {
		addline(onestr);
	}
	lStr->Clear( free );
	delete lStr;
}

void linebuf::addline( const char *str )
{
	if( used + 4 > lines ) expand();

	buffer[used] = strdup(str);
	used++;
}

void linebuf::expand( void )
{
	char **tmp = buffer;
	lines += 128;
	buffer = (char**)malloc( sizeof(char*)*lines );
	if( tmp ) {
		memcpy( buffer, tmp, sizeof(char*)*used );
	} else used=0;
	memset( &buffer[used], 0, sizeof(char*)*(lines-used) );
	free(tmp);
}




bitbuf::bitbuf(uint16_t chunk)
{
	len=0;
	alloc_chunk = chunk;
	alloced=0;
	while(alloced<=len)
		alloced += alloc_chunk;
	p = (char*)calloc(alloced, 1);
}

bitbuf::~bitbuf()
{
	free(p);
}
void bitbuf::append(char *src, int nlen)
{
	uint16_t newlen = len+nlen;
	if( alloced <= newlen ) {
		char *tmp;
		while(alloced<=newlen)
			alloced += alloc_chunk;
		tmp = (char*)calloc(alloced, 1);
		memcpy(tmp,p,len);
		free(p);
		p=tmp;
	}
	memcpy(p+len,src,nlen);
	len=newlen;
	p[len]=0;
}
void bitbuf::clear(void)
{
	len=0;
}



tlist *split( const char *str, const char *sep )
{
	tlist *seps = new tlist();
	void *septr = (void*)str_dup(sep);
	seps->Push( septr );
	tlist *tgt = split(str,seps);
	seps->Clear( free );
	delete seps;
	return tgt;
}

tlist *split( const char *str, tlist *seps )
{
	tlist *tgt = new tlist();
	const char *ptr, *lastPtr;
	tnode *n;
	const char *sep;
	char wordBuf[1024];
	int len,ilen;

	for( lastPtr = ptr = str; ptr && *ptr; ptr++ ) {
		ilen=strlen(ptr);
		forTLIST( sep, n, seps, const char* ) {
			len = strlen(sep);
			if( ilen>=len && strncmp(sep,ptr,len) == 0 ) {
				if( ptr != lastPtr ) {
					strncpy(wordBuf,lastPtr,ptr-lastPtr);
					wordBuf[ptr-lastPtr]=0;
					tgt->PushBack( (void*)str_dup(wordBuf) );
				}
				ptr += len;
				lastPtr = ptr;
				break;
			}
		}
		if( !*ptr ) break;
	}
	if( ptr > lastPtr ) {
		strncpy(wordBuf,lastPtr,ptr-lastPtr);
		wordBuf[ptr-lastPtr]=0;
		tgt->PushBack( (void*)str_dup(wordBuf) );
	}
	return tgt;
}


#include "sendao.h"

bool random_initted=false;

const char *read_to_letter(FILE *fp, char letter, bool bLines)
{
    static char inp[4096];
    char *pC, c;
    int len;
    bool bRead=TRUE;

    pC = inp;
    len=0;
    while( 1 )
    {
        c = fgetc(fp);

        if( feof(fp) )
            break;

        if( c == '\r' )
            continue;

        if( pC == inp )
        {
            if( ( c == '\n' ) || ( c == ' ' ) || ( c == '\t' ) || ( c == '\3' ) || ( c == letter ) ||
                ( ( letter == 0 ) && ( c != '-' ) && ( ( c < '0' ) || ( c > '9' ) ) ) )
                continue;
        }

        if( ( ( !bLines ) && ( c == '\n' ) ) ||
            ( ( letter == 0 ) && ( c != '-' ) && ( ( c < '0' ) || ( c > '9' ) ) ) ||
            ( c == letter ) || ( ( letter == ' ' ) && ( c == '\t' ) ) )
        {
            ungetc(c, fp);
            break;
        }
        if( len++ > 4096 - 1 )
            bRead=FALSE;

        if( bRead==TRUE )
        {
            *pC=c;
            pC++;
        }
    }
    *pC='\0';

    return inp;
}
char *readwholefile(const char *fn)
{
	FILE *fp = fopen(fn,"r");
	if(!fp) return NULL;
	char *ptr = readwholefile(fp);
	fclose(fp);
	return ptr;
}
char *readwholefile(FILE *fp)
{
	char buf[1025], *ptr=NULL;
	int n;

	do {
		*buf = '\0';
		n = fread(buf, 1, 1024, fp);
		if( n <= 0 )
			break;
		buf[n] = '\0';
		
		strexpand( &ptr, buf );
	} while ( n >= 1024 );
	
	return ptr;	
}

const char *getstaticword(FILE *fp)
{
	static char tmp[4096];

    strcpy(tmp, read_to_letter(fp, ' ', FALSE));
	return tmp;
}
const char *getstaticline(FILE *fp)
{
	static char tmp[4096];

    strcpy(tmp, read_to_letter(fp, '\n', FALSE));
	return tmp;
}
char *getword(FILE *fp)
{
    return str_dup(read_to_letter(fp, ' ', FALSE));
}
char *getline(FILE *fp)
{
    return str_dup(read_to_letter(fp, '\n', FALSE));
}
char *getstring(FILE *fp)
{
    return str_dup(read_to_letter(fp, '\3', TRUE));
}
int getnumber(FILE *fp)
{
	const char *buf;

	buf = read_to_letter(fp, 0, FALSE);
	if( *buf == '-' )
		return 0 - atoi(buf+1);
	return atoi(buf);
}

void mkfulldir(char *str)
{
    char buf[4096], *pBuf=buf, *ptr=str;

    while( true ) {
    	if( *ptr == '/' || !*ptr )
        {
            *pBuf = '\0';
            #ifdef WIN32
            mkdir(buf);
            #endif
			#ifdef LINUX
            mkdir(buf, 0700);
            #endif
            if( !*ptr ) break;
        }
        *pBuf++ = *ptr++;
    }
}

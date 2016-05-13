#include "sendao.h"



char *readFile( const char *fileName )
{
	stringbuf *sb = new stringbuf();
	char buf[1024];

	FILE *fp = fopen(fileName, "r");
	if( !fp ) {
		printf("Failed to open %s for read", fileName);
		return NULL;
	}
	while( fgets( buf, 1024, fp ) ) {
		sb->append(buf);
		sb->append("\n");
	}
	char *result = strdup(sb->p);
	delete sb;
	return result;
}


uint32_t charkey( char c )
{
	uint32_t a;
	if(( c >= 'a' && c <= 'z' ) || ( c >= 'A' && c <= 'Z' )){
		a = 1 + (LOWER(c) - 'a');
	} else if((c >= '0') && (c <= '9')) {
		a = 27 + (c - '0');
	} else {
		a = 38;
	}
	return a;
}



int strkey( const char *str )
{
	uint32_t v=0, j, c;

	for( j=0; str[j] && j < 6; j++ ) {
		c=charkey(str[j]);
		v = v*20 + c;
	}
	return v;
}

// Stolen from /usr/src/linux/lib/crc32.c
// Note: it's stolen because this is kernel code, but it'll work fine in GOD.
#define CRCPOLY_LE 0xedb88320
u32 crc32(const char *pin)
{
	int i;
	u32 crc=0;
	size_t len = strlen(pin);
	unsigned const char *p = (unsigned const char *)pin;

	while (len--) {
		crc ^= *p++;
		for (i = 0; i < 8; i++)
			crc = (crc >> 1) ^ ((crc & 1) ? CRCPOLY_LE : 0);
	}
	return crc;
}


int number_range(int low, int high)
{
    if( random_initted == false ) {
        random_initted=true;
#ifdef WIN32
        srand(clock());
#endif
#ifdef LINUX
        srandom(time(NULL));
#endif
    }
#ifdef WIN32
    return rand() % ((high+1)-low) + low;
#endif
#ifdef LINUX
    return random() % ((high+1)-low) + low;
#endif
}



int rrand(int low, int high)
{
	int val = low>=high?low : (low+(int)((float)(high-low)*rand()/(RAND_MAX+1.0)));
	
	return val;
}

float frand(float low, float high)
{
	float val = low>=high?low : low + (high-low) * rand() / (RAND_MAX+1.0);
	
	return val;
}

void memadd( char **pdest, char *psrc, int len )
{
	memcpy( *pdest, psrc, len );
	(*pdest) = (*pdest) + len;
}
void memaddstr( char **pdest, char *psrc )
{
	int len;
	
	len = strlen(psrc);
	memadd( pdest, (char*)&len, sizeof(int) );
	memadd( pdest, psrc, len );
}

void memsub( char **psrc, char *pdest, int len )
{
	memcpy( pdest, *psrc, len );
	(*psrc) = (*psrc) + len;
}

void memsubstr( char **psrc, char **pdest )
{
	int len;
	
	memsub(psrc, (char*)&len, sizeof(int));
	*pdest = (char*)malloc(len+1);
	memsub(psrc, *pdest, len);
	(*pdest)[len] = '\0';
}

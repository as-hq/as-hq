#include "sendao.h"

/*
Alright. The arrays must be expandable and contractable.
How do we definitively mark an entry as invalid without possibly hitting an existing value?
We could fill it with zeroes, but that's occasionally going to happen. However, what should
very rarely happen is an array entry filled with ones. Therefore, flag empty entries with
a full buffer! GO TEAM

Array sizes may vary, from a bit to a gigabyte. So we'll also shove an increment variable
into the array. We also need an increment variable for size allocation.

Now we need some functions to handle the array. Adding and removing from the array, along
with a method to handle longer appends, and creating the array. Also searching.
*/

tarray::tarray(int entry_size_bits, int alloc_blocks, int padding)
{
	this->entry_increment = entry_size_bits/8;
	this->alloc_increment = alloc_blocks;
	this->used = 0;
	this->allocated = 0;
	this->empty = 0;
	this->data = NULL;
}
tarray::tarray(int entry_size, int alloc_blocks)
{
	this->entry_increment = entry_size;
	this->alloc_increment = alloc_blocks;
	this->used = 0;
	this->allocated = 0;
	this->empty = 0;
	this->data = NULL;
}

/*
You can use this special format to pack data in the array.
To do this, you pass a number(of bytes) and a pointer. Data
is read from the pointer (accordint to the number of bytes)
and added to the array.
You can pass multiple number/pointer combinations, but you
can only fill up one entry increment.
This is useful if you have an array like

int num1, num2, num3, num4;
char c;

n = new array( sizeof(int)*4+sizeof(char) ); // 64 byte entry size
n->add( 1, &c, 4, &num1, 4, &num2, 4, &num3, 4, &num4 );

now we have a packed dataset: (each letter represents one byte of memory)
          "cnum1num2num3num4"

*/

void tarray::add(int size0, ...)
{
	va_list args;
	char *buf;	// this is a char * instead of a void* so we can use pointer arithmetic
	  // there is probably a better way to do this
	void *onearg=NULL;
	int bufoff;
	int inc;
	
	buf = (char*)malloc( entry_increment );
	
	va_start( args, size0 );
	onearg = va_arg(args, void*);
	inc = size0;
	bufoff = 0;
	
	while( 1 ) {
		memcpy( buf+bufoff, onearg, inc );
		bufoff += inc;
		if( bufoff >= entry_increment )
			break;
		inc = va_arg(args, int);
		onearg = va_arg(args, void*);
	}
	
	va_end( args );

	if( !onearg ) {
//		lprintf("Invalid array addition parameters at offset %d.", bufoff);
		return;
	}
	
	add(buf);
	free(buf);
}

// add a preformatted memory section of entrysize length
void tarray::add(char *entry)
{
	char *ptr;
	
	// See if we have an empty entry. Quickly eh?
	if( empty > 0 ) {
		int count;
		char *testbuf = (char*) malloc(entry_increment);
		
		crossout(testbuf, entry_increment);
		
		for( ptr = data, count = 0 ; count < used ; count++, ptr += entry_increment ) {
			if( memcmp( ptr, testbuf, entry_increment ) == 0 ) {
				free(testbuf);
				memcpy( ptr, entry, entry_increment );
				empty--;
				return;
			}
		}
		free(testbuf); // Couldn't find any empties for some reason.
		empty = 0;
	}
	
	if( used >= allocated ) {
		allocated = ((int)(used/alloc_increment+1)*this->alloc_increment);
		data = (char*) realloc( data, allocated*entry_increment );
	}
	
	ptr = data + (used*entry_increment);
	used++;
	
	memcpy( ptr, entry, entry_increment );
}

// Preallocate an additional n spaces (for instance if you load a file
// and immediately determine the number of records, you would use this
// function to speed up the read efficiency)
void tarray::alloc(long n)
{
	long old_alloc=0;
	
	used += n;
	if( used >= allocated ) {
		old_alloc = allocated;
		allocated = ((int)(used/alloc_increment+1)*this->alloc_increment);
		data = (char*) realloc( data, allocated*entry_increment );
		memset(data+(entry_increment*old_alloc), 0, (allocated-old_alloc)*entry_increment );
	}
}

// Allocate one entry and return the memory address.
 // NOTE that if you have already preallocated memory, this should go quite faster.
char *tarray::alloc(void)
{
	char *ptr;
	
/*	if( empty > 0 ) {
		int count;
		char *testbuf = (char*) malloc(entry_increment);
		
		crossout(testbuf, entry_increment);
		
		for( ptr = data, count = 0 ; count < used ; count++, ptr += entry_increment ) {
			if( memcmp( ptr, testbuf, entry_increment ) == 0 ) {
				free(testbuf);
				empty--;
				memset( ptr, 0, entry_increment );
				return ptr;
			}
		}
		free(testbuf); // Couldn't find any empties for some reason.
		empty = 0;
	} */
	
	used++;
	if( used >= allocated ) {
		allocated = ((int)(used/alloc_increment+1)*this->alloc_increment);
		data = (char*) realloc( data, allocated*entry_increment );
	}
	
	ptr = data + ((used-1)*entry_increment);
	memset( ptr, 0, entry_increment ); 
	
	return ptr;
}

// Remove a particular value from the array. This searches by value
// then invalidates the value. If you are using the pointer (returned
// from alloc/find/get) every bit in it will be set to 'on'.

void tarray::remove(char *entry)
{
	char *ptr = find(entry, entry_increment);
	
	if( !ptr )
		return;
	
	crossout( ptr, entry_increment );
	empty++;

	return;
}

void tarray::remove(int i)
{
	char *ptr = data + (i*entry_increment);
	
	crossout( ptr, entry_increment );
	empty++;
	
	return;
}

// Search by value
char *tarray::find(char *search, int len)
{
	char *ptr;
	int count;
	
	if( !search )
		return NULL;
	if( len == -1 )
		len = entry_increment;
	
	for( ptr = data, count = 0; count < used; count++, ptr += entry_increment ) {
		if( memcmp(ptr, search, len) == 0 ) {
			return ptr;
		}
	}
	return NULL;
}

char *tarray::forceget(int n)
{
	if( n >= used ) {
		alloc((n-used)+1);
	}
	return get(n);
}

char *tarray::get(int n)
{
	if( n >= used )
		return NULL;
	return data + entry_increment*n;
}

bool tarray::check(int n)
{
	int bitoffset;
	char *p;
	
	p = get(n);
	if( !p )
		return false;
	if( empty <= 0 )
		return true;
	
	bitoffset = 0;
	while( bitoffset < entry_increment*8 ) {
		if( (1 & (*p >> (bitoffset%8))) == 0 )
			return true;
		bitoffset++;
		if( bitoffset % 8 == 0 ) {
			p++;
		}
	}
	return false;	
}

void crossout(char *ptr, int size)
{
        int bitoffset;
        char *p;

        bitoffset = 0;
        p = ptr;
        while( bitoffset < size*8 ) {
                *p = *p | 1 << (bitoffset%8);

                bitoffset++;
                if( bitoffset % 8 == 0 ) {
                        p++;
                }
        }
}

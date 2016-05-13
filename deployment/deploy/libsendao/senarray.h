#ifndef _SENDAO_ARRAY_H
#define _SENDAO_ARRAY_H

/*


Details

Normally you would say 'int numbers[32];'
Here you say 'tarray numbers(sizeof(int), 32); numbers.allocate(32)'

Normally you would say 'numbers[13] = 4;'
Here you say 'int *v = numbers.get(13); *v = 4;'


typedef struct {
	int some_value;
	int some_other_one;
} some_structure;

alpha = new tarray( sizeof(some_structure), 32 );

some_structure *ssdata = alpha->alloc();


*/
class tarray {
	private:
	int entry_increment;
	int alloc_increment;
	
	public:
	long used;
	long allocated;
	int empty;
	char *data;
	
	public:
	tarray(int entry_size, int alloc_blocks=10);
	tarray(int entry_size_bits, int alloc_blocks, int padding);
	void add(int size0, ...);
	void add(char *entry);
	void alloc(long n);
	void remove(int i);
	void remove(char *entry);
	char *alloc(void);
	char *find(char *search, int len=-1);
	char *get(int n);
	char *forceget(int n);
	bool check(int n);
};

#define forARRAY( var, counter, array ) \
	for( (counter) = 0, (void*)(var) = NULL ; (array) && ((counter) < (array)->used) ; (counter)++ ) \
		if( ( ( (void *)(var) = (void *)((array)->get((counter))) ) != NULL ) && \
			( (array)->check((counter)) ) )
#define forPARRAY( var, counter, array ) \
	for( (counter) = 0, (var) = NULL ; (array) && (counter) < (array)->used ; (counter)++ ) \
		if( ( ( (var) = (void *)((array)->get((counter))) ) != NULL ) && \
			( (array)->check((counter)) ) )
#define forTARRAY( var, counter, array, type ) \
	for( (counter) = 0, (var) = (type)0 ; (array) && (counter) < (array)->used ; (counter)++ ) \
		if( ( ( (var) = (type)((array)->get((counter))) ) != NULL ) && \
			( (array)->check((counter)) ) )
	
void crossout(char *ptr, int size);

#endif

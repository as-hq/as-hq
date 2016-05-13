#ifndef _SENDAO_EXPR_H
#define _SENDAO_EXPR_H

typedef struct expr_		expr;
typedef struct expr_match_	expr_match;

struct expr_ {
	char *name;
	char *chars;
	char *str;
	int repetition;
	expr *next;
	expr *sub, *set;
};

struct expr_match_ {
	char *name;
	char *match;
	expr *by;
	expr_match *next;
};


expr_match *expr_test( expr *e, const char *buf, char *name=NULL );

expr *new_expr(void);
expr *expr_parse(const char *);

void expr_dump(expr *);
void expr_match_dump(expr_match *em);


#endif

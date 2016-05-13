#ifndef __DEPLOY_HH
#define __DEPLOY_HH

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "sendao.h"

typedef struct _scriptfile scriptfile;
typedef struct _scriptsegment scriptsegment;
typedef struct _scriptline scriptline;

#include "pipe.hh"
#include "rce.hh"

struct _scriptfile {
	char *source;
	tlist *segments;
	tnode *current;
	int runstate;
};

struct _scriptsegment {
	char *comment;
	tlist *lines;
	tnode *current;
};

struct _scriptline {
	char *cmd;
	int runstate; // 0=ready, 1=run, 2=done, 4=errors
};

scriptsegment *new_segment(void);
scriptline *new_scriptline(const char *txt);
scriptfile *loadScript(const char *fn);

#endif

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
	int segment_repeats;
	int line_repeats;
	bool chained;
};

struct _scriptsegment {
	char *comment;
	tlist *lines;
	tnode *current;
	bool repeatable; // lines in segment will each repeat
	bool repeat_segment; // segment will repeat if any step fails
	bool chained; // segment will stop if any step fails
	int segment_repeats;
	int line_repeats;
};

struct _scriptline {
	char *cmd;
	char *result;
	int runstate; // 0=ready, 1=run, 2=done, 4=errors
	int line_repeats;
};

scriptsegment *new_segment(scriptfile *sf);
scriptline *new_scriptline(scriptfile *sf, scriptsegment *seg, const char *txt);
scriptfile *loadScript(const char *fn);


scriptline * next_scriptline(scriptfile *sf );
scriptline *current_scriptline( scriptfile *sf );
scriptsegment * current_scriptsegment(scriptfile *sf);

enum {
	RUNSTATE_NONE,
	RUNSTATE_RUNNING,
	RUNSTATE_ERRORED,
	RUNSTATE_COMPLETE,
	RUNSTATE_TESTING,
	RUNSTATE_RESULT
};

#endif

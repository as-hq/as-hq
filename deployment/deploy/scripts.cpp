#include "deploy.hh"


scriptsegment *new_segment(void)
{
	scriptsegment *seg = (scriptsegment*)malloc(sizeof(scriptsegment));
	seg->lines = new tlist;
	seg->comment = NULL;
	seg->current = NULL;
	return seg;
}
scriptline *new_scriptline(const char *txt)
{
	scriptline *sl = (scriptline*)malloc(sizeof(scriptline));
	sl->cmd = str_dup(txt);
	sl->runstate = 0;
	return sl;
}

scriptfile *loadScript(const char *fn)
{
	scriptfile *script = (scriptfile*)malloc(sizeof(scriptfile));
	scriptsegment *seg = NULL;
	tlist *lines;
	tnode *n;
	char *strline;

	script->source = readwholefile(fn);
	if( !script->source ) {
		fprintf(stderr, "Problem reading %s (scriptfile).", fn);
		return NULL;
	}

	lines = split(script->source, "\n");

	script->segments = new tlist();
	forTLIST( strline, n, lines, char* ) {
		strip_newline(strline);
		if( !*strline ) continue;
		if( *strline == '#' ) {
			if( strlen(strline)<5 ) continue;

			if( seg ) {
				script->segments->PushBack(seg);
			}
			seg = new_segment();
			seg->comment = str_dup(strline);
//			printf("script seg: %s\n", strline);
		} else {
			if( !seg ) {
				seg = new_segment();
				seg->comment = str_dup("first");
			}
//			printf("script line: %s\n", strline);
			seg->lines->PushBack( new_scriptline(strline) );
		}
	}
	if( seg ) {
		seg->current = seg->lines->nodes;
		script->segments->PushBack(seg);
	}


	script->current = script->segments->nodes;
	script->runstate = 0;

	return script;
}

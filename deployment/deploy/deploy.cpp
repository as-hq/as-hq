#include "deploy.hh"

/*
 * Implement SensitiveBash(cmd) and make sure to grab the return value (probably echo $?)
 * Read bash script & run through sbash
 * Collect and compile errors
 * Continue if errors fixed
 *
 */


// Warning: this code may contain anti-patterns
// consume with salt

void pipeReadHandler( Pipe *p, char *buf );
void pipeIdleHandler( Pipe *p );
void pipeCmdHandler( Pipe *p );

scriptfile *running_script=NULL;

bool pipeTest( Pipe *p )
{
	if( running_script && !running_script->current ) {
		return true;//anyway:)return false;
	}
	return false;
}

Pipe *pipeConnection( void )
{
	Pipe *p = new Pipe;

	p->log("deploy_latest.log");
	p->readCB = pipeReadHandler;
	p->idleCB = pipeIdleHandler;
	p->cmdlineCB = pipeCmdHandler;

	p->breakTest = pipeTest;

	p->write_to_stdout = false;
	p->read_from_stdin = false;

	return p;
}


void pipeReadHandler( Pipe *p, char *buf )
{
	tlist *lines = split(buf, "\n");
	tnode *n;
	scriptsegment *seg;
//	scriptline *line;
	char *line_input;

	if( !running_script->current )
		seg = NULL;
	else
		seg = (scriptsegment*)running_script->current->data;

	//printf("Lines count from '%s': %d\n", buf, lines->count);
	forTLIST( line_input, n, lines, char* ) {
		switch( running_script->runstate ) {
		case 0: // initialization
			//fprintf(stdout, "init: '%s'\n", line_input);
			continue;
		case 1: // command runtime
			//! log to file
			//! detect obvious warnings
			if( seg ) {
				//line = (scriptline*)(seg->current ? seg->current->data : NULL);
				//fprintf(stdout, "cmd: %s: %s\n", line ? line->cmd : "[]", buf);
			}
			continue;
		case 2: // command test
			if( seg ) {
				//line = (scriptline*)(seg->current ? seg->current->data : NULL);
				//fprintf(stdout, "test:(%s):'%s'\n", line ? line->cmd : "[]", buf);
			}
			continue;
		}
	}

	lines->Clear( free );
	delete lines;
}

void pipeIdleHandler( Pipe *p )
{
	//! detect idle time
	//! possibly error out, or just take note of the elapsed time
}

void pipeCmdHandler( Pipe *p )
{
	scriptsegment *seg;
	scriptline *line = NULL;

	if( !running_script || !running_script->current ) {
//		printf("no_run_cmd\n");
		return;
	}

	seg = (scriptsegment*)running_script->current->data;
	while( !seg->current ) {
		seg->current = seg->lines->nodes;
		if( !seg->current ) {
			// next_segment for empty segments
			if( seg->comment )
				printf("\n##%s\n", seg->comment);
			running_script->current = running_script->current->next;
			seg = (scriptsegment*)running_script->current->data;
		}
	}
	line = (scriptline*)seg->current->data;

	// request return value from last command
	if( running_script->runstate == 1 ) {
//		printf("..read runstate=1..\n");
		p->write("echo exit:$?\n");
		running_script->runstate = 2;
		return;
	}
	if( running_script->runstate == 2 ) {
		//! read returned value from input log, verify == 0 or flag last process as failed
		const char *ecode = str_r_str( p->readbuffer->p, "exit:" );
		if( ecode ) ecode = ecode + 5;
		const char *endofline = strstr( ecode, "\n" );
		char exitCodeBuf[64];
		int iExit;

		if( !endofline ) {
			printf("\n\nCouldn't find eCode? :: '%s'\n", p->readbuffer->p);
		} else {
			strncpy( exitCodeBuf, ecode, endofline-ecode );
			exitCodeBuf[ endofline-ecode ] = 0;
			//printf("\nGot exit code: '%s'\n", exitCodeBuf);
			iExit = atoi(exitCodeBuf);

			if( iExit == 0 ) {
				// successful exit
				printf("success\n");
			} else {
				// not so successful.. repeat loop if configured
				printf("error\n");
			}
		}

		//printf("..read runstate=2..\n");

		if( seg->current->next ) { // next line
			seg->current = seg->current->next;
			//printf("next line: %s\n", ((scriptline*)seg->current->data)->cmd);
			line = (scriptline*)seg->current->data;
		} else if( running_script->current->next ) {
			running_script->current = running_script->current->next;
			seg = (scriptsegment*)running_script->current->data;
			if( !seg->current )
				seg->current = seg->lines->nodes;
			//printf("\nnext segment: %s\n", ((scriptsegment*)running_script->current->data)->comment);
			line = (scriptline*)seg->current->data;
		} else {
			running_script->current = NULL;
			printf("\nScript completed\n");
			return;
		}
		running_script->runstate = 0;
	}

	if(line ) {
		printf("\n\n%s: %s\n", seg->comment ? seg->comment : "runtime", line->cmd );
		p->write( line->cmd );
		p->write(" 2>&1\n");
		running_script->runstate = 1;
	} else {
		printf("\nempty_cmd\n");
		running_script->current = NULL;
		return;
	}
}



int main(int ac, char *av[])
{
	Pipe *target = pipeConnection();
	scriptfile *scr = loadScript("deploy.sh");

	if( !target || !scr ) {
		return 2;
	}

	target->open();

	printf("\n");

	target->getcommandprompt();
	running_script = scr;

	target->write("\n");

	target->inputloop();

//! clean up and store logfiles


	printf("exit()\n");
//! unnecessary GC:
	delete target;

	return 0;
}


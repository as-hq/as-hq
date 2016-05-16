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
	scriptline *line;
	char *line_input;

	if( !running_script ) return;
	seg = current_scriptsegment(running_script);
	if( !seg ) return;
	line = current_scriptline(running_script);

	if( line && line->runstate == RUNSTATE_RUNNING ) {
		strexpand( &line->result, buf );
	}

	//printf("Lines count from '%s': %d\n", buf, lines->count);
	forTLIST( line_input, n, lines, char* ) {
		switch( running_script->runstate ) {
		case RUNSTATE_NONE: // initialization
			//fprintf(stdout, "init: '%s'\n", line_input);
			continue;
		case RUNSTATE_RUNNING: // command runtime
			//! detect warnings during runtime
			// if( line->runtype && list_regexp( line->runtype->warnings, line ) ) {
			if( seg ) {
				//line = (scriptline*)(seg->current ? seg->current->data : NULL);
				//fprintf(stdout, "cmd: %s: %s\n", line ? line->cmd : "[]", buf);
			}
			continue;
		case RUNSTATE_TESTING: // command test
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
	// ... ;)
}

void pipeCmdHandler( Pipe *p )
{
	scriptsegment *seg = NULL;
	scriptline *line = NULL;

	if( !running_script ) {
		printf("no_run_cmd\n");
		return;
	}

	seg = current_scriptsegment(running_script);
	line = seg ? current_scriptline(running_script) : NULL;

	switch( running_script->runstate ) {
		case RUNSTATE_ERRORED:
			printf("Script had an error.\n");
			running_script->current = NULL;
			return;
		case RUNSTATE_COMPLETE:
			printf("Script completed.\n");
			running_script->current = NULL;
			return;
		case RUNSTATE_TESTING:
			printf("Command '%s' result:\n%s", line->cmd, line->result ? line->result : "unknown");
			line->runstate = RUNSTATE_TESTING;
			p->write("echo exit:$?\n");
			running_script->runstate = RUNSTATE_RESULT;
			//! check result patterns for regexp errors if we have any here, eg
			// if( list_regexp( line->result_buffer, line->runtype->errorlist ) ) { running_script->runstate = RUNSTATE_ERRORED; }
			return;
		case RUNSTATE_RESULT:
			const char *ecode = str_r_str( p->readbuffer->p, "exit:" );
			if( ecode ) ecode = ecode + 5;
			const char *endofline = strstr( ecode, "\n" );
			char exitCodeBuf[64];
			int iExit;

			if( !endofline ) {
				printf("\n\nCouldn't find eCode? :: '%s'\n", p->readbuffer->p);
				line->runstate = RUNSTATE_ERRORED;
			} else {
				strncpy( exitCodeBuf, ecode, endofline-ecode );
				exitCodeBuf[ endofline-ecode ] = 0;
				//printf("\nGot exit code: '%s'\n", exitCodeBuf);
				iExit = atoi(exitCodeBuf);

				if( iExit == 0 ) {
					line->runstate = RUNSTATE_COMPLETE;
				} else {
					// not so successful.. repeat loop if configured
					printf("Command failed! Exit code: %d\n", iExit);
					// log command output to failure buffer
					line->runstate = RUNSTATE_ERRORED;
				}
			}
			//...running_script->runstate = RUNSTATE_NONE;
			line = next_scriptline(running_script);
			break;
	}

	if(line ) {
		//printf("\n\n%s: %s[%d]\n", seg->comment ? seg->comment : "runtime", line->cmd, line->line_repeats );
		p->write( line->cmd );
		p->write(" 2>&1\n");
		running_script->runstate = RUNSTATE_TESTING;
		line->runstate = RUNSTATE_RUNNING;
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


	target->getcommandprompt();
	running_script = scr;

	printf("Running script.\n");
	target->write("\n");

	target->inputloop();

//! clean up and store logfiles


	printf("exit()\n");
//! unnecessary GC:
	delete target;

	return 0;
}


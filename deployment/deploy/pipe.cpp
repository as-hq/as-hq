#include "deploy.hh"

Pipe::Pipe(void)
{
	fdm = fds = -1;
	childproc = -1;
	logfn = NULL;
	logbuf = NULL;
	cmdprompt = NULL;
	readbuffer = new stringbuf(128);
	readbuffer->maxlen = 2048;
	read_from_stdin = write_to_stdout = true;
	readCB = NULL;
	idleCB = NULL;
	cmdlineCB = NULL;
	readBreakTest = NULL;
	breakTest = NULL;
}

void Pipe::log(const char *logfilename)
{
	logfn = strdup( logfilename );
}

void Pipe::logtobuf( char **lbuf )
{
	logbuf = lbuf;
	if( logbuf )
		*logbuf = NULL;
}

bool pipe_test_newline( Pipe *p, char *buf )
{
	return ( strstr(buf,"\n") == 0 );
}

bool Pipe::open(void)
{
	char **args;

	args = (char**) calloc( sizeof(char*), 2 );
	args[0] = strdup("bash");
	args[1] = NULL;

	run( args );
	free( args[0] );
	free( args );

	return true;
}

void Pipe::exec(char **lbuf, const char *cmd)
{
	char *args[256];
	int i;
	const char *p;
	char buf[1025], *pbuf;
	
	logbuf = lbuf;
	*lbuf = NULL;
	i=0;
	for( p = cmd, pbuf=buf; *p; p++ ) {
		if( *p == ' ' ) {
			if( buf != pbuf ) {
				*pbuf = '\0';
				args[i++] = strdup( buf );
				pbuf = buf;
				continue;
			}
		} else {
			*pbuf++ = *p;
		}
	}
	if( buf != pbuf ) {
		*pbuf = '\0';
		args[i++] = strdup( buf );
	}
	args[i] = NULL;
	
	run( args );

	while( i > 0 ) {
		--i;
		free(args[i]);
	}
}


void Pipe::run(char* const args[])
{
	close();
	fdm = ::open("/dev/ptmx", O_RDWR);
	if( fdm < 0 ) {
		perror("open");
		abort();
	}
	
	grantpt( fdm );
	unlockpt( fdm );
	char *slavename = ptsname(fdm);

	// Fork into a shell
	childproc = fork();
	if( childproc == 0 ) {
		//printf("fork: child\n");
		if( fdm >= 0 )
			::close(fdm);
		
		setsid();
		
		fds = ::open(slavename, O_RDWR);
		if( fds < 0 ) {
			perror("open");
			abort();
		}
		
		ioctl( fds, I_PUSH, "ptem" );
		ioctl( fds, I_PUSH, "ldterm" );

		// Redirect into 'fds'
		if( dup2( fds, STDIN_FILENO ) != STDIN_FILENO ) {
			fprintf(stdout, "dup2 stdin failed\n");
			abort();
		}
		if( dup2( fds, STDOUT_FILENO ) != STDOUT_FILENO ) {
			fprintf(stdout, "dup2 stdout failed\n");
			abort();
		}
		if( dup2( fds, STDERR_FILENO ) != STDERR_FILENO ) {
			fprintf(stdout, "dup2 stderr failed\n");
			abort();
		}
		// Grab the client shell
		
		
		struct termios tio;
		if (tcgetattr(0, &tio) < 0) {
			perror( "tcgetattr" );
			abort();
		}
		tio.c_oflag &= ~OPOST;
		tio.c_oflag |= ONLCR; 
		tio.c_lflag |= ICANON | ISIG;
		tio.c_lflag &= ~ECHO;
		if (tcsetattr(0, TCSANOW, &tio) < 0) {
			perror( "tcsetattr" );
			abort();
		}
		
		/*
		fprintf( stdout, "pipe " );
		for( i = 0; args[i]; i++ ) {
			fprintf( stdout, "%s ", args[i]);
		}
		fprintf( stdout, "\n" );
		*/
		
		//printf("child:run\n");
		//execlp( "echo", "hello", "world");
		execvp( args[0], args );
		//execlp("/usr/local/bin/bash", "bash", "-l", "-i", NULL);
		
		perror("execvp");
		abort();
	}
	if( fds >= 0 )
		::close(fds);
		
	struct termios tio;
	if (tcgetattr(fdm, &tio) < 0) {
		perror( "tcgetattr" );
		abort();
	}
	tio.c_oflag &= ~OPOST;
//	tio.c_oflag |= ONLCR;
	tio.c_iflag |= IGNCR; 
	tio.c_lflag &= ~ECHO;
	if (tcsetattr(fdm, TCSANOW, &tio) < 0) {
		perror( "tcsetattr" );
		abort();
	}
}

int Pipe::read(char *buf, int maxlen)
{
	int n, maxfd=fdm+1;
	fd_set ins;
	char *tptr;
	
	FD_ZERO( &ins );
	FD_SET( fdm, &ins );
	select( maxfd, &ins, NULL, NULL, NULL );
	
	if( FD_ISSET( fdm, &ins ) ) {
		n = ::read( fdm, buf, maxlen );
		if( n <= 0 ) {
			buf[0] = '\0';
			return n;
		}
		
		buf[n] = '\0';
		//printf("read[%d]:%s\n", n, buf);
		readbuffer->append(buf);

		if( readCB )
			readCB(this, buf);
		if( cmdprompt && cmdlineCB && match_commandline( buf ) ) {
			cmdlineCB(this);
		}

		if( logfn ) {
			FILE *fp;
			fp = fopen( logfn, "a" );
			fputs( buf, fp );
			fclose( fp );
		}
		if( logbuf ) {
			if( *logbuf ) {
				tptr = (char*)realloc( *logbuf, strlen( *logbuf ) + n + 5 );
				strcat(tptr, buf);
			} else {
				tptr = (char*)malloc( n + 5 );
				strcpy(tptr, buf);
			}
			*logbuf = tptr;
		}
		return n;
	}
	
	return 0;
}

int Pipe::write(const char *buf)
{
	int len = strlen(buf);
	
	return ::write( fdm, buf, len );
}

int Pipe::waitchild(int flags)
{
	int n;
	
	if( childproc == -1 )
		return 1;
	
	n = waitpid( childproc, NULL, flags );
	if( n > 0 )
		childproc = -1;
	
	return n;
}
bool Pipe::match_commandline( const char *inbuf )
{
	uint32_t len = strlen(inbuf), plen = strlen(cmdprompt);
	const char *pInbuf, *pCmdline;

	pInbuf = inbuf + len;
	pCmdline = cmdprompt + plen;

	while( *pInbuf == *pCmdline ) {
		pCmdline--;
		if( pCmdline <= cmdprompt ) return true;
		pInbuf--;
		if( pInbuf <= inbuf ) {
			if( logbuf && *logbuf && strlen(*logbuf) > len ) {
				len = strlen(*logbuf);
				pInbuf = *logbuf + len;
			} else return false;
		}
	}
	return false;
}
void Pipe::getcommandprompt( void )
{
	//printf("show prompt\n");

	// temporarily disable normal hooks
	pipe_read_cb *tempRead = readCB;
	pipe_cb *tempIdle = idleCB;

	pipe_readtest_cb *tempReadTest = readBreakTest;
	pipe_test_cb *tempTest = breakTest;
	readCB = NULL;
	idleCB = NULL;
	breakTest = NULL;
	readBreakTest = pipe_test_newline;

	readbuffer->clear();

	// trigger a commandline prompt to be displayed
	write("echo find prompt\n");

	// run loops to fill the input buffer
	int i, searchlen = strlen("find prompt\n");
	int readversion = 0;

	while( true ) {
		inputloop(); // read one line
		//printf("readbufferlen: %d\n", readbuffer->len);

		if( readversion == 0 ) {
			// we check the end of the input buffer
			i = readbuffer->len - searchlen;
			while( i > 0 && strncmp(readbuffer->p+i,"find prompt\n", searchlen) != 0 ) {
				i--;
			}
			if( i <= 0 )
				continue;

			readversion = i+searchlen;
		}

		if( readversion!=0 && readbuffer->len > readversion ) {
			//printf(" rv at %d: %s\n", readversion, readbuffer->p);
			cmdprompt = str_dup( readbuffer->p + readversion );
			strip_newline(cmdprompt);
			break;
		}
	}

	//printf(" Prompt looks like: '%s'\n", cmdprompt?cmdprompt:"null");
	// re-enable hooks
	readCB = tempRead;
	idleCB = tempIdle;
	breakTest = tempTest;
	readBreakTest = tempReadTest;

	// normal processing resumes
}
void Pipe::inputloop()
{
	fd_set ins, outs, excepts;
	int maxfd;
	char buf[1025];
	int len;
	struct timeval timeout;
	
//	printf("parent:inputloop\n");
	maxfd = fdm+1;
		
	while( 1 ) {
		FD_ZERO( &ins );
		FD_ZERO( &outs );
		FD_ZERO( &excepts );
		
		if( read_from_stdin ) {
			FD_SET( STDIN_FILENO, &ins ); // messages from keyboard/user
			FD_SET( STDIN_FILENO, &excepts );
		}
		FD_SET( fdm, &ins ); // messages from pipe
		FD_SET( fdm, &excepts );
		
		timeout.tv_sec = 0;
		timeout.tv_usec = 1000000;
		select( maxfd, &ins, NULL, &excepts, &timeout );

		/* Read from stdin and write to pipe */
		if( FD_ISSET( STDIN_FILENO, &ins ) ) {
			len = ::read( STDIN_FILENO, buf, 1024 );
			if( len == 0 ) {
				fprintf( stdout, "EOF read on stdin.\n");
				break;
			}
			
			buf[len] = '\0';
			::write( fdm, buf, len );
			// fsync( fdm );
		}
		
		/* Read from pipe, echo to stdout */
		if( FD_ISSET( fdm, &ins ) ) {
			len = read( buf, 1024 ); // this->read
			if( len > 0 ) {
				if( write_to_stdout )
					::write( STDOUT_FILENO, buf, len ); // write to the real stdout

				if( readBreakTest && readBreakTest(this, buf) ) {
					return;
				}
			} else {
				fprintf( stdout, "EOF on pipe.\n" );
				break;
			}
		}
		
		if( FD_ISSET(fdm, &excepts) || FD_ISSET(0, &excepts) ) {
			fprintf( stdout, "Session ended: pipe crashed.\n" );
			break;
		}
		
		/*
		if( waitchild(WNOHANG) != 0 ) {
			fprintf( stdout, "Session ended peacefully.\n" );
			break;
		}
		*/

		if( breakTest && breakTest(this) ) {
			::write( fdm, "exit\n", 5 );
			break;
		}
	}
	
	maxfd++;
	fprintf( stdout, "Waiting for child to exit\n");
	waitchild( WNOHANG );
	fprintf( stdout, "Closing pipe\n" );
	// disconnect
	close();
}

void Pipe::close(void)
{
	if( fdm != -1 ) {
		::close( fdm );
		fdm = -1;
	}
	if( fds != -1 ) {
		::close( fds );
		fds = -1;
	}
	if( childproc > 0 ) {
		waitchild(WNOHANG);
		childproc = -1;
	}
}

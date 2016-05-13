#ifndef __PIPE_HH
#define __PIPE_HH

typedef class Pipe Pipe;

typedef void pipe_cb( Pipe* );
typedef void pipe_read_cb( Pipe*, char* );
typedef bool pipe_readtest_cb( Pipe*, char* );
typedef bool pipe_test_cb( Pipe* );


class Pipe {
	public:
	char *logfn;
	int fdm, fds;
	int childproc;
	char **logbuf;
	
	char *cmdprompt;
	stringbuf *readbuffer;

	pipe_read_cb *readCB; // read input
	pipe_cb *idleCB; // on select() read timeout
	pipe_cb *cmdlineCB; // on prompt

	pipe_readtest_cb *readBreakTest; // for breaking out of inputloop()
	pipe_test_cb *breakTest;

	bool write_to_stdout, read_from_stdin;
	public:
	Pipe();
	bool open(void);
	bool match_commandline(const char *inbuf);
	void getcommandprompt(void);
	void log(const char *logfilename);	// adds a logging facility
	void logtobuf(char **);
	void run(char* const args[]);
	void exec(char**,const char *);
	void iterableloop(pipe_test_cb *);
	void inputloop(void);
	int waitchild(int);
	int read(char*,int);
	int write(const char *);
	void close(void);
};

bool pipe_test_newline( Pipe *p, char *buf );

#endif

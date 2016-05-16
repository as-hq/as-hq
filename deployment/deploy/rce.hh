#ifndef __RCE_HH
#define __RCE_HH

#include <unistd.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <stropts.h>
#include <termios.h>

#include "pipe.hh"

class rce : public Pipe {
	private:
	char *server;
	
	public:
	rce(const char *servername);
	bool open();				// opens a connection and pipes the user to it
	bool send(char *src, char *dest); // sends a file
	int exec(char **buf, char *cmd); // executes a command, with arguments possibly
									// buf is populated with the output
	void keygen(void);			// generates a priv/public key for this server
};


#endif

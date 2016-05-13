#include "rce.hh"
//???#include "deploy.hh"

rce::rce(const char *servername) : Pipe()
{
	server = strdup( servername );
}

bool rce::open(void)
{
	char buf[strlen(server)+6];
	char **args;
	
	keygen();
	
	args = (char**) calloc( sizeof(char*), 3 );
	sprintf(buf, "root@%s", server);
	args[0] = "ssh";
	args[1] = buf;
	args[2] = NULL;
	
	run( args );
	inputloop();
	
	free( args );
	
	return true;
}

void rce::keygen(void)
{
	FILE *fp;
	char buf[1024], readbuf[1024], fn[1024];
	char **args;
	
	if( strcasecmp( getenv("USER"), "root" ) == 0 ) {
		// oh shit! well they shouldn't be doing this but we'll let them get away with it,
		// since we're taking root access on the target anyway...
		strcpy(fn, "/root/.ssh/id_rsa.pub");
	} else {
		sprintf(fn, "/home/%s/.ssh/id_rsa.pub", getenv("USER"));
	}
	
	// check to see if a public key exists
	fp = fopen(fn, "r");
	if( !fp ) {
		// nope.. generate public key
		fprintf(stdout, "Preparing public key... please do not enter a passphrase; simply hit enter, twice.\n");
		system("ssh-keygen -t rsa -f ~/.ssh/id_rsa.pub -q");
	} else {
		fclose( fp );
	}
	
	// Open a connection to the target server, just to see if it wants a password...
	args = (char**)calloc( sizeof(char*), 4);
	sprintf( buf, "root@%s", server);
	args[0] = "ssh";
	args[1] = buf;
	args[2] = NULL;
	
	run( args );
	free( args );
	
	*readbuf = '\0';
	while( read( readbuf, 1024 ) <= 0 );
	Pipe::close();
	
	// If no password is requested, we don't need to gen a key
	if( strstr(readbuf, "root@") != readbuf )
		return;
	
	fprintf(stdout, "Enter the password for this server, and I will set up secure authentication with it.\n");
	fprintf(stdout, "root@%s's password: ", server);
	fgets( buf, 1024, stdin );
	

		// Send the public key
	send( fn, "/root/.ssh/auth_new" );
	while( read( readbuf, 1024 ) <= 0 );
	write( buf );
	inputloop();

		// Authenticate the public key
	exec( NULL, "cat /root/.ssh/auth_new >> /root/.ssh/authorized_keys ; rm /root/.ssh/auth_new" );
	while( read( readbuf, 1024 ) <= 0 );
	write( buf );
	inputloop();
	
	return;
}

int rce::exec(char **buf, char *cmd)
{
	char **args;
	char loginbuf[strlen(server)+6];
	
	sprintf( loginbuf, "root@%s", server);
	
	args = (char**)calloc(sizeof(char*), 4);	
	args[0] = "ssh";
	args[1] = loginbuf;
	args[2] = cmd;
	args[3] = NULL;
	
	logtobuf( buf );
	
	fprintf( stdout, "ssh %s \"%s\"\n", loginbuf, cmd );
	
	run( args );
	
	free( args );
	
	return 0;
}

bool rce::send(char *src, char *dest)
{
	char **args;
	char destbuf[strlen(dest)+strlen(server)+10];
	
	sprintf( destbuf, "root@%s:%s", server, dest);
	
	args = (char**) calloc( sizeof(char*), 4 );
	args[0] = "scp";
	args[1] = src;
	args[2] = destbuf;
	args[3] = NULL;
	
	fprintf( stdout, "%s %s %s\n", "scp", src, destbuf );

	run( args );
	
	free( args );
	
	return false;
}

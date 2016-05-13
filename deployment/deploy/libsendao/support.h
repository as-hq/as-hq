#ifndef _SENDAO_SUPPORT_H
#define _SENDAO_SUPPORT_H


// System Includes
#include <unistd.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>
#include <time.h>


// Macro Definitions
#define TRUE 1
#define FALSE !TRUE

typedef unsigned int u32;

// Support Includes

#include "senlist.h"
#include "senarray.h"
#include "senattribs.h"

// Utility Functions

void strip_newline( char *str );

#endif

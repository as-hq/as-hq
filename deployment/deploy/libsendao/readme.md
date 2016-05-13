# Classes And Objects


## Memory Management

### Pointer Manager
class ptrmap


### String Management
class strmap



## Data Structures

### list
A linked list.

#### class tlist
Basic implementation

### table
A hash table

#### class HTable
Basic implementation, returns a list when searched, is add/remove only, no 'set'

### map
A hash table with single Get/Set identities instead of add/remove and search.

#### class DMap
Maps intvoid

#### class TMap
Maps inttype

#### class SMap
Maps namekeyvoid

#### class OMap
Maps namekeytype


### direct
A tree with mapping to void pointers.

### sdirect
A tree with mapping to objects.


## Structs

### Named Object Pointers

#### intvoid
uint32 idc; void *ptr;
#### namevoid
char *name; void *ptr;
#### namekeyvoid
uint32 idc; char *name; void *ptr;

### Typed Object Pointers

#### inttype
uint32 idc; void *ptr; sint32 type;
#### nametype
char *name; void *ptr; sint32 type;
#### namekeytype
uint32 idc; char *name; void *ptr; sint32 type;



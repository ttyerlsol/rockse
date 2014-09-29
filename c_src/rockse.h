#include "rocksdb/c.h"

#include "erl_interface.h"
#include "ei.h"

typedef struct rockse {
  rocksdb_t *db;
  rocksdb_options_t *options;
  rocksdb_writeoptions_t *writeoptions;
  rocksdb_readoptions_t *readoptions;
} rockse;

typedef unsigned char byte;

void close_and_destroy(rockse *ptr);
rockse * open(const char *path);
void put(rockse *ptr, const char *key, const char *value);
char * get(rockse *ptr, const char *key);
void delete(rockse *ptr, const char *key);
void main_loop(rockse *ptr);

/* void match_pattern(ETERM *); */
int read_cmd(byte *buf);
int read_exact(byte *buf, int len);
int write_cmd(byte *buf, int len);
int write_exact(byte *buf, int len);
void respond(ETERM *r);
void send_bok();
void send_nok();
void send_put_ok();
void send_delete_ok();
void send_get_result(ETERM* r);

static FILE *log;

int match_close(ETERM *term);
int match_put(ETERM *term);
int match_get(ETERM *term);
int match_delete(ETERM *term);
int match_generic(ETERM *term, char *kw, int length);

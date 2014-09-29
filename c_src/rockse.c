#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>

#include "rockse.h"

/**
 ** rockse <path_to_tbl> <path_to_log>
 **/
int main(int argc, char **argv) {
  rockse * ptr;

  ETERM *from_erl;
  ETERM *cmd;
  ETERM *key;
  ETERM *value;

  char *ret;
  ETERM *to_send;

  byte buf[1024];
  int length;

  log = fopen(argv[2], "a+");

  erl_init(NULL, 0);
  fprintf(log, "*** rockse opening table %s\n", argv[1]);
  ptr = open(argv[1]);

  while (read_cmd(buf) > 0) {

    from_erl = erl_decode(buf);
    fprintf(log, "  rockse command type %d\n", ERL_TYPE(from_erl));
 
    if (match_close(from_erl)) {
      send_bok();
      fprintf(log, "   rockse command +close+ sent\n");
      break; 
    }

    if (ERL_IS_TUPLE(from_erl) && ERL_TUPLE_SIZE(from_erl) == 3) {
      /* can only be a {put, key, val} */
      cmd = ERL_TUPLE_ELEMENT(from_erl, 0);

      if (match_put(cmd)) {
	key = ERL_TUPLE_ELEMENT(from_erl, 1);
	value = ERL_TUPLE_ELEMENT(from_erl, 2);

	fprintf(log, "   rockse command +put+ %s %s sent\n", erl_iolist_to_string(key), erl_iolist_to_string(value));

	put(ptr, erl_iolist_to_string(key), erl_iolist_to_string(value));
	send_put_ok();
      }
      erl_free_term(from_erl);
      erl_free_term(cmd);
      erl_free_term(key);
      erl_free_term(value);
    }
    if (ERL_IS_TUPLE(from_erl) && ERL_TUPLE_SIZE(from_erl) == 2) {
      /* can only be a {get, key} or {delete, key} */
      cmd = ERL_TUPLE_ELEMENT(from_erl, 0);
      if (match_get(cmd)) {
	key = ERL_TUPLE_ELEMENT(from_erl, 1);

	fprintf(log, "   rockse command +get+ %s sent\n", erl_iolist_to_string(key));

	ret = get(ptr, erl_iolist_to_string(key));
	if (ret == NULL) {
	  send_nok();
	}
	else {
	  length = strlen(ret);

	  fprintf(log, "   rockse command +get+ found %s of size %d\n", ret, length);

	  to_send = erl_format("{ok, get, ~s}", ret);
	  send_get_result(to_send);
	  free(ret);
	}
      }
      else if (match_delete(cmd)) {
	key = ERL_TUPLE_ELEMENT(from_erl, 1);

	fprintf(log, "   rockse command +delete+ %s sent\n", erl_iolist_to_string(key));

	delete(ptr, erl_iolist_to_string(key));
	send_delete_ok();
      }

      erl_free_term(from_erl);
      erl_free_term(cmd);
      erl_free_term(key);
    }


    fflush(log);
  }

  close_and_destroy(ptr);
  fprintf(log, "*** rockse closing table %s\n", argv[1]);

  erl_free_term(from_erl);

  fclose(log);

  return 0;
}

rockse * open(const char *path) {
  rockse *p;

  char ** errptr;

  erl_init(NULL, 0);

  p = malloc(sizeof(rockse));

  p->options = rocksdb_options_create();
  rocksdb_options_set_create_if_missing(p->options, 1);
  p->writeoptions = rocksdb_writeoptions_create();
  p->readoptions = rocksdb_readoptions_create();

  p->db = rocksdb_open(p->options, path, errptr);

  return p;
}

char * get(rockse *p, const char *key) {
  char *errptr = NULL;
  size_t vallen;
  char * result = NULL;

  result =  rocksdb_get(p->db, p->readoptions, 
			key, strlen(key), &vallen, &errptr);

  fprintf(log, "    rockse get errptr %s vallen %lu\n", errptr, vallen);
  fflush(log);

  if (vallen == 0) {
    fprintf(log, "    rockse read returns no value\n");
    return NULL;
  }
  else if (errptr != NULL) {
    fprintf(log, "    rockse read error %s\n", errptr);
    free(errptr);
    return NULL;
  }
  else if ((errptr == NULL) && (vallen > 0)) {
    fprintf(log, "    rockse read success length %lu\n", vallen);
    result[vallen] = '\0';
    return result;
  }

  return NULL;
}

void delete(rockse *p, const char *key) {
  char *errptr = NULL;

  rocksdb_delete(p->db, p->writeoptions, key,
		 strlen(key), &errptr);

  if (errptr == NULL) {
    fprintf(log, "    rockse delete success\n");
  }
  else {
    fprintf(log, "    rockse delete failed %s\n", errptr);
    free(errptr);
  }

}

void put(rockse *p, const char *key, const char *value) {
  char *errptr = NULL;

  rocksdb_put(p->db, p->writeoptions,
	      key, strlen(key), value, strlen(value), &errptr);

  if (errptr == NULL) {
    fprintf(log, "    rockse read success\n");
  }
  else {
    fprintf(log, "    rockse read failed %s\n", errptr);
    free(errptr);
  }

}

void close_and_destroy(rockse *p) {
  rocksdb_close(p->db);
  rocksdb_writeoptions_destroy(p->writeoptions);
  rocksdb_readoptions_destroy(p->readoptions);
  rocksdb_options_destroy(p->options);

  free(p);
}


void send_bok()
{
  ETERM *to_send;

  to_send = erl_mk_atom("bok");
  fprintf(log, "    rockse sending bok at len %d\n", erl_term_len(to_send));
  respond(to_send);

  erl_free_term(to_send);
}

void send_nok()
{
  ETERM *to_send;

  to_send = erl_format("{error, notfound}");
  fprintf(log, "    rockse sending {error, notfound}\n");
  respond(to_send);

  erl_free_term(to_send);
}

void send_put_ok()
{
  ETERM *to_send;

  to_send = erl_mk_atom("putok");
  fprintf(log, "    rockse sending putok at len %d\n", erl_term_len(to_send));
  respond(to_send);

  erl_free_term(to_send);
}

void send_delete_ok()
{
  ETERM *to_send;

  to_send = erl_mk_atom("deleteok");
  fprintf(log, "    rockse sending deleteok at len %d\n", erl_term_len(to_send));
  respond(to_send);

  erl_free_term(to_send);
}

void send_get_result(ETERM* to_send)
{
  fprintf(log, "    rockse sending get result\n");
  respond(to_send);

  erl_free_term(to_send);
}

void respond(ETERM *r)
{
  byte buf[2048];

  bzero(buf, 2048);
  erl_encode(r, buf);
  write_cmd(buf, erl_term_len(r));

  fprintf(log, "   rockse ack\n");
}

int
read_cmd(byte *buf)
{
  int len;
  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int
read_exact(byte *buf, int len)
{
  int i, got=0;
  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);
  return(len);
}

int
write_cmd(byte *buf, int len)
{
  byte li;
  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int
write_exact(byte *buf, int len)
{
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);
 
 return (len);
}

int
match_close(ETERM *term)
{
  return match_generic(term, "close", 5);
}

int
match_put(ETERM *term)
{
  return match_generic(term, "put", 3);
}

int
match_get(ETERM *term)
{
  return match_generic(term, "get", 3);
}

int
match_delete(ETERM *term)
{
  return match_generic(term, "delete", 6);
}

int
match_generic(ETERM *term, char* kw, int length)
{
  if (ERL_IS_ATOM(term)) {
    return strncmp((const char *)ERL_ATOM_PTR(term), kw, length) == 0;
  }
  else {
    return 0;
  }
}

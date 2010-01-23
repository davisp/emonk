static void send_output(ErlDrvPort port, ErlDrvTermData *terms, int term_count) {
  driver_output_term(port, terms, term_count);
}

static void send_ok_response(spidermonkey_drv_t *dd, const char *call_id) {
  ErlDrvTermData terms[] = {ERL_DRV_BUF2BINARY, (ErlDrvTermData) call_id, strlen(call_id),
			    ERL_DRV_ATOM, dd->atom_ok,
			    ERL_DRV_TUPLE, 2};
  send_output(dd->port, terms, sizeof(terms) / sizeof(terms[0]));
}

static void send_error_string_response(spidermonkey_drv_t *dd, const char *call_id, const char *msg) {
  ErlDrvTermData terms[] = {ERL_DRV_BUF2BINARY, (ErlDrvTermData) call_id, strlen(call_id),
                            ERL_DRV_ATOM, dd->atom_error,
			    ERL_DRV_BUF2BINARY, (ErlDrvTermData) msg, strlen(msg),
			    ERL_DRV_TUPLE, 3};
  send_output(dd->port, terms, sizeof(terms) / sizeof(terms[0]));
}

static void send_string_response(spidermonkey_drv_t *dd, const char *call_id, const char *result) {
  ErlDrvTermData terms[] = {ERL_DRV_BUF2BINARY, (ErlDrvTermData) call_id, strlen(call_id),
                            ERL_DRV_ATOM, dd->atom_ok,
			    ERL_DRV_BUF2BINARY, (ErlDrvTermData) result, strlen(result),
			    ERL_DRV_TUPLE, 3};
  send_output(dd->port, terms, sizeof(terms) / sizeof(terms[0]));
}

static void unknown_command(spidermonkey_drv_t *dd, const char *call_id) {
  ErlDrvTermData terms[] = {ERL_DRV_BUF2BINARY, (ErlDrvTermData) call_id, strlen(call_id),
                            ERL_DRV_ATOM, dd->atom_error,
			    ERL_DRV_ATOM, dd->atom_unknown_cmd,
			    ERL_DRV_TUPLE, 3};
  send_output(dd->port, terms, sizeof(terms) / sizeof(terms[0]));
}
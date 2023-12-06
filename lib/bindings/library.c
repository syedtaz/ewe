#include "library.h"

#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value tsize() {

    struct winsize ws;
    ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);

    value v = caml_alloc_tuple(2);
    Store_field(v, 0, Val_int(ws.ws_row));
    Store_field(v, 1, Val_int(ws.ws_col));

    return v;
}
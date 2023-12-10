#include "library.h"
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value tsize() {
    struct winsize ws;
    ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);
    return Val_int(ws.ws_row << 16 | ws.ws_col);
}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

/* VT100 control codes */
#define ESC "\033"
#define CLEAR ESC "[2J"
#define HOME ESC "[H"
#define CURSOR_OFF ESC "[?25l"
#define CURSOR_ON ESC "[?25h"
#define REVERSE ESC "[7m"
#define NORMAL ESC "[0m"
#define BOLD ESC "[1m"
#define UP ESC "[A"
#define DOWN ESC "[B"
#define RIGHT ESC "[C"
#define LEFT ESC "[D"
#define SAVE_CURSOR ESC "[s"
#define RESTORE_CURSOR ESC "[u"
#define CLEAR_LINE ESC "[2K"
#define CLEAR_EOL ESC "[K"

/* Terminal dimensions */
int term_rows = 24;
int term_cols = 80;

/* Terminal state */
struct termios orig_termios;
int is_raw_mode = 0;

/* Function declarations */
void die(const char *s);
void disable_raw_mode();
void enable_raw_mode();
int get_cursor_position(int *rows, int *cols);
int get_window_size(int *rows, int *cols);
void init_terminal();
void handle_keypress();
void refresh_screen();
void draw_status_bar();
void draw_message_bar();
void set_status_message(const char *fmt, ...);
void move_cursor(int key);
void process_keypress();
void scroll();
void update_screen();
void draw_rows();
void draw_row(int row);
void draw_welcome_message();
void draw_status_message();
void clear_screen();
void position_cursor(int row, int col);

/* Global variables */
struct editor_state {
    int cx, cy;
    int screenrows;
    int screencols;
    int numrows;
    char *statusmsg;
    time_t statusmsg_time;
    char *filename;
    char statusmsg_buf[80];
} E;

/* Terminal handling functions */
void die(const char *s) {
    clear_screen();
    perror(s);
    exit(1);
}

void disable_raw_mode() {
    if (is_raw_mode) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        is_raw_mode = 0;
    }
}

void enable_raw_mode() {
    if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) die("tcgetattr");
    atexit(disable_raw_mode);

    struct termios raw = orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
    is_raw_mode = 1;
}

int get_cursor_position(int *rows, int *cols) {
    char buf[32];
    int i = 0;

    if (write(STDOUT_FILENO, ESC "[6n", 4) != 4) return -1;

    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    if (buf[0] != ESC || buf[1] != '[') return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

    return 0;
}

int get_window_size(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, ESC "[999C" ESC "[999B", 12) != 12) return -1;
        return get_cursor_position(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

void init_terminal() {
    if (get_window_size(&E.screenrows, &E.screencols) == -1) die("get_window_size");
    E.screenrows -= 2;
}

void handle_keypress() {
    char c;
    int nread;

    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }

    switch (c) {
        case 'q':
            clear_screen();
            exit(0);
            break;
        case 'h':
        case 'j':
        case 'k':
        case 'l':
            move_cursor(c);
            break;
    }
}

void refresh_screen() {
    scroll();
    clear_screen();
    draw_rows();
    draw_status_bar();
    draw_message_bar();
    position_cursor(E.cy, E.cx);
    fflush(stdout);
}

void draw_status_bar() {
    char status[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines",
        E.filename ? E.filename : "[No Name]", E.numrows);

    if (len > E.screencols) len = E.screencols;

    printf(REVERSE "%.*s" NORMAL, len, status);

    while (len < E.screencols) {
        printf(" ");
        len++;
    }
}

void draw_message_bar() {
    printf(CLEAR_LINE);
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && time(NULL) - E.statusmsg_time < 5)
        printf("%.*s", msglen, E.statusmsg);
}

void set_status_message(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg_buf, sizeof(E.statusmsg_buf), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

void move_cursor(int key) {
    switch (key) {
        case 'h':
            if (E.cx != 0) E.cx--;
            break;
        case 'j':
            if (E.cy < E.screenrows - 1) E.cy++;
            break;
        case 'k':
            if (E.cy != 0) E.cy--;
            break;
        case 'l':
            if (E.cx < E.screencols - 1) E.cx++;
            break;
    }
}

void scroll() {
    if (E.cy < 0) E.cy = 0;
    if (E.cy >= E.screenrows) E.cy = E.screenrows - 1;
    if (E.cx < 0) E.cx = 0;
    if (E.cx >= E.screencols) E.cx = E.screencols - 1;
}

void update_screen() {
    scroll();
    refresh_screen();
}

void draw_rows() {
    int y;
    for (y = 0; y < E.screenrows; y++) {
        if (y == E.screenrows / 3 && E.numrows == 0) {
            draw_welcome_message();
        } else {
            draw_row(y);
        }
    }
}

void draw_row(int row) {
    int len = E.screencols;
    while (len--) printf("~");
    printf("\r\n");
}

void draw_welcome_message() {
    char welcome[80];
    int welcomelen = snprintf(welcome, sizeof(welcome),
        "Duodyne Editor -- version %s", "0.0.1");
    if (welcomelen > E.screencols) welcomelen = E.screencols;
    int padding = (E.screencols - welcomelen) / 2;
    if (padding) {
        printf("~");
        padding--;
    }
    while (padding--) printf(" ");
    printf("%s", welcome);
    printf("\r\n");
}

void clear_screen() {
    printf(CLEAR HOME);
}

void position_cursor(int row, int col) {
    printf(ESC "[%d;%dH", row + 1, col + 1);
}

int main(int argc, char *argv[]) {
    enable_raw_mode();
    init_terminal();

    if (argc >= 2) {
        E.filename = argv[1];
    }

    set_status_message("HELP: Ctrl-Q = quit");

    while (1) {
        refresh_screen();
        handle_keypress();
    }

    return 0;
}
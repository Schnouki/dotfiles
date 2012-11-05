#include <stdlib.h>
#include <xcb/xcb.h>
#include <xcb/screensaver.h>

static xcb_connection_t* conn = NULL;
static const xcb_setup_t* setup = NULL;
static xcb_screen_t* scr = NULL;

void init() {
    if (!conn)
        conn = xcb_connect(NULL, NULL);
    if (!setup)
        setup = xcb_get_setup(conn);
    if (!scr)
        scr = xcb_setup_roots_iterator(setup).data;
}

uint32_t idle() {
    xcb_screensaver_query_info_cookie_t cookie;
    xcb_screensaver_query_info_reply_t *info;
    uint32_t ms_since_user_input;

    if (!conn || !scr)
        init();

    cookie = xcb_screensaver_query_info(conn, scr->root);
    info = xcb_screensaver_query_info_reply(conn, cookie, NULL);
    ms_since_user_input = info->ms_since_user_input;
    free(info);

    return ms_since_user_input;
}

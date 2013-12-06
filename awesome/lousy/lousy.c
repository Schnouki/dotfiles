#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include <xcb/xcb.h>
#include <xcb/xcb_util.h>
#include <xcb/xkb.h>
#include <xcb/xproto.h>
#include <xcb/randr.h>
#include <xcb/screensaver.h>

#include "lousy.h"

static xcb_connection_t* conn = NULL;
static const xcb_setup_t* setup = NULL;
static xcb_screen_t* scr = NULL;
static bool xkb_initialized = false;
static bool xkb_supported = false;

bool do_backlight(xcb_connection_t* conn, const xcb_setup_t* setup, double* in_new_pct, double* out_cur_pct);

void init() {
    if (!conn)
        conn = xcb_connect(NULL, NULL);
    if (!setup)
        setup = xcb_get_setup(conn);
    if (!scr)
        scr = xcb_setup_roots_iterator(setup).data;
    if (!xkb_initialized) {
        const xcb_query_extension_reply_t *extreply;

        extreply = xcb_get_extension_data(conn, &xcb_xkb_id);
        xkb_initialized = true;
        if (extreply->present) {
            xcb_xkb_use_extension_cookie_t cookie =
                xcb_xkb_use_extension(conn, XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION);
            xcb_xkb_use_extension_reply_t* reply =
                xcb_xkb_use_extension_reply(conn, cookie, NULL);
            xkb_supported = reply->supported;
        }
    }
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

double get_backlight() {
    double cur = 0;
    if (!conn || !setup)
        init();
    do_backlight(conn, setup, NULL, &cur);
    return cur;
}

void set_backlight(double new_pct) {
    if (!conn || !setup)
        init();
    do_backlight(conn, setup, &new_pct, NULL);
}

uint32_t keyboard_indicators() {
    xcb_xkb_get_indicator_state_cookie_t cookie;
    xcb_xkb_get_indicator_state_reply_t* reply = NULL;

    if (!conn || !xkb_initialized)
        init();
    if (!xkb_supported)
        return 0;

    cookie = xcb_xkb_get_indicator_state(conn, XCB_XKB_ID_USE_CORE_KBD);
    reply = xcb_xkb_get_indicator_state_reply(conn, cookie, NULL);
    if (reply)
        return reply->state;
    else
        return 0;
}

uint8_t get_caps_lock() {
    return keyboard_indicators() & (1 << 0);
}
uint8_t get_num_lock() {
    return keyboard_indicators() & (1 << 1);
}

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
static bool randr_initialized = false;
static bool randr_supported = false;

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
        if (extreply && extreply->present) {
            xcb_xkb_use_extension_cookie_t cookie =
                xcb_xkb_use_extension(conn, XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION);
            xcb_xkb_use_extension_reply_t* reply =
                xcb_xkb_use_extension_reply(conn, cookie, NULL);
            xkb_supported = reply && reply->supported;
        }
    }
    if (!randr_initialized) {
        const xcb_query_extension_reply_t *extreply;

        extreply = xcb_get_extension_data(conn, &xcb_randr_id);
        randr_initialized = true;
        if (extreply && extreply->present) {
            xcb_randr_query_version_cookie_t cookie =
                xcb_randr_query_version(conn, XCB_RANDR_MAJOR_VERSION, XCB_RANDR_MINOR_VERSION);
            xcb_randr_query_version_reply_t *version =
                xcb_randr_query_version_reply(conn, cookie, NULL);
            randr_supported = version && (version->major_version > 1 ||
                                          (version->major_version == 1 && version->minor_version >= 2));
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

uint8_t count_connected_displays() {
    uint8_t connected = 0;
    xcb_randr_get_screen_resources_cookie_t screen_res_cookie;
    xcb_randr_get_screen_resources_reply_t *screen_res_info;
    xcb_randr_crtc_t *crtcs;
    xcb_randr_get_crtc_info_cookie_t crtc_cookie;
    xcb_randr_get_crtc_info_reply_t *crtc_info;
    xcb_randr_output_t *outputs;
    int nb_outputs;
    xcb_randr_get_output_info_cookie_t output_cookie;
    xcb_randr_get_output_info_reply_t *output_info;
    int i, j;

    if (!conn || !randr_initialized)
        init();
    if (!randr_supported)
        return 0;

    /* Get screen resources */
    screen_res_cookie = xcb_randr_get_screen_resources(conn, scr->root);
    screen_res_info = xcb_randr_get_screen_resources_reply(conn, screen_res_cookie, NULL);
    if (!screen_res_info || screen_res_info->num_crtcs < 1)
        return 0;

    /* Get all the CRTCs */
    crtcs = xcb_randr_get_screen_resources_crtcs(screen_res_info);
    for (i = 0; i < screen_res_info->num_crtcs; i++) {
        crtc_cookie = xcb_randr_get_crtc_info(conn, crtcs[i], screen_res_info->config_timestamp);
        crtc_info = xcb_randr_get_crtc_info_reply(conn, crtc_cookie, NULL);
        if (crtc_info->mode != XCB_NONE) {
            /* CRTC is active: get all its outputs */
            nb_outputs = xcb_randr_get_crtc_info_outputs_length(crtc_info);
            outputs = xcb_randr_get_crtc_info_outputs(crtc_info);

            /* Check if each output is connected */
            for (j = 0; j < nb_outputs; j++) {
                output_cookie = xcb_randr_get_output_info(conn, outputs[j], XCB_CURRENT_TIME);
                output_info = xcb_randr_get_output_info_reply(conn, output_cookie, NULL);
                if (!output_info)
                    continue;
                if (output_info->connection == XCB_RANDR_CONNECTION_CONNECTED)
                    connected += 1;
                free(output_info);
            }
        }
        free(crtc_info);
    }

    //free(crtcs);
    free(screen_res_info);

    return connected;
}

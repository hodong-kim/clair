/*
 * clair.h
 * Copyright (C) 2021-2026 Hodong Kim <hodong@nimfsoft.art>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef CLAIR_H
#define CLAIR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct clair_event_loop_opaque clair_event_loop_t;

typedef struct clair_event_source_opaque clair_event_source_t;

typedef long long clair_ms_t;

typedef int clair_fd_t;

typedef unsigned int clair_event_mask_t;

#define CLAIR_INFINITE  ((clair_ms_t) -1)
#define CLAIR_IMMEDIATE ((clair_ms_t)  0)

#define CLAIR_EVENT_INPUT   0x001
#define CLAIR_EVENT_OUTPUT  0x004
#define CLAIR_EVENT_ERROR   0x008
#define CLAIR_EVENT_HANG_UP 0x010

typedef void (*clair_io_callback_t)     (clair_event_source_t* io,
                                         clair_fd_t            fd,
                                         clair_event_mask_t    events,
                                         void*                 user_data);

typedef void (*clair_timer_callback_t)  (clair_event_source_t* timer,
                                         void*                 user_data);

typedef void (*clair_idle_callback_t)   (clair_event_source_t* idle,
                                         void*                 user_data);

typedef void (*clair_signal_callback_t) (clair_event_source_t* signal,
                                         int                   signum,
                                         void*                 user_data);

clair_event_loop_t* clair_event_loop_create (void);
int clair_event_loop_destroy (clair_event_loop_t* loop);

int clair_event_loop_initialize (clair_event_loop_t* loop);
int clair_event_loop_finalize (clair_event_loop_t* loop);

int clair_event_loop_iterate (clair_event_loop_t* loop, clair_ms_t timeout);

int clair_event_loop_run (clair_event_loop_t* loop);

int clair_event_loop_quit (clair_event_loop_t* loop);

int clair_event_loop_get_depth (clair_event_loop_t* loop);

clair_event_source_t*
clair_event_loop_add_watch (clair_event_loop_t* loop,
                            clair_fd_t          fd,
                            clair_event_mask_t  events,
                            clair_io_callback_t callback,
                            void*               user_data);

clair_event_source_t*
clair_event_loop_add_timer (clair_event_loop_t*    loop,
                            clair_ms_t             interval,
                            clair_timer_callback_t callback,
                            bool                   one_shot,
                            void*                  user_data);

clair_event_source_t*
clair_event_loop_add_idle (clair_event_loop_t* loop,
                           clair_idle_callback_t callback,
                           void* user_data);

clair_event_source_t*
clair_event_loop_add_unix_signal (clair_event_loop_t*     loop,
                                  int                     signum,
                                  clair_signal_callback_t callback,
                                  void*                   user_data);

int clair_event_loop_remove (clair_event_loop_t*   loop,
                             clair_event_source_t* target);

int clair_event_loop_modify_watch (clair_event_loop_t*   loop,
                                   clair_event_source_t* watch,
                                   clair_event_mask_t    events);

#ifdef __cplusplus
}
#endif

#endif /* CLAIR_H */

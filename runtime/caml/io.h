/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Buffered input/output */

#ifndef CAML_IO_H
#define CAML_IO_H

#ifdef CAML_INTERNALS

#include "camlatomic.h"
#include "misc.h"
#include "mlvalues.h"
#include "platform.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 65536
#endif

#if defined(_WIN32)
typedef __int64 file_offset;
#else
#include <sys/types.h>
typedef off_t file_offset;
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  caml_plat_mutex mutex;        /* Mutex protecting buffer */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  uintnat refcount;             /* Number of custom blocks owning the channel */
  int flags;                    /* Bitfield */
  char * buff;                  /* The buffer */
  char * name;                  /* Optional name (to report fd leaks) */
};

enum {
  CHANNEL_FLAG_FROM_SOCKET = 1,  /* For Windows */
  CHANNEL_FLAG_MANAGED_BY_GC = 4,  /* Free and close using GC finalization. */
  /* Note: For backwards-compatibility, channels without the flag
     CHANNEL_FLAG_MANAGED_BY_GC can be used inside single-threaded
     programs without locking. As a consequence, using such a channel
     from an asynchronous callback can result in deadlocks. */
  CHANNEL_TEXT_MODE = 8, /* "Text mode" for Windows and Cygwin */
  CHANNEL_FLAG_UNBUFFERED = 16     /* Unbuffered (for output channels only) */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Creating and closing channels from C */

CAMLextern struct channel * caml_open_descriptor_in (int);
CAMLextern struct channel * caml_open_descriptor_out (int);
CAMLextern void caml_close_channel (struct channel *);
CAMLextern file_offset caml_channel_size (struct channel *);
CAMLextern void caml_seek_in (struct channel *, file_offset);
CAMLextern void caml_seek_out (struct channel *, file_offset);
CAMLextern file_offset caml_pos_in (struct channel *);
CAMLextern file_offset caml_pos_out (struct channel *);

/* I/O on channels from C. The channel must be locked (see below) before
   calling any of the functions and macros below */

CAMLextern value caml_alloc_channel(struct channel *chan);
CAMLextern int caml_channel_binary_mode (struct channel *);

CAMLextern int caml_flush_partial (struct channel *);
CAMLextern void caml_flush (struct channel *);
CAMLextern void caml_flush_if_unbuffered (struct channel *);
CAMLextern void caml_putch(struct channel *, int);
CAMLextern void caml_putword (struct channel *, uint32_t);
CAMLextern int caml_putblock (struct channel *, char *, intnat);
CAMLextern void caml_really_putblock (struct channel *, char *, intnat);

CAMLextern unsigned char caml_refill (struct channel *);
CAMLextern unsigned char caml_getch(struct channel *);
CAMLextern uint32_t caml_getword (struct channel *);
CAMLextern int caml_getblock (struct channel *, char *, intnat);
CAMLextern intnat caml_really_getblock (struct channel *, char *, intnat);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* The locking machinery */

CAMLextern void caml_channel_lock(struct channel *);
CAMLextern void caml_channel_unlock(struct channel *);

/* Lock and Unlock are compatibility aliases for OCaml<5.2.
   Remove whenever 5.2 is old enough. (See #12792.) */
#define Lock(channel) caml_channel_lock(channel)
#define Unlock(channel) caml_channel_unlock(channel)

CAMLextern void caml_channel_cleanup_on_raise(void);

CAMLextern struct channel * caml_all_opened_channels;
CAMLextern caml_plat_mutex caml_all_opened_channels_mutex;

/* Conversion between file_offset and int64_t */

#define Val_file_offset(fofs) caml_copy_int64(fofs)
#define File_offset_val(v) ((file_offset) Int64_val(v))

/* Primitives required by the Unix library */
CAMLextern value caml_ml_open_descriptor_in(value fd);
CAMLextern value caml_ml_open_descriptor_out(value fd);
CAMLextern value caml_ml_open_descriptor_in_with_flags(int fd, int flags);
CAMLextern value caml_ml_open_descriptor_out_with_flags(int fd, int flags);

#endif /* CAML_INTERNALS */

#endif /* CAML_IO_H */

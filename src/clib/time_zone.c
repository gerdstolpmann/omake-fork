/*
 *  Retrieve time zone information.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2020 Chris Spiel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, version 2.1 of the
 * License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 *
 * Author: Chris Spiel @email{cspiel@users.sourceforge.org}
 * @end[license]
 */


#include <time.h>             /* daylight, timezone, tzname */

#define CAML_NAME_SPACE
#include <caml/alloc.h>       /* caml_alloc(), caml_copy_string() */
#include <caml/memory.h>      /* CAMLlocal1(), CAMLparam0(), CAMLreturn(), Store_field() */


CAMLprim value
get_time_zone()
{
    enum {name, zone, is_daylight_saving, number_of_fields_};

    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(number_of_fields_, 0);

#if defined(_POSIX_C_SOURCE)
    Store_field(result, name, caml_copy_string(*tzname ? *tzname : ""));
#else
    Store_field(result, name, caml_copy_string(""));
#endif

#if defined(_DEFAULT_SOURCE) || defined(_SVID_SOURCE) || defined(_XOPEN_SOURCE)
    Store_field(result, zone, Val_long(timezone));
    Store_field(result, is_daylight_saving, Val_bool(daylight));
#else
    Store_field(result, zone, Val_long(0L));
    Store_field(result, is_daylight_saving, Val_bool(0));
#endif

    CAMLreturn(result);
}

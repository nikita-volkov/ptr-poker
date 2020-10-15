/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 *
 * Portions copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>


uint8_t* encode_text
(
  uint8_t *dest,
  const uint16_t *src,
  size_t src_offset,
  size_t src_length
)
{

  src += src_offset;
  
  const uint16_t *src_end = src + src_length;

  while (src < src_end) {
    uint16_t x = *src++;

    if (x <= 0x7F) {
      *dest++ = x;
    }
    else if (x <= 0x7FF) {
      *dest++ = (x >> 6) | 0xC0;
      *dest++ = (x & 0x3f) | 0x80;
    }
    else if (x < 0xD800 || x > 0xDBFF) {
      *dest++ = (x >> 12) | 0xE0;
      *dest++ = ((x >> 6) & 0x3F) | 0x80;
      *dest++ = (x & 0x3F) | 0x80;
    } else {
      uint32_t c =
        ((((uint32_t) x) - 0xD800) << 10) + 
        (((uint32_t) *src++) - 0xDC00) + 0x10000;
      *dest++ = (c >> 18) | 0xF0;
      *dest++ = ((c >> 12) & 0x3F) | 0x80;
      *dest++ = ((c >> 6) & 0x3F) | 0x80;
      *dest++ = (c & 0x3F) | 0x80;
    }
  }

  return dest;
}

int count_text_allocation_size
(
  const uint16_t *src_ptr,
  size_t src_off,
  size_t src_len
)
{
  src_ptr += src_off;
  const uint16_t *end_ptr = src_ptr + src_len;

  size_t size = 0;

  while (src_ptr < end_ptr) {
    uint16_t w = *src_ptr++;

    if (w <= 0x7F) {
      size += 1;
    }
    else if (w <= 0x7FF) {
      size += 2;
    }
    else if (w < 0xD800 || w > 0xDBFF) {
      size += 3;
    } else {
      src_ptr++;
      size += 4;
    }
  }

  return size;
}

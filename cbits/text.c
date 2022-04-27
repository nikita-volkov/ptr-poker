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

#if defined(__x86_64__)
#include <xmmintrin.h>
#include <emmintrin.h>
#endif

uint8_t* encode_text
(
  uint8_t *dest,
  const uint16_t *src,
  size_t src_offset,
  size_t src_length
)
#if defined(__x86_64__)
{
  src += src_offset;
  
  const uint16_t* src_end = src + src_length;
  const uint16_t* full_xmm_src_end = src_end - 8;

  while (src < src_end) {
    uint16_t x = *src++;

    if (x <= 0x7F) {
      *dest++ = x;

      while (src < full_xmm_src_end) {
        union { uint64_t halves[2]; __m128i whole; } eight_chars;
        eight_chars.whole = _mm_loadu_si128((__m128i *) src);

        const uint64_t h1 = eight_chars.halves[0];
        const uint64_t non_asciiness_of_the_first_half = h1 & 0xFF80FF80FF80FF80ULL;

        if (non_asciiness_of_the_first_half) {
          break;
        }

        const __m128i eight_ascii_chars = _mm_packus_epi16(eight_chars.whole, eight_chars.whole);
        _mm_storel_epi64((__m128i *)dest, eight_ascii_chars);

        const uint64_t non_asciiness_of_the_second_half = eight_chars.halves[1] & 0xFF80FF80FF80FF80ULL;
        if (non_asciiness_of_the_second_half) {
          const int ascii_char_count = __builtin_ctz(non_asciiness_of_the_second_half) / 16;
          dest += 4 + ascii_char_count;
          src += 4 + ascii_char_count;
          break;
        }

        dest += 8;
        src += 8;
      }
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
#elif defined(__i386__) || defined(__arm64__)
{

  src += src_offset;
  
  const uint16_t* src_end = src + src_length;
  const uint16_t* full_word_src_end = src_end - 2;

  while (src < src_end) {
    uint16_t x = *src++;

    if (x <= 0x7F) {
      *dest++ = x;

      while (src < full_word_src_end) {
        uint32_t x = *((uint32_t *) src);

        if (x & 0xFF80FF80)
          break;
        *dest++ = x & 0xFFFF;
        *dest++ = x >> 16;
        src += 2;
      }

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
#endif


int count_text_allocation_size
(
  const uint16_t *src_ptr,
  size_t src_off,
  size_t src_len
)
{
  src_ptr += src_off;

  const uint16_t* after_src_ptr = src_ptr + src_len;
  const uint16_t* full_word_after_src_ptr = after_src_ptr - 2;

  size_t size = 0;

  while (src_ptr < after_src_ptr) {
    uint16_t w = *src_ptr++;

    if (w <= 0x7F) {
      size += 1;

      /* Try to go in batches of 2 bytes. */
      while (src_ptr < full_word_after_src_ptr) {
        uint32_t x = *((uint32_t *) src_ptr);

        if (x & 0xFF80FF80) break;
        
        size += 2;
        src_ptr += 2;
      }

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

/*
 * Fudge over differences between K&R(I) and ANSI C.  We won't have
 * <stdc.h> available until BSD 4.4 or so...
 */

#if !defined(__STDC__) && !defined(volatile)
#define volatile
#define const
#define signed
#endif

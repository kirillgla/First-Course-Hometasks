#ifndef TASK6_LINEEND_H
#define TASK6_LINEEND_H

/**
 * This is a flag that tells what to use as as line break:
 * "\r\n" if definded, "\n" otherwise.
 */
#ifdef CRLF
#  error "I didn't expect CRLF to be defined"
#else // CRLF
#  define CRLF
#endif // CRLF

// #undef CRLF

#if defined(END_OF_LINE) || defined(EOL_LENGTH)
#  ifdef END_OF_LINE
#    error "I didn't expect END_OF_LINE to be defined"
#  endif
#  error "I didn't expect EOL_LENGTH to be defined"
#else // END_OF_LINE || EOL_LENGTH
#  ifdef CRLF
#    define END_OF_LINE "\r\n"
#    define EOL_LENGTH 2
#  else // CRLF
#    define END_OF_LINE "\n"
#    define EOL_LENGTH 1
#  endif // CRLF
#endif // END_OF_LINE || EOL_LENGTH

#endif // TASK6_LINEEND_H

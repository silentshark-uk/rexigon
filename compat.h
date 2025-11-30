#ifndef COMPAT_H
#define COMPAT_H

#include <stdint.h>

// Compiler detection
#ifdef _MSC_VER
    #define COMPILER_MSVC 1
    #define COMPILER_GCC 0
#elif defined(__GNUC__)
    #define COMPILER_MSVC 0
    #define COMPILER_GCC 1
#else
    #define COMPILER_MSVC 0
    #define COMPILER_GCC 0
#endif

// MSVC compatibility for builtin functions
#if COMPILER_MSVC
    #include <intrin.h>
    
    // Count trailing zeros (equivalent to __builtin_ctzll)
    static __forceinline int ctz64(uint64_t x) {
        unsigned long index;
        #ifdef _WIN64
            _BitScanForward64(&index, x);
        #else
            if ((uint32_t)x) {
                _BitScanForward(&index, (uint32_t)x);
            } else {
                _BitScanForward(&index, (uint32_t)(x >> 32));
                index += 32;
            }
        #endif
        return (int)index;
    }
    
    // Population count (equivalent to __builtin_popcountll)
    static __forceinline int popcount64(uint64_t x) {
        #ifdef _WIN64
            return (int)__popcnt64(x);
        #else
            return (int)(__popcnt((uint32_t)x) + __popcnt((uint32_t)(x >> 32)));
        #endif
    }
    
    #define builtin_ctzll(x) ctz64(x)
    #define builtin_popcountll(x) popcount64(x)
    
#elif COMPILER_GCC
    #define builtin_ctzll(x) __builtin_ctzll(x)
    #define builtin_popcountll(x) __builtin_popcountll(x)
#else
    // Fallback implementations for other compilers
    static int ctz64_fallback(uint64_t x) {
        int count = 0;
        if (!x) return 64;
        while (!(x & 1)) {
            x >>= 1;
            count++;
        }
        return count;
    }
    
    static int popcount64_fallback(uint64_t x) {
        int count = 0;
        while (x) {
            count += x & 1;
            x >>= 1;
        }
        return count;
    }
    
    #define builtin_ctzll(x) ctz64_fallback(x)
    #define builtin_popcountll(x) popcount64_fallback(x)
#endif

// C99 compatibility for MSVC (older versions)
#if COMPILER_MSVC && _MSC_VER < 1900
    #define inline __inline
    #define restrict __restrict
#endif

#endif // COMPAT_H
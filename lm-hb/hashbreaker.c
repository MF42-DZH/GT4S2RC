#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Compile me with --std=c17 and -O2.

#define MAX_USERNAME_ARR 31

const uint32_t fnv1a_mod_inverse = 899433627u;
const uint32_t fnv1a_init_const = 0x811c9dc5u;

const char charset[] = "\0ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz 0123456789`'\\;,.[]/-=!@#$^&*()~|:<>?_+{}";
const size_t charset_size = sizeof(charset) / sizeof(char);

bool verify(uint32_t start_hash, const size_t to_test[MAX_USERNAME_ARR]) {
    uint32_t current = start_hash;
    for (size_t ix = 0u; to_test[ix] > 0u && ix < MAX_USERNAME_ARR; ++ix) {
        current = (current * fnv1a_mod_inverse) ^ ((uint32_t) charset[to_test[ix]]);
    }

    return current == fnv1a_init_const;
}

bool bruteforce(uint32_t start_hash, /* Out */ char result[MAX_USERNAME_ARR]) {
    size_t intermediate[MAX_USERNAME_ARR] = { 0u };
    size_t length = 0u;

    do {
        if (length > 0u && verify(start_hash, intermediate)) {
            // Reverse the result, as the algorithm above outputs the answer in reverse.
            for (size_t ix = 0; ix < length; ++ix) {
                result[(length - ix) - 1] = charset[intermediate[ix]];
            }

            return true;
        }

        for (size_t ix = 0; ix < MAX_USERNAME_ARR - 1; ++ix) {
            if (intermediate[ix] == 0u) {
                intermediate[ix] = 1u;

                length += 1;
                if (length < MAX_USERNAME_ARR) printf("[!] Now testing %zu-long usernames.\n", length);
                break;
            } else if (intermediate[ix] == charset_size - 1u) {
                intermediate[ix] = 1u;
            } else {
                intermediate[ix] += 1u;
                break;
            }
        }
    } while (intermediate[MAX_USERNAME_ARR - 1] == 0u);

    return false;
}

int main(void) {
    char result[MAX_USERNAME_ARR] = { '\0' };

    printf("Enter the hash to reverse (0 - 2^32-1): ");
    fflush(stdout);

    int64_t hash_intermediate = -1l;
    scanf_s("%" SCNi64, &hash_intermediate);

    if (hash_intermediate < 0 || hash_intermediate > ((int64_t) UINT32_MAX)) {
        puts("Invalid hash.");
        return 1;
    }

    if (bruteforce((uint32_t) hash_intermediate, result)) {
        printf("Found matching username (remove quotes): \"%s\"\n", result);
    } else {
        puts("No matching username found for hash.");
    }

    return 0;
}

#include <inttypes.h>
#include <stdio.h>

uint64_t read_int_c(void) {
    uint64_t n;
    scanf("%" SCNu64, &n);
    return n;
}

void print_int_c(uint64_t n) {
    printf("%" PRIu64 "\n", n);
}

void rust_main(void);

int main() {
    rust_main();
    return 0;
}
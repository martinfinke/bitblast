
#ifdef _WIN64
//define something for Windows (64-bit)
#elif _WIN32
//define something for Windows (32-bit)
#elif __APPLE__
#include "TargetConditionals.h"
#if TARGET_IPHONE_SIMULATOR
// iOS Simulator
#elif TARGET_OS_IPHONE
// iOS device
// main.m's main() calls init_tests().
#elif TARGET_OS_MAC
// Other kinds of Mac OS
#include "tests_main.h"
int main(int argc, char** argv) {
    return init_tests(argc, argv);
}
#else
// Unsupported platform
#endif
#elif __linux
// linux
#elif __unix // all unices not caught above
// Unix
#elif __posix
// POSIX
#endif


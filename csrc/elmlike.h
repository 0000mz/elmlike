#ifndef _ELMLIKE_H_
#define _ELMLIKE_H_

#if defined(__linux__) || defined(__APPLE__)
#define EXPORT __attribute__((visibility("default")))
#else
#define EXPORT __declspec(dllexport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

EXPORT void start_gui();
EXPORT void stop_gui();

#ifdef __cplusplus
}
#endif

#endif // _ELMLIKE_H_

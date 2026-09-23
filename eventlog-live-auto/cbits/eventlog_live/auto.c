#if !defined(__has_attribute) || !__has_attribute(__constructor__)
#error "C compiler does not support __attribute__((__constructor__))"
#endif

#include "auto.h"
#define EVENTLOG_LIVE_AUTO_H_IMPLEMENTATION
#include "auto.h"

__attribute__((__constructor__)) void eventlog_live_auto(void) {
  eventlog_live_auto_register();
}

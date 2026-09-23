#ifndef EVENTLOG_LIVE_AUTO_H
#define EVENTLOG_LIVE_AUTO_H

#include <stdio.h>
#include <stdlib.h>

void eventlog_live_auto_unregister(void);

void eventlog_live_auto_register(void);

#else
#ifdef EVENTLOG_LIVE_AUTO_H_IMPLEMENTATION

void eventlog_live_auto_unregister(void) {
  // Destructor.
  printf("Oh no, I crashed!\n");
}

void eventlog_live_auto_register(void) {
  // Constructor.
  printf("Hello, I'm a little car!\n");
  atexit(eventlog_live_auto_unregister);
}

#endif // EVENTLOG_LIVE_AUTO_H_IMPLEMENTATION
#endif // EVENTLOG_LIVE_AUTO_H

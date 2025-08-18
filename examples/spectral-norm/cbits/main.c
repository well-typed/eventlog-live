#include <stdlib.h>
#include <ghcversion.h>
#include <rts/PosixSource.h>
#include <Rts.h>
#include <HsFFI.h>
#include <eventlog_socket.h>

int main (int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;

    // Set the eventlog writer:
    conf.eventlog_writer = &NullEventLogWriter;

    // Enable RTS options:
    conf.rts_opts_enabled = RtsOptsAll;

    extern StgClosure ZCMain_main_closure;
    return hs_main(argc, argv, &ZCMain_main_closure, conf);
}

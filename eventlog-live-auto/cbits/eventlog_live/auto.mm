#import <Foundation/Foundation.h>
#include "auto.h"
#define EVENTLOG_LIVE_AUTO_H_IMPLEMENTATION
#include "auto.h"

@interface EventlogLiveAuto : NSObject
@end

@implementation EventlogLiveAuto : NSObject
+ (void)load {
  eventlog_live_auto_register();
}
@end

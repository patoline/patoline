#include <stdio.h>
#include <getopt.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/errno.h>
#include <sysexits.h>
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/IOCFPlugIn.h>
#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hid/IOHIDKeys.h>
#include <IOKit/hid/IOHIDUsageTables.h>
#include <CoreFoundation/CoreFoundation.h>
#include <Carbon/Carbon.h>
#import <Cocoa/Cocoa.h>

IOHIDElementCookie buttonNextID = 0;
IOHIDElementCookie buttonPreviousID = 0;
IOHIDElementCookie buttonUpID = 0;
IOHIDElementCookie buttonDownID = 0;

typedef struct cookie_struct
{
  IOHIDElementCookie gButtonCookie_SystemAppMenu;
  IOHIDElementCookie gButtonCookie_SystemMenuSelect;
  IOHIDElementCookie gButtonCookie_SystemMenuRight;
  IOHIDElementCookie gButtonCookie_SystemMenuLeft;
  IOHIDElementCookie gButtonCookie_SystemMenuUp;
  IOHIDElementCookie gButtonCookie_SystemMenuDown;
} *cookie_struct_t;

NSString* nightly=@"org.mozilla.nightly";


inline          void print_errmsg_if_io_err(int expr, char *msg);
inline          void print_errmsg_if_err(int expr, char *msg);
void            QueueCallbackFunction(void *target, IOReturn result,
                                      void *refcon, void *sender);
bool            addQueueCallbacks(IOHIDQueueInterface **hqi);
void            processQueue(IOHIDDeviceInterface **hidDeviceInterface,
                             cookie_struct_t cookies);
void            doRun(IOHIDDeviceInterface **hidDeviceInterface,
                      cookie_struct_t cookies);
cookie_struct_t getHIDCookies(IOHIDDeviceInterface122 **handle);
void            createHIDDeviceInterface(io_object_t hidDevice,
                                         IOHIDDeviceInterface ***hdi);
void            setupAndRun(void);

OSStatus
KeynoteChangeSlide(bool pressedDown,int keycode)
{
  NSArray* apps=
    [NSRunningApplication runningApplicationsWithBundleIdentifier: nightly];
  if([apps count]==0)
    apps=[NSRunningApplication runningApplicationsWithBundleIdentifier: @"org.mozilla.firefox"];
  if([apps count]==0)
    apps=[NSRunningApplication runningApplicationsWithBundleIdentifier: @"org.webkit.WebKit"];
  if([apps count]==0)
    apps=[NSRunningApplication runningApplicationsWithBundleIdentifier: @"org.webkit.nightly.WebKit"];
  if([apps count]==0)
    apps=[NSRunningApplication runningApplicationsWithBundleIdentifier: @"com.apple.Safari"];
  if([apps count]>0){
    NSRunningApplication* app=[apps objectAtIndex:0];
    ProcessSerialNumber psn = { kNoProcess, kNoProcess };
    GetProcessForPID(app.processIdentifier, &psn);
    if(pressedDown){
      CGEventRef keyDownEvent=CGEventCreateKeyboardEvent(NULL, keycode, true);
      CGEventPostToPSN(&psn, keyDownEvent);
      CFRelease(keyDownEvent);
    }else{
      CGEventRef keyUpEvent=CGEventCreateKeyboardEvent(NULL, keycode, false);
      CGEventPostToPSN(&psn, keyUpEvent);
      CFRelease(keyUpEvent);
    }
  }
  [apps release];
}

inline void
print_errmsg_if_io_err(int expr, char *msg)
{
  IOReturn err = (expr);

  if (err != kIOReturnSuccess) {
    fprintf(stderr, "*** %s - %s(%x, %d).\n", msg, mach_error_string(err),
            err, err & 0xffffff);
    fflush(stderr);
    exit(EX_OSERR);
  }
}

inline void
print_errmsg_if_err(int expr, char *msg)
{
  if (expr) {
    fprintf(stderr, "*** %s.\n", msg);
    fflush(stderr);
    exit(EX_OSERR);
  }
}

void
QueueCallbackFunction(void *target, IOReturn result, void *refcon, void *sender)
{
  HRESULT               ret = 0;
  AbsoluteTime          zeroTime = {0,0};
  IOHIDQueueInterface **hqi;
  IOHIDEventStruct      event;

  while (!ret) {
    hqi = (IOHIDQueueInterface **)sender;
    ret = (*hqi)->getNextEvent(hqi, &event, zeroTime, 0);
    if (!ret) {
      /*
      printf("%#lx %s\n", (long unsigned int)event.elementCookie,
             (event.value == 0) ? "depressed" : "pressed");
      */
      if (event.elementCookie == buttonNextID)
        KeynoteChangeSlide(event.value==0,124);
      else if (event.elementCookie == buttonPreviousID)
        KeynoteChangeSlide(event.value==0,123);
      else if (event.elementCookie == buttonUpID)
        KeynoteChangeSlide(event.value==0,125);
      else if (event.elementCookie == buttonDownID)
        KeynoteChangeSlide(event.value==0,126);
    }
  }
}

bool
addQueueCallbacks(IOHIDQueueInterface **hqi)
{
  IOReturn               ret;
  CFRunLoopSourceRef     eventSource;
  IOHIDQueueInterface ***privateData;

  privateData = malloc(sizeof(*privateData));
  *privateData = hqi;

  ret = (*hqi)->createAsyncEventSource(hqi, &eventSource);
  if (ret != kIOReturnSuccess)
    return false;

  ret = (*hqi)->setEventCallout(hqi, QueueCallbackFunction,
                                NULL, &privateData);
  if (ret != kIOReturnSuccess)
    return false;

  CFRunLoopAddSource(CFRunLoopGetCurrent(), eventSource,
                     kCFRunLoopDefaultMode);
  return true;
}

void
processQueue(IOHIDDeviceInterface **hidDeviceInterface, cookie_struct_t cookies)
{
  HRESULT               result;
  IOHIDQueueInterface **queue;
  queue = (*hidDeviceInterface)->allocQueue(hidDeviceInterface);
  if (!queue) {
    fprintf(stderr, "Failed to allocate event queue.\n");
    return;
  }
  (void)(*queue)->create(queue, 0, 8);
  (void)(*queue)->addElement(queue,
                             cookies->gButtonCookie_SystemAppMenu, 0);
  (void)(*queue)->addElement(queue,
                             cookies->gButtonCookie_SystemMenuSelect, 0);
  (void)(*queue)->addElement(queue,
                             cookies->gButtonCookie_SystemMenuRight, 0);
  (void)(*queue)->addElement(queue,
                             cookies->gButtonCookie_SystemMenuLeft, 0);
  (void)(*queue)->addElement(queue,
                             cookies->gButtonCookie_SystemMenuUp, 0);
  (void)(*queue)->addElement(queue,
                             cookies->gButtonCookie_SystemMenuDown, 0);
  addQueueCallbacks(queue);
  result = (*queue)->start(queue);
  CFRunLoopRun();
  result = (*queue)->stop(queue);
  result = (*queue)->dispose(queue);
  (*queue)->Release(queue);
}

void
doRun(IOHIDDeviceInterface **hidDeviceInterface, cookie_struct_t cookies)
{
  IOReturn ioReturnValue;
  ioReturnValue = (*hidDeviceInterface)->open(hidDeviceInterface, 1);
  processQueue(hidDeviceInterface, cookies);
  if (ioReturnValue == KERN_SUCCESS)
    ioReturnValue = (*hidDeviceInterface)->close(hidDeviceInterface);
  (*hidDeviceInterface)->Release(hidDeviceInterface);
}

cookie_struct_t
getHIDCookies(IOHIDDeviceInterface122 **handle)
{
  cookie_struct_t    cookies;
  IOHIDElementCookie cookie;
  CFTypeRef          object;
  long               number;
  long               usage;
  long               usagePage;
  CFArrayRef         elements;
  CFDictionaryRef    element;
  IOReturn           result;

  if ((cookies = (cookie_struct_t)malloc(sizeof(*cookies))) == NULL) {
    fprintf(stderr, "Failed to allocate cookie memory.\n");
    exit(1);
  }

  memset(cookies, 0, sizeof(*cookies));

  if (!handle || !(*handle))
    return cookies;

  result = (*handle)->copyMatchingElements(handle, NULL, &elements);

  if (result != kIOReturnSuccess) {
    fprintf(stderr, "Failed to copy cookies.\n");
    exit(1);
  }

  CFIndex i;
  for (i = 0; i < CFArrayGetCount(elements); i++) {
    element = CFArrayGetValueAtIndex(elements, i);
    object = (CFDictionaryGetValue(element, CFSTR(kIOHIDElementCookieKey)));
    if (object == 0 || CFGetTypeID(object) != CFNumberGetTypeID())
      continue;
    if(!CFNumberGetValue((CFNumberRef) object, kCFNumberLongType, &number))
      continue;
    cookie = (IOHIDElementCookie)number;
    object = CFDictionaryGetValue(element, CFSTR(kIOHIDElementUsageKey));
    if (object == 0 || CFGetTypeID(object) != CFNumberGetTypeID())
      continue;
    if (!CFNumberGetValue((CFNumberRef)object, kCFNumberLongType, &number))
      continue;
    usage = number;
    object = CFDictionaryGetValue(element,CFSTR(kIOHIDElementUsagePageKey));
    if (object == 0 || CFGetTypeID(object) != CFNumberGetTypeID())
      continue;
    if (!CFNumberGetValue((CFNumberRef)object, kCFNumberLongType, &number))
      continue;
    usagePage = number;

    if (usagePage == kHIDPage_GenericDesktop) {
      switch (usage) {
      case kHIDUsage_GD_SystemAppMenu:
        cookies->gButtonCookie_SystemAppMenu = cookie;
        break;
      case kHIDUsage_GD_SystemMenu:
        cookies->gButtonCookie_SystemMenuSelect = cookie;
        break;
      case kHIDUsage_GD_SystemMenuRight:
        buttonNextID = cookie;
        cookies->gButtonCookie_SystemMenuRight = cookie;
        break;
      case kHIDUsage_GD_SystemMenuLeft:
        buttonPreviousID = cookie;
        cookies->gButtonCookie_SystemMenuLeft = cookie;
        break;
      case kHIDUsage_GD_SystemMenuUp:
        buttonUpID = cookie;
        cookies->gButtonCookie_SystemMenuUp = cookie;
        break;
      case kHIDUsage_GD_SystemMenuDown:
        buttonDownID = cookie;
        cookies->gButtonCookie_SystemMenuDown = cookie;
        break;
      }
    }
  }
  return cookies;
}

void
createHIDDeviceInterface(io_object_t hidDevice, IOHIDDeviceInterface ***hdi)
{
  io_name_t             className;
  IOCFPlugInInterface **plugInInterface = NULL;
  HRESULT               plugInResult = S_OK;
  SInt32                score = 0;
  IOReturn              ioReturnValue = kIOReturnSuccess;

  ioReturnValue = IOObjectGetClass(hidDevice, className);

  print_errmsg_if_io_err(ioReturnValue, "Failed to get class name.");

  ioReturnValue = IOCreatePlugInInterfaceForService(
                                                    hidDevice,
                                                    kIOHIDDeviceUserClientTypeID,
                                                    kIOCFPlugInInterfaceID,
                                                    &plugInInterface,
                                                    &score);

  if (ioReturnValue != kIOReturnSuccess)
    return;

  plugInResult = (*plugInInterface)->QueryInterface(
                                                    plugInInterface,
                                                    CFUUIDGetUUIDBytes(kIOHIDDeviceInterfaceID),
                                                    (LPVOID)hdi);
  print_errmsg_if_err(plugInResult != S_OK,
                      "Failed to create device interface.\n");

  (*plugInInterface)->Release(plugInInterface);
}

void
setupAndRun(void)
{
  CFMutableDictionaryRef hidMatchDictionary = NULL;
  io_service_t           hidService = (io_service_t)0;
  io_object_t            hidDevice = (io_object_t)0;
  IOHIDDeviceInterface **hidDeviceInterface = NULL;
  IOReturn               ioReturnValue = kIOReturnSuccess;
  cookie_struct_t        cookies;
  hidMatchDictionary = IOServiceNameMatching("AppleIRController");
  hidService = IOServiceGetMatchingService(kIOMasterPortDefault,
                                           hidMatchDictionary);

  if (!hidService) {
    fprintf(stderr, "Apple Infrared Remote not found.\n");
    exit(1);
  }

  hidDevice = (io_object_t)hidService;

  createHIDDeviceInterface(hidDevice, &hidDeviceInterface);
  cookies = getHIDCookies((IOHIDDeviceInterface122 **)hidDeviceInterface);
  ioReturnValue = IOObjectRelease(hidDevice);
  print_errmsg_if_io_err(ioReturnValue, "Failed to release HID.");

  if (hidDeviceInterface == NULL) {
    fprintf(stderr, "No HID.\n");
    exit(1);
  }

  ioReturnValue = (*hidDeviceInterface)->open(hidDeviceInterface, 1);

  doRun(hidDeviceInterface, cookies);

  if (ioReturnValue == KERN_SUCCESS)
    ioReturnValue = (*hidDeviceInterface)->close(hidDeviceInterface);

  (*hidDeviceInterface)->Release(hidDeviceInterface);
}

int main (int argc, char **argv)
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  setupAndRun();
  return 0;
}

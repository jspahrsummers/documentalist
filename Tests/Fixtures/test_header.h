#import <Foundation/Foundation.h>

/** Im in ur NSStrings */
extern NSString * const DocumentingThingz;

/// sup
void this_is_a_function (BOOL totes);

/**
 * a
 * multi
 * line
 * comment * here
 */
@interface MyClass : NSObject

/// Lazily binds a _block_ to the *values* in the receiver.
///
/// This should only be used if you need to terminate the bind early, or close
/// over some state. -flattenMap: is more appropriate for all other cases.
///
/// block - A block returning a RACStreamBindBlock. This block will be invoked
///         each time the bound stream is re-evaluated. This block must not be
///         nil or return nil.
/// name - Someone's name
/// other - thing
///
/// Returns a new stream which represents the combined result of all lazy
/// applications of `block`.
- (void)emptyMethod;

@end

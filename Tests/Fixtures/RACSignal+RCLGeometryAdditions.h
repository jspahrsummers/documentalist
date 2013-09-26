//
//  RACSignal+RCLGeometryAdditions.h
//  ReactiveCocoaLayout
//
//  Created by Justin Spahr-Summers on 2012-12-12.
//  Copyright (c) 2012 GitHub. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <ReactiveCocoa/ReactiveCocoa.h>
#import "Fixtures/RACSignal.h"
// Adds geometry functions to RACSignal.
@interface RACSignal ()

/// Returns a signal which sends 0 and completes.
+ (RACSignal *)zero;

/// Invokes -divideWithAmount:padding:fromEdge: with a constant padding of 0.
///
/// words and stuff

@end

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

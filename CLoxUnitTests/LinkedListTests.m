//
//  LinkedListTests.m
//  CLoxUnitTests
//
//  Created by Dalton Claybrook on 8/13/21.
//

#import <XCTest/XCTest.h>
#include "linked_list.h"

@interface LinkedListTests : XCTestCase

@property (nonatomic, assign) LinkedList *subject;

@end

@implementation LinkedListTests

- (void)setUp {
    self.subject = LinkedListCreate();
}

- (void)tearDown {
    LinkedListDestroy(self.subject);
    self.subject = nil;
}

- (void)test_onCreation_countIsZero {
    XCTAssertEqual(LinkedListGetCount(self.subject), 0);
}

- (void)test_onInsertOne_countIsCorrect {
    LinkedListInsertStringCopy(self.subject, "Test string...", 0);
    XCTAssertEqual(LinkedListGetCount(self.subject), 1);
}

- (void)test_onDelete_countIsCorrect {
    LinkedListInsertStringCopy(self.subject, "string 1", 0);
    LinkedListInsertStringCopy(self.subject, "string 2", 0);
    LinkedListInsertStringCopy(self.subject, "string 3", 0);
    LinkedListInsertStringCopy(self.subject, "string 4", 0);
    LinkedListInsertStringCopy(self.subject, "string 5", 0);
    XCTAssertEqual(LinkedListGetCount(self.subject), 5);

    LinkedListDeleteString(self.subject, 2);
    XCTAssertEqual(LinkedListGetCount(self.subject), 4);
    LinkedListDeleteString(self.subject, 0);
    XCTAssertEqual(LinkedListGetCount(self.subject), 3);
}

- (void)test_insertAtStartWorksCorrectly {
    LinkedListInsertStringCopy(self.subject, "initial", 0);
    LinkedListInsertStringCopy(self.subject, "inserted", 0);
    XCTAssertTrue([[self getStringAtIndex:0] isEqualToString: @"inserted"]);
    XCTAssertTrue([[self getStringAtIndex:1] isEqualToString: @"initial"]);
}

- (void)test_insertAtEndWorksCorrectly {
    LinkedListInsertStringCopy(self.subject, "initial", 0);
    LinkedListInsertStringCopy(self.subject, "inserted", 1);
    XCTAssertTrue([[self getStringAtIndex:1] isEqualToString: @"inserted"]);
    XCTAssertTrue([[self getStringAtIndex:0] isEqualToString: @"initial"]);
}

- (void)test_insertInMiddleWorksCorrectly {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "last", 1);
    LinkedListInsertStringCopy(self.subject, "inserted", 1);
    XCTAssertTrue([[self getStringAtIndex:0] isEqualToString: @"first"]);
    XCTAssertTrue([[self getStringAtIndex:1] isEqualToString: @"inserted"]);
    XCTAssertTrue([[self getStringAtIndex:2] isEqualToString: @"last"]);
}

- (void)test_deleteFromStartWorksCorrectly {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "second", 1);
    LinkedListInsertStringCopy(self.subject, "third", 2);
    LinkedListDeleteString(self.subject, 0);

    XCTAssertTrue([[self getStringAtIndex:0] isEqualToString: @"second"]);
    XCTAssertTrue([[self getStringAtIndex:1] isEqualToString: @"third"]);
}

- (void)test_deleteFromEndWorksCorrectly {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "second", 1);
    LinkedListInsertStringCopy(self.subject, "third", 2);
    LinkedListDeleteString(self.subject, 2);

    XCTAssertTrue([[self getStringAtIndex:0] isEqualToString: @"first"]);
    XCTAssertTrue([[self getStringAtIndex:1] isEqualToString: @"second"]);
}

- (void)test_deleteFromMiddleWorksCorrectly {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "second", 1);
    LinkedListInsertStringCopy(self.subject, "third", 2);
    LinkedListDeleteString(self.subject, 1);

    XCTAssertTrue([[self getStringAtIndex:0] isEqualToString: @"first"]);
    XCTAssertTrue([[self getStringAtIndex:1] isEqualToString: @"third"]);
}

- (void)test_ifCannotFindString_notFoundIsReturned {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "second", 1);
    LinkedListInsertStringCopy(self.subject, "third", 2);
    int index = LinkedListFirstIndexOfString(self.subject, "foobar");
    XCTAssertEqual(index, LinkedListIndexNotFound);
}

- (void)test_ifStringFound_indexIsReturned {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "second", 1);
    LinkedListInsertStringCopy(self.subject, "third", 2);
    int index = LinkedListFirstIndexOfString(self.subject, "second");
    XCTAssertEqual(index, 1);
}

- (void)test_ifMultipleStringsMatch_firstIndexIsReturned {
    LinkedListInsertStringCopy(self.subject, "first", 0);
    LinkedListInsertStringCopy(self.subject, "foobar", 1);
    LinkedListInsertStringCopy(self.subject, "third", 2);
    LinkedListInsertStringCopy(self.subject, "foobar", 3);
    int index = LinkedListFirstIndexOfString(self.subject, "foobar");
    XCTAssertEqual(index, 1);
}

// MARK: - Helpers

- (NSString *)getStringAtIndex:(int)index {
    const char *string = LinkedListGetString(self.subject, index);
    return [[NSString alloc] initWithCString:string encoding:NSUTF8StringEncoding];
}

@end

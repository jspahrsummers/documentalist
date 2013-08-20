#include <clang-c/Index.h>

/*
 * Because the Haskell FFI doesn't handle plain structs (value types) well, we
 * wrap the Clang API so we can use pointers instead.
 *
 * The naming should be mostly consistent with the Clang API. Just replace
 * `doc_` with `clang_` and the original function should be easy to find.
 *
 * Any pointer return values must be free()d once they are no longer in use.
 */

CXCursor *doc_getTranslationUnitCursor(CXTranslationUnit unit);
char *doc_Cursor_getRawCommentText(const CXCursor *cursor);
unsigned doc_isDeclaration(const CXCursor *cursor);
CXSourceRange *doc_getCursorExtent(const CXCursor *cursor);
enum CXCursorKind doc_getCursorKind(const CXCursor *cursor);
CXCursor *doc_dupCursor(const CXCursor *cursor);
char *doc_getCursorSpelling(const CXCursor *cursor);

typedef enum CXChildVisitResult (*doc_CXCursorVisitor)(const CXCursor *cursor, const CXCursor *parent);
unsigned doc_visitChildren(const CXCursor *cursor, doc_CXCursorVisitor visitor);

CXToken *doc_tokenize(CXTranslationUnit unit, const CXSourceRange *range, unsigned *numTokens);
char **doc_getTokenSpellings(CXTranslationUnit unit, const CXToken *tokens, unsigned numTokens);

// Convenience method to deeply free the array of strings returned by
// doc_getTokenSpellings().
void doc_disposeTokenSpellings(char **spellings, unsigned numTokens);

CXSourceLocation *doc_getRangeStart(const CXSourceRange *range);
CXSourceLocation *doc_getRangeEnd(const CXSourceRange *range);
int doc_Range_isNull(const CXSourceRange *range);
CXFile *doc_getFileLocation(const CXSourceLocation *location, unsigned *line, unsigned *column, unsigned *offset);
char *doc_getFileName(const CXFile *file);

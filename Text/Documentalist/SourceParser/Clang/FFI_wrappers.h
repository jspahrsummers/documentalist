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
enum CXCursorKind doc_getCursorKind(const CXCursor *cursor);
CXCursor *doc_dupCursor(const CXCursor *cursor);
char *doc_getCursorSpelling(const CXCursor *cursor);
char *doc_getCursorFilename(const CXCursor *cursor);

CXType *doc_getCursorType(const CXCursor *cursor);
CXType *doc_getCursorResultType(const CXCursor *cursor);
CXType *doc_getTypedefDeclUnderlyingType(const CXCursor *cursor);
CXType *doc_getEnumDeclIntegerType(const CXCursor *cursor);
char *doc_getTypeSpelling(const CXType *type);

typedef enum CXChildVisitResult (*doc_CXCursorVisitor)(const CXCursor *cursor, const CXCursor *parent);
unsigned doc_visitChildren(const CXCursor *cursor, doc_CXCursorVisitor visitor);

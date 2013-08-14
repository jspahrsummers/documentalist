#include <clang-c/Index.h>

char *doc_Cursor_getRawCommentText(const CXCursor *cursor);
CXCursor *doc_getTranslationUnitCursor(CXTranslationUnit unit);
unsigned doc_isDeclaration(const CXCursor *cursor);
CXSourceRange *doc_getCursorExtent(const CXCursor *cursor);
CXToken *doc_tokenize(CXTranslationUnit unit, const CXSourceRange *range, unsigned *numTokens);
char **doc_getTokenSpellings(CXTranslationUnit unit, const CXToken *tokens, unsigned numTokens);
void doc_disposeTokenSpellings(char **spellings, unsigned numTokens);
CXSourceLocation *doc_getRangeStart(const CXSourceRange *range);
CXSourceLocation *doc_getRangeEnd(const CXSourceRange *range);
CXFile *doc_getFileLocation(const CXSourceLocation *location, unsigned *line, unsigned *column, unsigned *offset);
char *doc_getFileName(const CXFile *file);
int doc_Range_isNull(const CXSourceRange *range);
enum CXCursorKind doc_getCursorKind(const CXCursor *cursor);

typedef enum CXChildVisitResult (*doc_CXCursorVisitor)(const CXCursor *cursor, const CXCursor *parent);
unsigned doc_visitChildren(const CXCursor *cursor, doc_CXCursorVisitor visitor);
CXCursor *doc_dupCursor(const CXCursor *cursor);

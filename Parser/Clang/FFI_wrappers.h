#include <clang-c/Index.h>

char *doc_Cursor_getRawCommentText(const CXCursor *cursor);
CXCursor *doc_getTranslationUnitCursor(CXTranslationUnit unit);
unsigned doc_isDeclaration(const CXCursor *cursor);
CXSourceRange *doc_getCursorExtent(const CXCursor *cursor);
CXToken *doc_tokenize(CXTranslationUnit unit, const CXSourceRange *range, unsigned *numTokens);
char **doc_getTokenSpellings(CXTranslationUnit unit, const CXToken *tokens, unsigned numTokens);
void doc_disposeTokenSpellings(char **spellings, unsigned numTokens);

typedef enum CXChildVisitResult (*doc_CXCursorVisitor)(const CXCursor *cursor, const CXCursor *parent);
unsigned doc_visitChildren(const CXCursor *cursor, doc_CXCursorVisitor visitor);
CXCursor *doc_dupCursor(const CXCursor *cursor);

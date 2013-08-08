#include "FFI_wrappers.h"
#include <stdlib.h>
#include <string.h>

static char *doc_fromCXString(CXString str) {
	char *copy = strdup(clang_getCString(str));
	clang_disposeString(str);
	return copy;
}

#define doc_dupValue(VALUE) \
	({ \
		__typeof__(VALUE) *ptr = malloc(sizeof(*ptr)); \
		*ptr = (VALUE); \
		ptr; \
	})

char *doc_Cursor_getRawCommentText(const CXCursor *cursor) {
	return doc_fromCXString(clang_Cursor_getRawCommentText(*cursor));
}

CXCursor *doc_getTranslationUnitCursor(CXTranslationUnit unit) {
	return doc_dupValue(clang_getTranslationUnitCursor(unit));
}

unsigned doc_isDeclaration(const CXCursor *cursor) {
	return clang_isDeclaration(clang_getCursorKind(*cursor));
}

CXSourceRange *doc_getCursorExtent(const CXCursor *cursor) {
	return doc_dupValue(clang_getCursorExtent(*cursor));
}

CXToken *doc_tokenize(CXTranslationUnit unit, const CXSourceRange *range, unsigned *numTokens) {
	CXToken *tokens = NULL;
	clang_tokenize(unit, *range, &tokens, numTokens);
	return tokens;
}

char **doc_getTokenSpellings(CXTranslationUnit unit, const CXToken *tokens, unsigned numTokens) {
	char **spellings = malloc(sizeof(*spellings) * numTokens);
	for (unsigned i = 0; i < numTokens; i++) {
		spellings[i] = doc_fromCXString(clang_getTokenSpelling(unit, tokens[i]));
	}

	return spellings;
}

void doc_disposeTokenSpellings(char **spellings, unsigned numTokens) {
	for (unsigned i = 0; i < numTokens; i++) {
		free(spellings[i]);
	}

	free(spellings);
}

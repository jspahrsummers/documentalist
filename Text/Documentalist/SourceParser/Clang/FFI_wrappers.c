#include "FFI_wrappers.h"
#include <stdlib.h>
#include <string.h>

static char *doc_fromCXString(CXString str) {
	const char *original = clang_getCString(str);
	if (original == NULL) return NULL;

	size_t len = strlen(original);
	char *copy = malloc(len + 1);
	if (copy != NULL) {
		strncpy(copy, original, len);
		copy[len] = '\0';
	}

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

enum CXCursorKind doc_getCursorKind(const CXCursor *cursor) {
	return clang_getCursorKind(*cursor);
}

typedef struct {
	doc_CXCursorVisitor visitor;
} doc_visitorClientData;

static enum CXChildVisitResult doc_visitor(CXCursor cursor, CXCursor parent, CXClientData opaqueClientData) {
	const doc_visitorClientData *clientData = opaqueClientData;
	return clientData->visitor(&cursor, &parent);
}

unsigned doc_visitChildren(const CXCursor *cursor, doc_CXCursorVisitor visitor) {
	doc_visitorClientData clientData = { .visitor = visitor };
	return clang_visitChildren(*cursor, &doc_visitor, &clientData);
}

CXCursor *doc_dupCursor(const CXCursor *cursor) {
	CXCursor *copy = malloc(sizeof(*copy));
	*copy = *cursor;
	return copy;
}

char *doc_getCursorSpelling(const CXCursor *cursor) {
	return doc_fromCXString(clang_getCursorSpelling(*cursor));
}

char *doc_getCursorFilename(const CXCursor *cursor) {
	CXFile file;
	clang_getSpellingLocation(clang_getCursorLocation(*cursor), &file, NULL, NULL, NULL);

	return doc_fromCXString(clang_getFileName(file));
}

CXType *doc_getCursorType(const CXCursor *cursor) {
	return doc_dupValue(clang_getCursorType(*cursor));
}

CXType *doc_getCursorResultType(const CXCursor *cursor) {
	return doc_dupValue(clang_getCursorResultType(*cursor));
}

CXType *doc_getTypedefDeclUnderlyingType(const CXCursor *cursor) {
	return doc_dupValue(clang_getTypedefDeclUnderlyingType(*cursor));
}

CXType *doc_getEnumDeclIntegerType(const CXCursor *cursor) {
	return doc_dupValue(clang_getEnumDeclIntegerType(*cursor));
}

char *doc_getTypeSpelling(const CXType *type) {
	return doc_fromCXString(clang_getTypeSpelling(*type));
}

#ifndef RTags_h
#define RTags_h

#include "rct-config.h"
#include <rct/Path.h>

namespace RTags {

enum { CompilationError = -1, CompilationErrorXml = -2 };

void initMessages();
Path findProjectRoot(const Path &path);
}

#endif

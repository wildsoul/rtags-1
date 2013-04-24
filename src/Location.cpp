#include "Location.h"

Map<Path, uint32_t> Location::sPathsToIds;
Map<uint32_t, Path> Location::sIdsToPaths;
uint32_t Location::sLastId = 0;
ReadWriteLock Location::sLock;

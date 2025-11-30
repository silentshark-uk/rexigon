#include "chess.h"
#include <time.h>

void init_engine(void) {
    // Initialize random seed for Zobrist hashing
    srand((unsigned int)time(NULL));
    
    // Initialize Zobrist hash keys
    init_zobrist();
    
    // Initialize transposition table
    init_tt();
    
    // Don't print startup messages - Arena doesn't like them
}
#include "compat.h"
#include "chess.h"

// Move encoding and flags are now in chess.h

// File masks for pawn capture generation
#define FILE_A 0x0101010101010101ULL
#define FILE_H 0x8080808080808080ULL

// Direction vectors
static const int knight_dirs[8] = {-17, -15, -10, -6, 6, 10, 15, 17};
static const int king_dirs[8] = {-9, -8, -7, -1, 1, 7, 8, 9};
static const int bishop_dirs[4] = {-9, -7, 7, 9};
static const int rook_dirs[4] = {-8, -1, 1, 8};

// Pre-computed attack tables for fast lookup
static Bitboard knight_attacks[64];
static Bitboard king_attacks[64];
static Bitboard pawn_attacks[2][64]; // [color][square]

// Magic bitboard constants and tables for sliding pieces
#define ROOK_MAGIC_BITS 12
#define BISHOP_MAGIC_BITS 9

// Magic numbers (these are carefully chosen constants for perfect hashing)
static const Bitboard rook_magics[64] = {
    0x0080001020400080ULL, 0x0040001000200040ULL, 0x0080081000200080ULL, 0x0080040800100080ULL,
    0x0080020400080080ULL, 0x0080010200040080ULL, 0x0080008001000200ULL, 0x0080002040800100ULL,
    0x0000800020400080ULL, 0x0000400020005000ULL, 0x0000801000200080ULL, 0x0000800800100080ULL,
    0x0000800400080080ULL, 0x0000800200040080ULL, 0x0000800100020080ULL, 0x0000800040800100ULL,
    0x0000208000400080ULL, 0x0000404000201000ULL, 0x0000808010002000ULL, 0x0000808008001000ULL,
    0x0000808004000800ULL, 0x0000808002000400ULL, 0x0000010100020004ULL, 0x0000020000408104ULL,
    0x0000208080004000ULL, 0x0000200040005000ULL, 0x0000100080200080ULL, 0x0000080080100080ULL,
    0x0000040080080080ULL, 0x0000020080040080ULL, 0x0000010080800200ULL, 0x0000800080004100ULL,
    0x0000204000800080ULL, 0x0000200040401000ULL, 0x0000100080802000ULL, 0x0000080080801000ULL,
    0x0000040080800800ULL, 0x0000020080800400ULL, 0x0000020001010004ULL, 0x0000800040800100ULL,
    0x0000204000808000ULL, 0x0000200040008080ULL, 0x0000100020008080ULL, 0x0000080010008080ULL,
    0x0000040008008080ULL, 0x0000020004008080ULL, 0x0000010002008080ULL, 0x0000004081020004ULL,
    0x0000204000800080ULL, 0x0000200040008080ULL, 0x0000100020008080ULL, 0x0000080010008080ULL,
    0x0000040008008080ULL, 0x0000020004008080ULL, 0x0000800100020080ULL, 0x0000800041000080ULL,
    0x00FFFCDDFCED714AULL, 0x007FFCDDFCED714AULL, 0x003FFFCDFFD88096ULL, 0x0000040810002101ULL,
    0x0001000204080011ULL, 0x0001000204000801ULL, 0x0001000082000401ULL, 0x0001FFFAABFAD1A2ULL
};

static const Bitboard bishop_magics[64] = {
    0x0002020202020200ULL, 0x0002020202020000ULL, 0x0004010202000000ULL, 0x0004040080000000ULL,
    0x0001104000000000ULL, 0x0000821040000000ULL, 0x0000410410400000ULL, 0x0000104104104000ULL,
    0x0000040404040400ULL, 0x0000020202020200ULL, 0x0000040102020000ULL, 0x0000040400800000ULL,
    0x0000011040000000ULL, 0x0000008210400000ULL, 0x0000004104104000ULL, 0x0000002082082000ULL,
    0x0004000808080800ULL, 0x0002000404040400ULL, 0x0001000202020200ULL, 0x0000800802004000ULL,
    0x0000800400A00000ULL, 0x0000200100884000ULL, 0x0000400082082000ULL, 0x0000200041041000ULL,
    0x0002080010101000ULL, 0x0001040008080800ULL, 0x0000208004010400ULL, 0x0000404004010200ULL,
    0x0000840000802000ULL, 0x0000404002011000ULL, 0x0000808001041000ULL, 0x0000404000820800ULL,
    0x0001041000202000ULL, 0x0000820800101000ULL, 0x0000104400080800ULL, 0x0000020080080080ULL,
    0x0000404040040100ULL, 0x0000808100020100ULL, 0x0001010100020800ULL, 0x0000808080010400ULL,
    0x0000820820004000ULL, 0x0000410410002000ULL, 0x0000082088001000ULL, 0x0000002011000800ULL,
    0x0000080100400400ULL, 0x0001010101000200ULL, 0x0002020202000400ULL, 0x0001010101000200ULL,
    0x0000410410400000ULL, 0x0000208208200000ULL, 0x0000002084100000ULL, 0x0000000020880000ULL,
    0x0000001002020000ULL, 0x0000040408020000ULL, 0x0004040404040000ULL, 0x0002020202020000ULL,
    0x0000104104104000ULL, 0x0000002082082000ULL, 0x0000000020841000ULL, 0x0000000000208800ULL,
    0x0000000010020200ULL, 0x0000000404080200ULL, 0x0000040404040400ULL, 0x0002020202020200ULL
};

// Attack lookup tables
static Bitboard rook_attacks_table[64][4096];   // 64 squares * 2^12 occupancy patterns
static Bitboard bishop_attacks_table[64][512];  // 64 squares * 2^9 occupancy patterns

// Masks for relevant occupancy bits
static Bitboard rook_masks[64];
static Bitboard bishop_masks[64];

// Initialize attack tables (called once at startup)
void init_attack_tables(void) {
    static bool initialized = false;
    if (initialized) return;
    
    // Initialize knight attacks
    for (int sq = 0; sq < 64; sq++) {
        knight_attacks[sq] = 0;
        int rank = sq / 8;
        int file = sq % 8;
        
        for (int i = 0; i < 8; i++) {
            int new_sq = sq + knight_dirs[i];
            if (new_sq >= 0 && new_sq < 64) {
                int new_rank = new_sq / 8;
                int new_file = new_sq % 8;
                // Check for board wrapping
                if (abs(new_rank - rank) <= 2 && abs(new_file - file) <= 2) {
                    knight_attacks[sq] |= (1ULL << new_sq);
                }
            }
        }
    }
    
    // Initialize king attacks
    for (int sq = 0; sq < 64; sq++) {
        king_attacks[sq] = 0;
        int rank = sq / 8;
        int file = sq % 8;
        
        for (int i = 0; i < 8; i++) {
            int new_sq = sq + king_dirs[i];
            if (new_sq >= 0 && new_sq < 64) {
                int new_rank = new_sq / 8;
                int new_file = new_sq % 8;
                // Check for board wrapping
                if (abs(new_rank - rank) <= 1 && abs(new_file - file) <= 1) {
                    king_attacks[sq] |= (1ULL << new_sq);
                }
            }
        }
    }
    
    // Initialize pawn attacks
    for (int sq = 0; sq < 64; sq++) {
        int rank = sq / 8;
        int file = sq % 8;
        
        pawn_attacks[WHITE][sq] = 0;
        pawn_attacks[BLACK][sq] = 0;
        
        // White pawn attacks
        if (rank < 7) {
            if (file > 0) pawn_attacks[WHITE][sq] |= (1ULL << (sq + 7));
            if (file < 7) pawn_attacks[WHITE][sq] |= (1ULL << (sq + 9));
        }
        
        // Black pawn attacks
        if (rank > 0) {
            if (file > 0) pawn_attacks[BLACK][sq] |= (1ULL << (sq - 9));
            if (file < 7) pawn_attacks[BLACK][sq] |= (1ULL << (sq - 7));
        }
    }
    
    // Initialize magic bitboard tables
    init_magic_bitboards();
    
    initialized = true;
}

// Magic bitboard helper functions
static Bitboard generate_rook_attacks_slow(int square, Bitboard occupied) {
    Bitboard attacks = 0;
    int rank = square / 8;
    int file = square % 8;
    
    // Four straight directions
    int dirs[4][2] = {{1,0}, {-1,0}, {0,1}, {0,-1}};
    
    for (int d = 0; d < 4; d++) {
        for (int dist = 1; dist < 8; dist++) {
            int new_rank = rank + dirs[d][0] * dist;
            int new_file = file + dirs[d][1] * dist;
            
            if (new_rank < 0 || new_rank >= 8 || new_file < 0 || new_file >= 8) break;
            
            int new_sq = new_rank * 8 + new_file;
            attacks |= (1ULL << new_sq);
            
            if (occupied & (1ULL << new_sq)) break; // Blocked
        }
    }
    
    return attacks;
}

static Bitboard generate_bishop_attacks_slow(int square, Bitboard occupied) {
    Bitboard attacks = 0;
    int rank = square / 8;
    int file = square % 8;
    
    // Four diagonal directions
    int dirs[4][2] = {{1,1}, {1,-1}, {-1,1}, {-1,-1}};
    
    for (int d = 0; d < 4; d++) {
        for (int dist = 1; dist < 8; dist++) {
            int new_rank = rank + dirs[d][0] * dist;
            int new_file = file + dirs[d][1] * dist;
            
            if (new_rank < 0 || new_rank >= 8 || new_file < 0 || new_file >= 8) break;
            
            int new_sq = new_rank * 8 + new_file;
            attacks |= (1ULL << new_sq);
            
            if (occupied & (1ULL << new_sq)) break; // Blocked
        }
    }
    
    return attacks;
}

static Bitboard generate_rook_mask(int square) {
    Bitboard mask = 0;
    int rank = square / 8;
    int file = square % 8;
    
    // Horizontal (exclude edges)
    for (int f = 1; f < 7; f++) {
        if (f != file) {
            mask |= (1ULL << (rank * 8 + f));
        }
    }
    
    // Vertical (exclude edges)
    for (int r = 1; r < 7; r++) {
        if (r != rank) {
            mask |= (1ULL << (r * 8 + file));
        }
    }
    
    return mask;
}

static Bitboard generate_bishop_mask(int square) {
    Bitboard mask = 0;
    int rank = square / 8;
    int file = square % 8;
    
    // Four diagonal directions (exclude edges)
    int dirs[4][2] = {{1,1}, {1,-1}, {-1,1}, {-1,-1}};
    
    for (int d = 0; d < 4; d++) {
        for (int dist = 1; dist < 7; dist++) {
            int new_rank = rank + dirs[d][0] * dist;
            int new_file = file + dirs[d][1] * dist;
            
            if (new_rank <= 0 || new_rank >= 7 || new_file <= 0 || new_file >= 7) break;
            
            int new_sq = new_rank * 8 + new_file;
            mask |= (1ULL << new_sq);
        }
    }
    
    return mask;
}

void init_magic_bitboards(void) {
    // Initialize masks
    for (int sq = 0; sq < 64; sq++) {
        rook_masks[sq] = generate_rook_mask(sq);
        bishop_masks[sq] = generate_bishop_mask(sq);
    }
    
    // Initialize attack tables
    for (int sq = 0; sq < 64; sq++) {
        // Rook attacks
        Bitboard mask = rook_masks[sq];
        int bits = builtin_popcountll(mask);
        int occupancy_count = 1 << bits;
        
        for (int i = 0; i < occupancy_count; i++) {
            Bitboard occupancy = 0;
            int temp_i = i;
            Bitboard temp_mask = mask;
            
            // Generate occupancy pattern
            while (temp_mask) {
                int bit_pos = builtin_ctzll(temp_mask);
                if (temp_i & 1) {
                    occupancy |= (1ULL << bit_pos);
                }
                temp_i >>= 1;
                temp_mask &= temp_mask - 1;
            }
            
            // Calculate magic index
            int magic_index = (occupancy * rook_magics[sq]) >> (64 - ROOK_MAGIC_BITS);
            rook_attacks_table[sq][magic_index] = generate_rook_attacks_slow(sq, occupancy);
        }
        
        // Bishop attacks
        mask = bishop_masks[sq];
        bits = builtin_popcountll(mask);
        occupancy_count = 1 << bits;
        
        for (int i = 0; i < occupancy_count; i++) {
            Bitboard occupancy = 0;
            int temp_i = i;
            Bitboard temp_mask = mask;
            
            // Generate occupancy pattern
            while (temp_mask) {
                int bit_pos = builtin_ctzll(temp_mask);
                if (temp_i & 1) {
                    occupancy |= (1ULL << bit_pos);
                }
                temp_i >>= 1;
                temp_mask &= temp_mask - 1;
            }
            
            // Calculate magic index
            int magic_index = (occupancy * bishop_magics[sq]) >> (64 - BISHOP_MAGIC_BITS);
            bishop_attacks_table[sq][magic_index] = generate_bishop_attacks_slow(sq, occupancy);
        }
    }
}

// Get piece at square for specific color
static int get_piece_at(const Position* pos, int square, int color) {
    Bitboard sq_bb = 1ULL << square;
    for (int piece = PAWN; piece <= KING; piece++) {
        if (pos->pieces[color][piece] & sq_bb) {
            return piece;
        }
    }
    return 0;
}

// Optimized sliding piece attack generation using magic bitboards
Bitboard get_bishop_attacks(int square, Bitboard occupied) {
    // Mask relevant occupancy bits
    occupied &= bishop_masks[square];
    
    // Calculate magic index
    int magic_index = (occupied * bishop_magics[square]) >> (64 - BISHOP_MAGIC_BITS);
    
    // Return pre-computed attacks
    return bishop_attacks_table[square][magic_index];
}

Bitboard get_rook_attacks(int square, Bitboard occupied) {
    // Mask relevant occupancy bits
    occupied &= rook_masks[square];
    
    // Calculate magic index
    int magic_index = (occupied * rook_magics[square]) >> (64 - ROOK_MAGIC_BITS);
    
    // Return pre-computed attacks
    return rook_attacks_table[square][magic_index];
}

// Simple functions to access attack tables
Bitboard get_knight_attacks(int square) {
    return knight_attacks[square];
}

Bitboard get_king_attacks(int square) {
    return king_attacks[square];
}

// Add a move to the move list
static void add_move(MoveList* moves, int from, int to, int piece, int captured, int promoted, int flags) {
    if (moves->count < MAX_MOVES) {
        moves->moves[moves->count].move = MAKE_MOVE(from, to, piece, captured, promoted, flags);
        moves->moves[moves->count].score = 0;  // Will be set by move ordering
        moves->count++;
    }
}

// Check if position is in check
bool in_check(const Position* pos, int color) {
    Bitboard king_bb = pos->pieces[color][KING];
    if (!king_bb) return false;
    
    int king_sq = builtin_ctzll(king_bb);
    return is_square_attacked(pos, king_sq, 1 - color);
}

// Check if square is attacked by opponent
bool is_square_attacked(const Position* pos, int square, int by_color) {
    // Ensure attack tables are initialized
    init_attack_tables();
    
    // Check pawn attacks (most common)
    // We need to check if any pawns of 'by_color' can attack 'square'
    // For each pawn, check if it attacks the target square
    Bitboard pawns = pos->pieces[by_color][PAWN];
    while (pawns) {
        int pawn_sq = builtin_ctzll(pawns);
        if (pawn_attacks[by_color][pawn_sq] & (1ULL << square)) return true;
        pawns &= pawns - 1; // Clear the lowest set bit
    }
    
    // Check knight attacks - single bitboard operation
    if (knight_attacks[square] & pos->pieces[by_color][KNIGHT]) return true;
    
    // Check sliding piece attacks using optimized functions
    Bitboard all_pieces = pos->pieces[WHITE][0] | pos->pieces[BLACK][0];
    
    // Rook and queen attacks (horizontal/vertical)
    Bitboard rook_attacks = get_rook_attacks(square, all_pieces);
    if (rook_attacks & (pos->pieces[by_color][ROOK] | pos->pieces[by_color][QUEEN])) return true;
    
    // Bishop and queen attacks (diagonal)
    Bitboard bishop_attacks = get_bishop_attacks(square, all_pieces);
    if (bishop_attacks & (pos->pieces[by_color][BISHOP] | pos->pieces[by_color][QUEEN])) return true;
    
    // Check king attacks (least common) - single bitboard operation
    if (king_attacks[square] & pos->pieces[by_color][KING]) return true;
    
    return false;
}

// Generate pawn moves
static void generate_pawn_moves(const Position* pos, MoveList* moves, int color) {
    Bitboard pawns = pos->pieces[color][PAWN];
    Bitboard all_pieces = pos->pieces[WHITE][0] | pos->pieces[BLACK][0];
    Bitboard enemy_pieces = pos->pieces[1-color][0];
    
    int forward = (color == WHITE) ? 8 : -8;
    int start_rank = (color == WHITE) ? 1 : 6;
    int promo_rank = (color == WHITE) ? 7 : 0;
    
    while (pawns) {
        int from = builtin_ctzll(pawns);
        int to = from + forward;
        
        // Forward moves
        if (to >= 0 && to < 64 && !(all_pieces & (1ULL << to))) {
            if (to / 8 == promo_rank) {
                // Promotion
                add_move(moves, from, to, PAWN, 0, QUEEN, FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, 0, ROOK, FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, 0, BISHOP, FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, 0, KNIGHT, FLAG_PROMOTION);
            } else {
                add_move(moves, from, to, PAWN, 0, 0, 0);
                
                // Double push
                if (from / 8 == start_rank) {
                    int double_to = to + forward;
                    if (double_to >= 0 && double_to < 64 && !(all_pieces & (1ULL << double_to))) {
                        add_move(moves, from, double_to, PAWN, 0, 0, 0);
                    }
                }
            }
        }
        
        // Captures
        for (int side = -1; side <= 1; side += 2) {
            int capture_to = from + forward + side;
            if (capture_to >= 0 && capture_to < 64 && 
                abs((capture_to % 8) - (from % 8)) == 1) {
                
                if (enemy_pieces & (1ULL << capture_to)) {
                    // Find captured piece
                    int captured = 0;
                    for (int p = PAWN; p <= KING; p++) {
                        if (pos->pieces[1-color][p] & (1ULL << capture_to)) {
                            captured = p;
                            break;
                        }
                    }
                    
                    if (capture_to / 8 == promo_rank) {
                        // Capture promotion
                        add_move(moves, from, capture_to, PAWN, captured, QUEEN, FLAG_CAPTURE | FLAG_PROMOTION);
                        add_move(moves, from, capture_to, PAWN, captured, ROOK, FLAG_CAPTURE | FLAG_PROMOTION);
                        add_move(moves, from, capture_to, PAWN, captured, BISHOP, FLAG_CAPTURE | FLAG_PROMOTION);
                        add_move(moves, from, capture_to, PAWN, captured, KNIGHT, FLAG_CAPTURE | FLAG_PROMOTION);
                    } else {
                        add_move(moves, from, capture_to, PAWN, captured, 0, FLAG_CAPTURE);
                    }
                }
                
                // En passant
                if (pos->en_passant_square == capture_to) {
                    add_move(moves, from, capture_to, PAWN, PAWN, 0, FLAG_EP);
                }
            }
        }
        
        pawns &= pawns - 1;
    }
}

// Generate knight moves
static void generate_knight_moves(const Position* pos, MoveList* moves, int color) {
    Bitboard knights = pos->pieces[color][KNIGHT];
    Bitboard own_pieces = pos->pieces[color][0];
    
    while (knights) {
        int from = builtin_ctzll(knights);
        
        for (int i = 0; i < 8; i++) {
            int to = from + knight_dirs[i];
            
            if (to >= 0 && to < 64 && abs((to % 8) - (from % 8)) <= 2) {
                if (!(own_pieces & (1ULL << to))) {
                    int captured = 0;
                    int flags = 0;
                    
                    if (pos->pieces[1-color][0] & (1ULL << to)) {
                        flags |= FLAG_CAPTURE;
                        for (int p = PAWN; p <= KING; p++) {
                            if (pos->pieces[1-color][p] & (1ULL << to)) {
                                captured = p;
                                break;
                            }
                        }
                    }
                    
                    add_move(moves, from, to, KNIGHT, captured, 0, flags);
                }
            }
        }
        
        knights &= knights - 1;
    }
}

// Generate sliding piece moves (bishop, rook, queen)
static void generate_sliding_moves(const Position* pos, MoveList* moves, int color, int piece) {
    Bitboard pieces = pos->pieces[color][piece];
    Bitboard own_pieces = pos->pieces[color][0];
    Bitboard all_pieces = pos->pieces[WHITE][0] | pos->pieces[BLACK][0];
    
    const int* dirs;
    int num_dirs;
    
    if (piece == BISHOP) {
        dirs = bishop_dirs;
        num_dirs = 4;
    } else if (piece == ROOK) {
        dirs = rook_dirs;
        num_dirs = 4;
    } else if (piece == QUEEN) {
        // Queen moves like both bishop and rook - combine both direction sets
        static int queen_dirs[8] = {-9, -8, -7, -1, 1, 7, 8, 9};
        dirs = queen_dirs;
        num_dirs = 8;
    } else {
        return;
    }
    
    while (pieces) {
        int from = builtin_ctzll(pieces);
        
        for (int d = 0; d < num_dirs; d++) {
            int dir = dirs[d];
            
            for (int to = from + dir; to >= 0 && to < 64; to += dir) {
                // Check if we've wrapped around the board
                if (abs((to % 8) - ((to - dir) % 8)) > 2) break;
                
                if (own_pieces & (1ULL << to)) {
                    break;  // Blocked by own piece
                }
                
                int captured = 0;
                int flags = 0;
                
                if (pos->pieces[1-color][0] & (1ULL << to)) {
                    flags |= FLAG_CAPTURE;
                    for (int p = PAWN; p <= KING; p++) {
                        if (pos->pieces[1-color][p] & (1ULL << to)) {
                            captured = p;
                            break;
                        }
                    }
                }
                
                add_move(moves, from, to, piece, captured, 0, flags);
                
                if (all_pieces & (1ULL << to)) {
                    break;  // Blocked by any piece
                }
            }
        }
        
        pieces &= pieces - 1;
    }
}

// Generate castling moves
static void generate_castle_moves(const Position* pos, MoveList* moves, int color) {
    if (in_check(pos, color)) return;  // Can't castle in check
    
    Bitboard all_pieces = pos->pieces[WHITE][0] | pos->pieces[BLACK][0];
    
    if (color == WHITE) {
        // White kingside castling
        if ((pos->castle_rights & CASTLE_WK) && 
            !(all_pieces & ((1ULL << F1) | (1ULL << G1))) &&
            !is_square_attacked(pos, F1, BLACK) && 
            !is_square_attacked(pos, G1, BLACK)) {
            add_move(moves, E1, G1, KING, 0, 0, FLAG_CASTLE);
        }
        
        // White queenside castling
        if ((pos->castle_rights & CASTLE_WQ) && 
            !(all_pieces & ((1ULL << D1) | (1ULL << C1) | (1ULL << B1))) &&
            !is_square_attacked(pos, D1, BLACK) && 
            !is_square_attacked(pos, C1, BLACK)) {
            add_move(moves, E1, C1, KING, 0, 0, FLAG_CASTLE);
        }
    } else {
        // Black kingside castling
        if ((pos->castle_rights & CASTLE_BK) && 
            !(all_pieces & ((1ULL << F8) | (1ULL << G8))) &&
            !is_square_attacked(pos, F8, WHITE) && 
            !is_square_attacked(pos, G8, WHITE)) {
            add_move(moves, E8, G8, KING, 0, 0, FLAG_CASTLE);
        }
        
        // Black queenside castling
        if ((pos->castle_rights & CASTLE_BQ) && 
            !(all_pieces & ((1ULL << D8) | (1ULL << C8) | (1ULL << B8))) &&
            !is_square_attacked(pos, D8, WHITE) && 
            !is_square_attacked(pos, C8, WHITE)) {
            add_move(moves, E8, C8, KING, 0, 0, FLAG_CASTLE);
        }
    }
}

// Generate king moves
static void generate_king_moves(const Position* pos, MoveList* moves, int color) {
    Bitboard king = pos->pieces[color][KING];
    if (!king) return;
    
    int from = builtin_ctzll(king);
    Bitboard own_pieces = pos->pieces[color][0];
    
    for (int i = 0; i < 8; i++) {
        int to = from + king_dirs[i];
        
        if (to >= 0 && to < 64 && abs((to % 8) - (from % 8)) <= 1) {
            if (!(own_pieces & (1ULL << to))) {
                int captured = 0;
                int flags = 0;
                
                if (pos->pieces[1-color][0] & (1ULL << to)) {
                    flags |= FLAG_CAPTURE;
                    for (int p = PAWN; p <= KING; p++) {
                        if (pos->pieces[1-color][p] & (1ULL << to)) {
                            captured = p;
                            break;
                        }
                    }
                }
                
                add_move(moves, from, to, KING, captured, 0, flags);
            }
        }
    }
    
    // Add castling moves
    generate_castle_moves(pos, moves, color);
}

// Generate all legal moves
void generate_moves(const Position* pos, MoveList* moves) {
    moves->count = 0;
    
    generate_pawn_moves(pos, moves, pos->side_to_move);
    generate_knight_moves(pos, moves, pos->side_to_move);
    generate_sliding_moves(pos, moves, pos->side_to_move, BISHOP);
    generate_sliding_moves(pos, moves, pos->side_to_move, ROOK);
    generate_sliding_moves(pos, moves, pos->side_to_move, QUEEN);
    generate_king_moves(pos, moves, pos->side_to_move);
}

// Generate only captures (for quiescence search)
// Generate pawn captures only
static void generate_pawn_captures(const Position* pos, MoveList* moves, int color) {
    Bitboard pawns = pos->pieces[color][PAWN];
    Bitboard enemy_pieces = pos->pieces[1 - color][0];
    
    if (color == WHITE) {
        // White pawn captures
        Bitboard left_attacks = (pawns << 7) & ~FILE_H & enemy_pieces;
        Bitboard right_attacks = (pawns << 9) & ~FILE_A & enemy_pieces;
        
        // Process left diagonal captures
        while (left_attacks) {
            int to = builtin_ctzll(left_attacks);
            int from = to - 7;
            int captured = get_piece_at(pos, to, 1 - color);
            
            if (to >= 56) { // Promotion
                add_move(moves, from, to, PAWN, captured, QUEEN, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, ROOK, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, BISHOP, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, KNIGHT, FLAG_CAPTURE | FLAG_PROMOTION);
            } else {
                add_move(moves, from, to, PAWN, captured, 0, FLAG_CAPTURE);
            }
            left_attacks &= left_attacks - 1;
        }
        
        // Process right diagonal captures
        while (right_attacks) {
            int to = builtin_ctzll(right_attacks);
            int from = to - 9;
            int captured = get_piece_at(pos, to, 1 - color);
            
            if (to >= 56) { // Promotion
                add_move(moves, from, to, PAWN, captured, QUEEN, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, ROOK, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, BISHOP, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, KNIGHT, FLAG_CAPTURE | FLAG_PROMOTION);
            } else {
                add_move(moves, from, to, PAWN, captured, 0, FLAG_CAPTURE);
            }
            right_attacks &= right_attacks - 1;
        }
        
        // En passant captures
        if (pos->en_passant_square != -1) {
            Bitboard ep_pawns = pawns & pawn_attacks[BLACK][pos->en_passant_square];
            while (ep_pawns) {
                int from = builtin_ctzll(ep_pawns);
                add_move(moves, from, pos->en_passant_square, PAWN, PAWN, 0, FLAG_EP);
                ep_pawns &= ep_pawns - 1;
            }
        }
    } else {
        // Black pawn captures
        Bitboard left_attacks = (pawns >> 9) & ~FILE_H & enemy_pieces;
        Bitboard right_attacks = (pawns >> 7) & ~FILE_A & enemy_pieces;
        
        // Process left diagonal captures
        while (left_attacks) {
            int to = builtin_ctzll(left_attacks);
            int from = to + 9;
            int captured = get_piece_at(pos, to, 1 - color);
            
            if (to <= 7) { // Promotion
                add_move(moves, from, to, PAWN, captured, QUEEN, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, ROOK, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, BISHOP, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, KNIGHT, FLAG_CAPTURE | FLAG_PROMOTION);
            } else {
                add_move(moves, from, to, PAWN, captured, 0, FLAG_CAPTURE);
            }
            left_attacks &= left_attacks - 1;
        }
        
        // Process right diagonal captures
        while (right_attacks) {
            int to = builtin_ctzll(right_attacks);
            int from = to + 7;
            int captured = get_piece_at(pos, to, 1 - color);
            
            if (to <= 7) { // Promotion
                add_move(moves, from, to, PAWN, captured, QUEEN, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, ROOK, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, BISHOP, FLAG_CAPTURE | FLAG_PROMOTION);
                add_move(moves, from, to, PAWN, captured, KNIGHT, FLAG_CAPTURE | FLAG_PROMOTION);
            } else {
                add_move(moves, from, to, PAWN, captured, 0, FLAG_CAPTURE);
            }
            right_attacks &= right_attacks - 1;
        }
        
        // En passant captures
        if (pos->en_passant_square != -1) {
            Bitboard ep_pawns = pawns & pawn_attacks[WHITE][pos->en_passant_square];
            while (ep_pawns) {
                int from = builtin_ctzll(ep_pawns);
                add_move(moves, from, pos->en_passant_square, PAWN, PAWN, 0, FLAG_EP);
                ep_pawns &= ep_pawns - 1;
            }
        }
    }
}

// Generate piece captures only (knight, bishop, rook, queen, king)
static void generate_piece_captures(const Position* pos, MoveList* moves, int color, int piece) {
    Bitboard pieces = pos->pieces[color][piece];
    Bitboard enemy_pieces = pos->pieces[1 - color][0];
    
    while (pieces) {
        int from = builtin_ctzll(pieces);
        Bitboard attacks = 0;
        
        switch (piece) {
            case KNIGHT:
                attacks = knight_attacks[from];
                break;
            case BISHOP:
                attacks = get_bishop_attacks(from, pos->pieces[WHITE][0] | pos->pieces[BLACK][0]);
                break;
            case ROOK:
                attacks = get_rook_attacks(from, pos->pieces[WHITE][0] | pos->pieces[BLACK][0]);
                break;
            case QUEEN:
                attacks = get_bishop_attacks(from, pos->pieces[WHITE][0] | pos->pieces[BLACK][0]) |
                         get_rook_attacks(from, pos->pieces[WHITE][0] | pos->pieces[BLACK][0]);
                break;
            case KING:
                attacks = king_attacks[from];
                break;
        }
        
        // Only consider captures
        attacks &= enemy_pieces;
        
        while (attacks) {
            int to = builtin_ctzll(attacks);
            int captured = get_piece_at(pos, to, 1 - color);
            add_move(moves, from, to, piece, captured, 0, FLAG_CAPTURE);
            attacks &= attacks - 1;
        }
        
        pieces &= pieces - 1;
    }
}

// Optimized capture generation - only generates captures directly
void generate_captures(const Position* pos, MoveList* moves) {
    init_attack_tables(); // Initialize attack tables if not done already
    moves->count = 0;
    
    int color = pos->side_to_move;
    
    // Generate pawn captures (including promotions and en passant)
    generate_pawn_captures(pos, moves, color);
    
    // Generate piece captures
    generate_piece_captures(pos, moves, color, KNIGHT);
    generate_piece_captures(pos, moves, color, BISHOP);
    generate_piece_captures(pos, moves, color, ROOK);
    generate_piece_captures(pos, moves, color, QUEEN);
    generate_piece_captures(pos, moves, color, KING);
}

// Make a move on the position
bool make_move(Position* pos, Move move) {
    int from = MOVE_FROM(move);
    int to = MOVE_TO(move);
    int piece = MOVE_PIECE(move);
    int captured = MOVE_CAPTURED(move);
    int promoted = MOVE_PROMOTED(move);
    int flags = MOVE_FLAGS(move);
    
    // Basic move validation
    if (from < 0 || from >= 64 || to < 0 || to >= 64) return false;
    if (!(pos->pieces[pos->side_to_move][piece] & (1ULL << from))) return false;
    
    // Store old values for incremental hash updates
    int old_castle_rights = pos->castle_rights;
    int old_ep_square = pos->en_passant_square;
    
    // Clear en passant square
    pos->en_passant_square = -1;
    
    // Update incremental evaluation - remove piece from source
    update_eval_remove_piece(pos, piece, from, pos->side_to_move);
    
    // Update incremental hash - remove piece from source
    update_hash_remove_piece(pos, piece, from, pos->side_to_move);
    
    // Remove piece from source
    pos->pieces[pos->side_to_move][piece] &= ~(1ULL << from);
    pos->pieces[pos->side_to_move][0] &= ~(1ULL << from);
    
    // Handle capture
    if (flags & FLAG_CAPTURE) {
        // Update incremental evaluation - remove captured piece
        update_eval_remove_piece(pos, captured, to, 1 - pos->side_to_move);
        
        // Update incremental hash - remove captured piece
        update_hash_remove_piece(pos, captured, to, 1 - pos->side_to_move);
        
        pos->pieces[1-pos->side_to_move][captured] &= ~(1ULL << to);
        pos->pieces[1-pos->side_to_move][0] &= ~(1ULL << to);
    }
    
    // Handle en passant capture
    if (flags & FLAG_EP) {
        int ep_capture_sq = (pos->side_to_move == WHITE) ? to - 8 : to + 8;
        
        // Update incremental evaluation - remove en passant captured pawn
        update_eval_remove_piece(pos, PAWN, ep_capture_sq, 1 - pos->side_to_move);
        
        // Update incremental hash - remove en passant captured pawn
        update_hash_remove_piece(pos, PAWN, ep_capture_sq, 1 - pos->side_to_move);
        
        pos->pieces[1-pos->side_to_move][PAWN] &= ~(1ULL << ep_capture_sq);
        pos->pieces[1-pos->side_to_move][0] &= ~(1ULL << ep_capture_sq);
    }
    
    // Handle promotion
    if (flags & FLAG_PROMOTION) {
        // Update incremental evaluation - add promoted piece to destination
        update_eval_add_piece(pos, promoted, to, pos->side_to_move);
        
        // Update incremental hash - add promoted piece to destination
        update_hash_add_piece(pos, promoted, to, pos->side_to_move);
        
        pos->pieces[pos->side_to_move][promoted] |= (1ULL << to);
        pos->pieces[pos->side_to_move][0] |= (1ULL << to);
    } else {
        // Update incremental evaluation - add piece to destination
        update_eval_add_piece(pos, piece, to, pos->side_to_move);
        
        // Update incremental hash - add piece to destination
        update_hash_add_piece(pos, piece, to, pos->side_to_move);
        
        pos->pieces[pos->side_to_move][piece] |= (1ULL << to);
        pos->pieces[pos->side_to_move][0] |= (1ULL << to);
    }
    
    // Handle castling
    if (flags & FLAG_CASTLE) {
        if (to == G1) {  // White kingside
            // Update incremental evaluation - move rook from H1 to F1
            update_eval_move_piece(pos, ROOK, H1, F1, WHITE);
            
            // Update incremental hash - move rook from H1 to F1
            update_hash_move_piece(pos, ROOK, H1, F1, WHITE);
            
            pos->pieces[WHITE][ROOK] &= ~(1ULL << H1);
            pos->pieces[WHITE][ROOK] |= (1ULL << F1);
            pos->pieces[WHITE][0] &= ~(1ULL << H1);
            pos->pieces[WHITE][0] |= (1ULL << F1);
        } else if (to == C1) {  // White queenside
            // Update incremental evaluation - move rook from A1 to D1
            update_eval_move_piece(pos, ROOK, A1, D1, WHITE);
            
            // Update incremental hash - move rook from A1 to D1
            update_hash_move_piece(pos, ROOK, A1, D1, WHITE);
            
            pos->pieces[WHITE][ROOK] &= ~(1ULL << A1);
            pos->pieces[WHITE][ROOK] |= (1ULL << D1);
            pos->pieces[WHITE][0] &= ~(1ULL << A1);
            pos->pieces[WHITE][0] |= (1ULL << D1);
        } else if (to == G8) {  // Black kingside
            // Update incremental evaluation - move rook from H8 to F8
            update_eval_move_piece(pos, ROOK, H8, F8, BLACK);
            
            // Update incremental hash - move rook from H8 to F8
            update_hash_move_piece(pos, ROOK, H8, F8, BLACK);
            
            pos->pieces[BLACK][ROOK] &= ~(1ULL << H8);
            pos->pieces[BLACK][ROOK] |= (1ULL << F8);
            pos->pieces[BLACK][0] &= ~(1ULL << H8);
            pos->pieces[BLACK][0] |= (1ULL << F8);
        } else if (to == C8) {  // Black queenside
            // Update incremental evaluation - move rook from A8 to D8
            update_eval_move_piece(pos, ROOK, A8, D8, BLACK);
            
            // Update incremental hash - move rook from A8 to D8
            update_hash_move_piece(pos, ROOK, A8, D8, BLACK);
            
            pos->pieces[BLACK][ROOK] &= ~(1ULL << A8);
            pos->pieces[BLACK][ROOK] |= (1ULL << D8);
            pos->pieces[BLACK][0] &= ~(1ULL << A8);
            pos->pieces[BLACK][0] |= (1ULL << D8);
        }
    }
    
    // Update castling rights
    if (piece == KING) {
        if (pos->side_to_move == WHITE) {
            pos->castle_rights &= ~(CASTLE_WK | CASTLE_WQ);
        } else {
            pos->castle_rights &= ~(CASTLE_BK | CASTLE_BQ);
        }
    } else if (piece == ROOK) {
        if (from == A1) pos->castle_rights &= ~CASTLE_WQ;
        else if (from == H1) pos->castle_rights &= ~CASTLE_WK;
        else if (from == A8) pos->castle_rights &= ~CASTLE_BQ;
        else if (from == H8) pos->castle_rights &= ~CASTLE_BK;
    }
    
    // CRITICAL FIX: Remove castling rights when rooks are captured on castling squares
    if (flags & FLAG_CAPTURE) {
        if (to == A1) pos->castle_rights &= ~CASTLE_WQ;      // White queenside rook captured
        else if (to == H1) pos->castle_rights &= ~CASTLE_WK; // White kingside rook captured
        else if (to == A8) pos->castle_rights &= ~CASTLE_BQ; // Black queenside rook captured
        else if (to == H8) pos->castle_rights &= ~CASTLE_BK; // Black kingside rook captured
    }
    
    // Set en passant square for pawn double moves
    if (piece == PAWN && abs(to - from) == 16) {
        pos->en_passant_square = (from + to) / 2;
    }
    
    // Update halfmove clock
    if (piece == PAWN || (flags & FLAG_CAPTURE)) {
        pos->halfmove_clock = 0;
    } else {
        pos->halfmove_clock++;
    }
    
    // Update fullmove number
    if (pos->side_to_move == BLACK) {
        pos->fullmove_number++;
    }
    
    // Switch sides
    pos->side_to_move = 1 - pos->side_to_move;
    
    // Update incremental hash for castling rights change
    if (old_castle_rights != pos->castle_rights) {
        update_hash_castle_rights(pos, old_castle_rights, pos->castle_rights);
    }
    
    // Update incremental hash for en passant change
    if (old_ep_square != pos->en_passant_square) {
        update_hash_en_passant(pos, old_ep_square, pos->en_passant_square);
    }
    
    // Update incremental hash for side to move change
    update_hash_side_to_move(pos);
    
    return true;
}

// Print move in algebraic notation
void print_move(Move move) {
    int from = MOVE_FROM(move);
    int to = MOVE_TO(move);
    
    printf("%c%d%c%d", 
           'a' + (from % 8), 1 + (from / 8),
           'a' + (to % 8), 1 + (to / 8));
    
    // Add promotion piece suffix for Winboard compatibility
    if (MOVE_FLAGS(move) & FLAG_PROMOTION) {
        int promoted_piece = MOVE_PROMOTED(move);
        switch (promoted_piece) {
            case QUEEN:  printf("q"); break;
            case ROOK:   printf("r"); break;
            case BISHOP: printf("b"); break;
            case KNIGHT: printf("n"); break;
            default:     printf("q"); break;  // Default to queen if unknown
        }
    }
}

// Parse move from algebraic notation (e.g., "e2e4", "e7e8q")
Move parse_move(const Position* pos, const char* move_str) {
    if (strlen(move_str) < 4) return 0;
    
    int from_file = move_str[0] - 'a';
    int from_rank = move_str[1] - '1';
    int to_file = move_str[2] - 'a';
    int to_rank = move_str[3] - '1';
    
    if (from_file < 0 || from_file > 7 || from_rank < 0 || from_rank > 7 ||
        to_file < 0 || to_file > 7 || to_rank < 0 || to_rank > 7) {
        return 0;
    }
    
    int from = from_rank * 8 + from_file;
    int to = to_rank * 8 + to_file;
    
    // Generate all legal moves and find matching one
    MoveList moves;
    generate_moves(pos, &moves);
    
    for (int i = 0; i < moves.count; i++) {
        Move move = moves.moves[i].move;
        if (MOVE_FROM(move) == from && MOVE_TO(move) == to) {
            // Check for promotion
            if (strlen(move_str) == 5) {
                int promoted = 0;
                switch (move_str[4]) {
                    case 'q': promoted = QUEEN; break;
                    case 'r': promoted = ROOK; break;
                    case 'b': promoted = BISHOP; break;
                    case 'n': promoted = KNIGHT; break;
                    default: continue;
                }
                if (MOVE_PROMOTED(move) == promoted) {
                    return move;
                }
            } else if (!(MOVE_FLAGS(move) & FLAG_PROMOTION)) {
                return move;
            }
        }
    }
    
    return 0;  // Move not found
}

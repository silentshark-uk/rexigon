#include "compat.h"
#include "chess.h"

// Piece values in centipawns - Middlegame and Endgame
static const int mg_value[6] = { 82, 337, 365, 477, 1025, 20000};  // PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
static const int eg_value[6] = { 94, 281, 297, 512,  936, 20000};  // PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING

// Legacy piece values array for compatibility (using middlegame values)
static const Score piece_values[7] = {
    0, 82, 337, 365, 477, 1025, 20000  // EMPTY, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
};

// Tapered Piece-Square Tables (from white's perspective)
// Middlegame PST values
static const int mg_pawn_table[64] = {
       0,   0,   0,   0,   0,   0,   0,   0,
     -35,  -1, -20, -23, -15,  24,  38, -22,
     -26,  -4,  -4, -10,   3,   3,  33, -12,
     -27,  -2,  -5,  12,  17,   6,  10, -25,
     -14,  13,   6,  21,  23,  12,  17, -23,
      -6,   7,  26,  31,  65,  56,  25, -20,
      98, 134,  61,  95,  68, 126,  34, -11,
       0,   0,   0,   0,   0,   0,   0,   0
};

static const int mg_knight_table[64] = {
    -105, -21, -58, -33, -17, -28, -19, -23,
     -29, -53, -12,  -3,  -1,  18, -14, -19,
     -23,  -9,  12,  10,  19,  17,  25, -16,
     -13,   4,  16,  13,  28,  19,  21,  -8,
      -9,  17,  19,  53,  37,  69,  18,  22,
     -47,  60,  37,  65,  84, 129,  73,  44,
     -73, -41,  72,  36,  23,  62,   7, -17,
    -167, -89, -34, -49,  61, -97, -15,-107
};

static const int mg_bishop_table[64] = {
     -33,  -3, -14, -21, -13, -12, -39, -21,
       4,  15,  16,   0,   7,  21,  33,   1,
       0,  15,  15,  15,  14,  27,  18,  10,
      -6,  13,  13,  26,  34,  12,  10,   4,
      -4,   5,  19,  50,  37,  37,   7,  -2,
     -16,  37,  43,  40,  35,  50,  37,  -2,
     -26,  16, -18, -13,  30,  59,  18, -47,
     -29,   4, -82, -37, -25, -42,   7,  -8
};

static const int mg_rook_table[64] = {
     -19, -13,   1,  17,  16,   7, -37, -26,
     -44, -16, -20,  -9,  -1,  11,  -6, -71,
     -45, -25, -16, -17,   3,   0,  -5, -33,
     -36, -26, -12,  -1,   9,  -7,   6, -23,
     -24, -11,   7,  26,  24,  35,  -8, -20,
      -5,  19,  26,  36,  17,  45,  61,  16,
      27,  32,  58,  62,  80,  67,  26,  44,
      32,  42,  32,  51,  63,   9,  31,  43
};

static const int mg_queen_table[64] = {
      -1, -18,  -9,  10, -15, -25, -31, -50,
     -35,  -8,  11,   2,   8,  15,  -3,   1,
     -14,   2, -11,  -2,  -5,   2,  14,   5,
      -9, -26,  -9, -10,  -2,  -4,   3,  -3,
     -27, -27, -16, -16,  -1,  17,  -2,   1,
     -13, -17,   7,   8,  29,  56,  47,  57,
     -24, -39,  -5,   1, -16,  57,  28,  54,
     -28,   0,  29,  12,  59,  44,  43,  45
};

static const int mg_king_table[64] = {
     -15,  36,  12, -54,   8, -28,  24,  14,
       1,   7,  -8, -64, -43, -16,   9,   8,
     -14, -14, -22, -46, -44, -30, -15, -27,
     -49,  -1, -27, -39, -46, -44, -33, -51,
     -17, -20, -12, -27, -30, -25, -14, -36,
      -9,  24,   2, -16, -20,   6,  22, -22,
      29,  -1, -20,  -7,  -8,  -4, -38, -29,
     -65,  23,  16, -15, -56, -34,   2,  13
};

// Endgame PST values
static const int eg_pawn_table[64] = {
       0,   0,   0,   0,   0,   0,   0,   0,
      13,   8,   8,  10,  13,   0,   2,  -7,
       4,   7,  -6,   1,   0,  -5,  -1,  -8,
      13,   9,  -3,  -7,  -7,  -8,   3,  -1,
      32,  24,  13,   5,  -2,   4,  17,  17,
      94, 100,  85,  67,  56,  53,  82,  84,
     178, 173, 158, 134, 147, 132, 165, 187,
       0,   0,   0,   0,   0,   0,   0,   0
};

static const int eg_knight_table[64] = {
     -29, -51, -23, -15, -22, -18, -50, -64,
     -42, -20, -10,  -5,  -2, -20, -23, -44,
     -23,  -3,  -1,  15,  10,  -3, -20, -22,
     -18,  -6,  16,  25,  16,  17,   4, -18,
     -17,   3,  22,  22,  22,  11,   8, -18,
     -24, -20,  10,   9,  -1,  -9, -19, -41,
     -25,  -8, -25,  -2,  -9, -25, -24, -52,
     -58, -38, -13, -28, -31, -27, -63, -99
};

static const int eg_bishop_table[64] = {
     -23,  -9, -23,  -5,  -9, -16,  -5, -17,
     -14, -18,  -7,  -1,   4,  -9, -15, -27,
     -12,  -3,   8,  10,  13,   3,  -7, -15,
      -6,   3,  13,  19,   7,  10,  -3,  -9,
      -3,   9,  12,   9,  14,  10,   3,   2,
       2,  -8,   0,  -1,  -2,   6,   0,   4,
      -8,  -4,   7, -12,  -3, -13,  -4, -14,
     -14, -21, -11,  -8,  -7,  -9, -17, -24
};

static const int eg_rook_table[64] = {
      -9,   2,   3,  -1,  -5, -13,   4, -20,
      -6,  -6,   0,   2,  -9,  -9, -11,  -3,
      -4,   0,  -5,  -1,  -7, -12,  -8, -16,
       3,   5,   8,   4,  -5,  -6,  -8, -11,
       4,   3,  13,   1,   2,   1,  -1,   2,
       7,   7,   7,   5,   4,  -3,  -5,  -3,
      11,  13,  13,  11,  -3,   3,   8,   3,
      13,  10,  18,  15,  12,  12,   8,   5
};

static const int eg_queen_table[64] = {
     -33, -28, -22, -43,  -5, -32, -20, -41,
     -22, -23, -30, -16, -16, -23, -36, -32,
     -16, -27,  15,   6,   9,  17,  10,   5,
     -18,  28,  19,  47,  31,  34,  39,  23,
       3,  22,  24,  45,  57,  40,  57,  36,
     -20,   6,   9,  49,  47,  35,  19,   9,
     -17,  20,  32,  41,  58,  25,  30,   0,
      -9,  22,  22,  27,  27,  19,  10,  20
};

static const int eg_king_table[64] = {
     -53, -34, -21, -11, -28, -14, -24, -43,
     -27, -11,   4,  13,  14,   4,  -5, -17,
     -19,  -3,  11,  21,  23,  16,   7,  -9,
     -18,  -4,  21,  24,  27,  23,   9, -11,
      -8,  22,  24,  27,  26,  33,  26,   3,
      10,  17,  23,  15,  20,  45,  44,  13,
     -12,  17,  14,  17,  17,  38,  23,  11,
     -74, -35, -18, -18, -11,  15,   4, -17
};

// PST lookup table
// PST table pointers for middlegame and endgame
static const int* mg_pst_tables[7] = {
    NULL, mg_pawn_table, mg_knight_table, mg_bishop_table, mg_rook_table, mg_queen_table, mg_king_table
};

static const int* eg_pst_tables[7] = {
    NULL, eg_pawn_table, eg_knight_table, eg_bishop_table, eg_rook_table, eg_queen_table, eg_king_table
};

// Get tapered PST value for a piece on a square
static void get_pst_values(int piece, int square, int color, int* mg_value, int* eg_value) {
    *mg_value = 0;
    *eg_value = 0;
    
    if (piece == EMPTY || !mg_pst_tables[piece] || !eg_pst_tables[piece]) return;
    
    // Flip square for black pieces
    int sq = (color == WHITE) ? square : (square ^ 56);
    
    *mg_value = mg_pst_tables[piece][sq];
    *eg_value = eg_pst_tables[piece][sq];
}

// Get combined material + PST value for a piece
static void get_piece_eval_values(int piece, int square, int color, int* mg_total, int* eg_total) {
    // Get material values
    Score mg_material = (piece == KING) ? mg_value[5] : mg_value[piece - 1];
    Score eg_material = (piece == KING) ? eg_value[5] : eg_value[piece - 1];
    
    // Get PST values
    int mg_pst, eg_pst;
    get_pst_values(piece, square, color, &mg_pst, &eg_pst);
    
    *mg_total = mg_material + mg_pst;
    *eg_total = eg_material + eg_pst;
}

// Initialize evaluation for a position from scratch
void init_evaluation(Position* pos) {
    pos->mg_score = 0;
    pos->eg_score = 0;
    pos->game_phase = 0;
    
    // Calculate game phase
    const int phase_values[6] = {0, 1, 1, 2, 4, 0};  // PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    
    for (int color = 0; color < 2; color++) {
        for (int piece = PAWN; piece <= QUEEN; piece++) {
            pos->game_phase += builtin_popcountll(pos->pieces[color][piece]) * phase_values[piece];
        }
    }
    
    // Total phase is 24 (8 pawns + 4 knights + 4 bishops + 4 rooks + 2 queens)
    const int total_phase = 24;
    if (pos->game_phase > total_phase) pos->game_phase = total_phase;
    
    // Calculate material and PST scores
    for (int color = 0; color < 2; color++) {
        for (int piece = PAWN; piece <= KING; piece++) {
            Bitboard bb = pos->pieces[color][piece];
            
            while (bb) {
                int sq = builtin_ctzll(bb);
                
                int mg_piece_value, eg_piece_value;
                get_piece_eval_values(piece, sq, color, &mg_piece_value, &eg_piece_value);
                
                if (color == WHITE) {
                    pos->mg_score += mg_piece_value;
                    pos->eg_score += eg_piece_value;
                } else {
                    pos->mg_score -= mg_piece_value;
                    pos->eg_score -= eg_piece_value;
                }
                
                bb &= bb - 1;  // Clear lowest set bit
            }
        }
    }
    
    pos->eval_dirty = false;
}

// Update evaluation when adding a piece
void update_eval_add_piece(Position* pos, int piece, int square, int color) {
    int mg_piece_value, eg_piece_value;
    get_piece_eval_values(piece, square, color, &mg_piece_value, &eg_piece_value);
    
    if (color == WHITE) {
        pos->mg_score += mg_piece_value;
        pos->eg_score += eg_piece_value;
    } else {
        pos->mg_score -= mg_piece_value;
        pos->eg_score -= eg_piece_value;
    }
    
    // Update game phase for non-king pieces
    if (piece != KING) {
        const int phase_values[6] = {0, 1, 1, 2, 4, 0};  // PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
        pos->game_phase += phase_values[piece];
        if (pos->game_phase > 24) pos->game_phase = 24;
    }
}

// Update evaluation when removing a piece
void update_eval_remove_piece(Position* pos, int piece, int square, int color) {
    int mg_piece_value, eg_piece_value;
    get_piece_eval_values(piece, square, color, &mg_piece_value, &eg_piece_value);
    
    if (color == WHITE) {
        pos->mg_score -= mg_piece_value;
        pos->eg_score -= eg_piece_value;
    } else {
        pos->mg_score += mg_piece_value;
        pos->eg_score += eg_piece_value;
    }
    
    // Update game phase for non-king pieces
    if (piece != KING) {
        const int phase_values[6] = {0, 1, 1, 2, 4, 0};  // PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
        pos->game_phase -= phase_values[piece];
        if (pos->game_phase < 0) pos->game_phase = 0;
    }
}

// Update evaluation when moving a piece
void update_eval_move_piece(Position* pos, int piece, int from_sq, int to_sq, int color) {
    // Remove from old square
    int mg_from_value, eg_from_value;
    get_piece_eval_values(piece, from_sq, color, &mg_from_value, &eg_from_value);
    
    // Add to new square
    int mg_to_value, eg_to_value;
    get_piece_eval_values(piece, to_sq, color, &mg_to_value, &eg_to_value);
    
    // Net change
    int mg_delta = mg_to_value - mg_from_value;
    int eg_delta = eg_to_value - eg_from_value;
    
    if (color == WHITE) {
        pos->mg_score += mg_delta;
        pos->eg_score += eg_delta;
    } else {
        pos->mg_score -= mg_delta;
        pos->eg_score -= eg_delta;
    }
    
    // Game phase doesn't change when moving pieces
}

// Check for insufficient material
static bool insufficient_material(const Position* pos) {
    // Count material for both sides
    int white_knights = builtin_popcountll(pos->pieces[WHITE][KNIGHT]);
    int white_bishops = builtin_popcountll(pos->pieces[WHITE][BISHOP]);
    int white_rooks = builtin_popcountll(pos->pieces[WHITE][ROOK]);
    int white_queens = builtin_popcountll(pos->pieces[WHITE][QUEEN]);
    int white_pawns = builtin_popcountll(pos->pieces[WHITE][PAWN]);
    
    int black_knights = builtin_popcountll(pos->pieces[BLACK][KNIGHT]);
    int black_bishops = builtin_popcountll(pos->pieces[BLACK][BISHOP]);
    int black_rooks = builtin_popcountll(pos->pieces[BLACK][ROOK]);
    int black_queens = builtin_popcountll(pos->pieces[BLACK][QUEEN]);
    int black_pawns = builtin_popcountll(pos->pieces[BLACK][PAWN]);
    
    // If either side has pawns, rooks, or queens, there's sufficient material
    if (white_pawns || black_pawns || white_rooks || black_rooks || 
        white_queens || black_queens) {
        return false;
    }
    
    // King vs King
    if (!white_knights && !white_bishops && !black_knights && !black_bishops) {
        return true;
    }
    
    // King + minor piece vs King
    if ((white_knights + white_bishops <= 1) && (black_knights + black_bishops == 0)) {
        return true;
    }
    if ((black_knights + black_bishops <= 1) && (white_knights + white_bishops == 0)) {
        return true;
    }
    
    return false;
}

// Main evaluation function - now uses incremental evaluation
// King Safety Evaluation - Non-linear attack counting
static int evaluate_king_safety(const Position* pos, int color) {
    // Find king position
    Bitboard king_bb = pos->pieces[color][KING];
    if (!king_bb) return 0;
    int king_sq = builtin_ctzll(king_bb);
    
    int king_file = king_sq % 8;
    int king_rank = king_sq / 8;
    int opponent = 1 - color;
    int forward = (color == WHITE) ? 1 : -1;
    
    // Build occupied bitboard once
    Bitboard occupied = 0;
    for (int c = 0; c < 2; c++) {
        for (int p = PAWN; p <= KING; p++) {
            occupied |= pos->pieces[c][p];
        }
    }
    
    // 1. Count attacks on king zone with weighted pieces
    int attack_weight = 0;
    const int attack_weights[7] = {0, 0, 2, 2, 3, 5, 0};  // KNIGHT=2, BISHOP=2, ROOK=3, QUEEN=5
    
    for (int file_offset = -1; file_offset <= 1; file_offset++) {
        for (int rank_offset = -1; rank_offset <= 1; rank_offset++) {
            int target_file = king_file + file_offset;
            int target_rank = king_rank + rank_offset;
            
            if (target_file < 0 || target_file > 7 || target_rank < 0 || target_rank > 7) continue;
            int target_sq = target_rank * 8 + target_file;
            
            // Check each attacking piece type
            if (get_knight_attacks(target_sq) & pos->pieces[opponent][KNIGHT]) 
                attack_weight += attack_weights[KNIGHT];
            if (get_bishop_attacks(target_sq, occupied) & pos->pieces[opponent][BISHOP]) 
                attack_weight += attack_weights[BISHOP];
            if (get_rook_attacks(target_sq, occupied) & pos->pieces[opponent][ROOK]) 
                attack_weight += attack_weights[ROOK];
            
            Bitboard queen_attacks = get_rook_attacks(target_sq, occupied) | get_bishop_attacks(target_sq, occupied);
            if (queen_attacks & pos->pieces[opponent][QUEEN]) 
                attack_weight += attack_weights[QUEEN];
        }
    }
    
    // 2. Evaluate pawn shield with gradual penalties based on distance
    int pawn_shield_score[3] = {0, 0, 0};  // For each file: left, center, right
    
    for (int file_offset = -1; file_offset <= 1; file_offset++) {
        int shield_file = king_file + file_offset;
        if (shield_file < 0 || shield_file > 7) continue;
        
        int file_idx = file_offset + 1;  // 0=left, 1=center, 2=right
        int closest_pawn_dist = 8;  // Max distance
        
        // Find closest pawn on this file
        for (int dist = 1; dist <= 6; dist++) {
            int shield_rank = king_rank + forward * dist;
            if (shield_rank < 0 || shield_rank > 7) break;
            
            int shield_sq = shield_rank * 8 + shield_file;
            if (pos->pieces[color][PAWN] & (1ULL << shield_sq)) {
                closest_pawn_dist = dist;
                break;
            }
        }
        
        // Gradual scoring based on pawn distance
        // Distance 1: +20, Distance 2: +12, Distance 3: +6, Distance 4+: 0, Missing: penalty
        if (closest_pawn_dist == 1) {
            pawn_shield_score[file_idx] = 20;
        } else if (closest_pawn_dist == 2) {
            pawn_shield_score[file_idx] = 12;
        } else if (closest_pawn_dist == 3) {
            pawn_shield_score[file_idx] = 6;
        } else if (closest_pawn_dist == 4) {
            pawn_shield_score[file_idx] = 2;
        } else {
            // No pawn - penalty depends on file importance
            pawn_shield_score[file_idx] = (file_idx == 1) ? -15 : -8;  // Center file more critical
        }
    }
    
    int total_pawn_shield = pawn_shield_score[0] + pawn_shield_score[1] + pawn_shield_score[2];
    
    // 3. Calculate final safety score with interactions
    int safety_score = 0;
    
    // Base attack penalty (quadratic)
    if (attack_weight > 0) {
        int attack_penalty = attack_weight * attack_weight;
        
        // Interaction: Weak pawn shield amplifies attack danger
        // If pawn shield is weak (negative or low), multiply attack penalty
        if (total_pawn_shield < 10) {
            // Scale from 1.0x (shield=10) to 2.5x (shield=-30)
            int shield_factor = 100 + (10 - total_pawn_shield) * 3;  // 100 to 220
            attack_penalty = (attack_penalty * shield_factor) / 100;
        }
        
        safety_score -= attack_penalty;
    }
    
    // Add pawn shield score
    safety_score += total_pawn_shield;
    
    // 4. Open/semi-open file penalties (interact with attacks)
    for (int file_offset = -1; file_offset <= 1; file_offset++) {
        int check_file = king_file + file_offset;
        if (check_file < 0 || check_file > 7) continue;
        
        bool has_our_pawn = false;
        bool has_opp_pawn = false;
        
        for (int rank = 0; rank < 8; rank++) {
            int sq = rank * 8 + check_file;
            if (pos->pieces[color][PAWN] & (1ULL << sq)) has_our_pawn = true;
            if (pos->pieces[opponent][PAWN] & (1ULL << sq)) has_opp_pawn = true;
        }
        
        // Open file (no pawns): dangerous if attacks present
        if (!has_our_pawn && !has_opp_pawn) {
            int open_penalty = (file_offset == 0) ? 8 : 4;  // Base penalty
            if (attack_weight > 3) {
                open_penalty *= 2;  // Double penalty if under attack
            }
            safety_score -= open_penalty;
        }
        // Semi-open file (only opponent pawns): moderately dangerous
        else if (!has_our_pawn && has_opp_pawn) {
            int semi_penalty = (file_offset == 0) ? 5 : 2;
            if (attack_weight > 5) {
                semi_penalty = (semi_penalty * 3) / 2;  // 1.5x penalty if heavily attacked
            }
            safety_score -= semi_penalty;
        }
    }
    safety_score/=2;
    return safety_score;
}

// Mobility Evaluation - Non-linear with tapering
// Mobility bonus tables (non-linear, diminishing returns)
static const int knight_mobility_mg[9] = {-8, -4, 0, 3, 6, 8, 10, 11, 12};
static const int knight_mobility_eg[9] = {-10, -5, 0, 4, 7, 10, 12, 13, 14};

static const int bishop_mobility_mg[14] = {-10, -6, -2, 2, 5, 8, 10, 12, 13, 14, 15, 15, 16, 16};
static const int bishop_mobility_eg[14] = {-12, -7, -3, 3, 7, 11, 14, 16, 18, 19, 20, 20, 21, 21};

static const int rook_mobility_mg[15] = {-8, -4, -2, 0, 2, 4, 5, 6, 7, 8, 9, 9, 10, 10, 10};
static const int rook_mobility_eg[15] = {-10, -6, -3, 0, 3, 6, 8, 10, 11, 12, 13, 14, 14, 15, 15};

static const int queen_mobility_mg[28] = {
    -5, -3, -1, 1, 2, 3, 4, 5, 6, 7, 7, 8, 8, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
};
static const int queen_mobility_eg[28] = {
    -8, -5, -2, 2, 4, 6, 8, 10, 11, 12, 13, 14, 15, 15, 16, 16, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18
};

static void evaluate_mobility(const Position* pos, int color, int* mg_score, int* eg_score) {
    int mg = 0, eg = 0;
    int opponent = 1 - color;
    
    // Build occupied bitboard once for efficiency
    Bitboard occupied = 0;
    for (int c = 0; c < 2; c++) {
        for (int p = PAWN; p <= KING; p++) {
            occupied |= pos->pieces[c][p];
        }
    }
    
    // Squares attacked by enemy pawns (we don't want to count moves to these)
    Bitboard enemy_pawn_attacks = 0;
    Bitboard enemy_pawns = pos->pieces[opponent][PAWN];
    
    if (opponent == WHITE) {
        enemy_pawn_attacks |= (enemy_pawns & ~0x0101010101010101ULL) >> 9;  // Left attacks
        enemy_pawn_attacks |= (enemy_pawns & ~0x8080808080808080ULL) >> 7;  // Right attacks
    } else {
        enemy_pawn_attacks |= (enemy_pawns & ~0x0101010101010101ULL) << 7;  // Left attacks
        enemy_pawn_attacks |= (enemy_pawns & ~0x8080808080808080ULL) << 9;  // Right attacks
    }
    
    // Our pieces (exclude king and pawns from mobility)
    Bitboard our_pieces = 0;
    for (int p = PAWN; p <= KING; p++) {
        our_pieces |= pos->pieces[color][p];
    }
    
    // Knight mobility
    Bitboard knights = pos->pieces[color][KNIGHT];
    while (knights) {
        int sq = builtin_ctzll(knights);
        knights &= knights - 1;
        
        Bitboard attacks = get_knight_attacks(sq);
        // Count moves to squares not occupied by our pieces and not attacked by enemy pawns
        Bitboard safe_moves = attacks & ~our_pieces & ~enemy_pawn_attacks;
        int mobility = builtin_popcountll(safe_moves);
        
        if (mobility < 9) {
            mg += knight_mobility_mg[mobility];
            eg += knight_mobility_eg[mobility];
        }
    }
    
    // Bishop mobility
    Bitboard bishops = pos->pieces[color][BISHOP];
    while (bishops) {
        int sq = builtin_ctzll(bishops);
        bishops &= bishops - 1;
        
        Bitboard attacks = get_bishop_attacks(sq, occupied);
        Bitboard safe_moves = attacks & ~our_pieces & ~enemy_pawn_attacks;
        int mobility = builtin_popcountll(safe_moves);
        
        if (mobility < 14) {
            mg += bishop_mobility_mg[mobility];
            eg += bishop_mobility_eg[mobility];
        } else {
            mg += bishop_mobility_mg[13];
            eg += bishop_mobility_eg[13];
        }
    }
    
    // Rook mobility
    Bitboard rooks = pos->pieces[color][ROOK];
    while (rooks) {
        int sq = builtin_ctzll(rooks);
        rooks &= rooks - 1;
        
        Bitboard attacks = get_rook_attacks(sq, occupied);
        Bitboard safe_moves = attacks & ~our_pieces & ~enemy_pawn_attacks;
        int mobility = builtin_popcountll(safe_moves);
        
        if (mobility < 15) {
            mg += rook_mobility_mg[mobility];
            eg += rook_mobility_eg[mobility];
        } else {
            mg += rook_mobility_mg[14];
            eg += rook_mobility_eg[14];
        }
    }
    
    // Queen mobility
    Bitboard queens = pos->pieces[color][QUEEN];
    while (queens) {
        int sq = builtin_ctzll(queens);
        queens &= queens - 1;
        
        Bitboard rook_attacks = get_rook_attacks(sq, occupied);
        Bitboard bishop_attacks = get_bishop_attacks(sq, occupied);
        Bitboard attacks = rook_attacks | bishop_attacks;
        Bitboard safe_moves = attacks & ~our_pieces & ~enemy_pawn_attacks;
        int mobility = builtin_popcountll(safe_moves);
        
        if (mobility < 28) {
            mg += queen_mobility_mg[mobility];
            eg += queen_mobility_eg[mobility];
        } else {
            mg += queen_mobility_mg[27];
            eg += queen_mobility_eg[27];
        }
    }
    
    *mg_score = mg;
    *eg_score = eg;
}

// Pawn Structure Evaluation - Very conservative to work with PST
static void evaluate_pawn_structure(const Position* pos, int color, int* mg_score, int* eg_score) {
    int mg = 0, eg = 0;
    int opponent = 1 - color;
    
    Bitboard our_pawns = pos->pieces[color][PAWN];
    Bitboard opp_pawns = pos->pieces[opponent][PAWN];
    
    // Process each pawn
    while (our_pawns) {
        int sq = builtin_ctzll(our_pawns);
        our_pawns &= our_pawns - 1;  // Clear the bit
        
        int file = sq % 8;
        int rank = sq / 8;
        int relative_rank = (color == WHITE) ? rank : (7 - rank);
        
        Bitboard file_mask = 0x0101010101010101ULL << file;
        
        // 1. Doubled Pawns - VERY conservative (PST already handles this)
        int pawns_on_file = builtin_popcountll(pos->pieces[color][PAWN] & file_mask);
        if (pawns_on_file > 1) {
            mg -= 5 * (pawns_on_file - 1);   // Only -5cp per extra pawn
            eg -= 10 * (pawns_on_file - 1);  // -10cp in endgame
        }
        
        // 2. Isolated Pawns - conservative
        Bitboard adjacent_files = 0;
        if (file > 0) adjacent_files |= 0x0101010101010101ULL << (file - 1);
        if (file < 7) adjacent_files |= 0x0101010101010101ULL << (file + 1);
        
        bool is_isolated = !(pos->pieces[color][PAWN] & adjacent_files);
        if (is_isolated) {
            mg -= 8;   // Only -8cp
            eg -= 12;  // -12cp in endgame
        }
        
        // 3. Phalanx (side-by-side pawns) - small bonus
        bool has_phalanx = false;
        if (file > 0 && (pos->pieces[color][PAWN] & (1ULL << (sq - 1)))) has_phalanx = true;
        if (file < 7 && (pos->pieces[color][PAWN] & (1ULL << (sq + 1)))) has_phalanx = true;
        
        if (has_phalanx && relative_rank >= 3) {  // Only bonus for advanced phalanx
            mg += 3 + relative_rank;  // Small bonus
            eg += 4 + relative_rank;
        }
        
        // 4. Passed Pawns - VERY conservative (PST gives huge bonuses already)
        // Only give bonus in endgame where passed pawns are critical
        Bitboard passed_mask = file_mask;
        if (file > 0) passed_mask |= 0x0101010101010101ULL << (file - 1);
        if (file < 7) passed_mask |= 0x0101010101010101ULL << (file + 1);
        
        Bitboard ahead_mask = 0;
        if (color == WHITE) {
            for (int r = rank + 1; r < 8; r++) {
                ahead_mask |= 0xFFULL << (r * 8);
            }
        } else {
            for (int r = rank - 1; r >= 0; r--) {
                ahead_mask |= 0xFFULL << (r * 8);
            }
        }
        
        bool is_passed = !(opp_pawns & passed_mask & ahead_mask);
        
        if (is_passed && relative_rank >= 4) {  // Only bonus for advanced passed pawns
            // Very small MG bonus, moderate EG bonus
            int passed_eg_bonus = 0;
            
            switch (relative_rank) {
                case 4: passed_eg_bonus = 10; break;
                case 5: passed_eg_bonus = 20; break;
                case 6: passed_eg_bonus = 40; break;
                case 7: passed_eg_bonus = 80; break;
            }
            
            mg += passed_eg_bonus / 4;  // Very small MG bonus
            eg += passed_eg_bonus;
        }
    }
    
    *mg_score = mg;
    *eg_score = eg;
}

Score evaluate(const Position* pos) {
    // Check for insufficient material draw
    if (insufficient_material(pos)) {
        return DRAW_SCORE;
    }
    
    // Check for 50-move rule
    if (pos->halfmove_clock >= 100) {
        return DRAW_SCORE;
    }
    
    // If evaluation is dirty, we need to recalculate from scratch
    // This should rarely happen with proper incremental updates
    if (pos->eval_dirty) {
        Position* mutable_pos = (Position*)pos;  // Cast away const for initialization
        init_evaluation(mutable_pos);
    }
    
    // Use pre-calculated scores from incremental evaluation
    const int total_phase = 24;
    
    // Tapered evaluation: interpolate between MG and EG based on game phase
    Score score = ((pos->mg_score * pos->game_phase) + (pos->eg_score * (total_phase - pos->game_phase))) / total_phase;
    
    // Pawn structure evaluation removed - PST values already handle this well
    
    // Add mobility evaluation (tapered across all phases)
    int white_mob_mg = 0, white_mob_eg = 0;
    int black_mob_mg = 0, black_mob_eg = 0;
    
    evaluate_mobility(pos, WHITE, &white_mob_mg, &white_mob_eg);
    evaluate_mobility(pos, BLACK, &black_mob_mg, &black_mob_eg);
    
    int mob_mg = white_mob_mg - black_mob_mg;
    int mob_eg = white_mob_eg - black_mob_eg;
    
    // Taper mobility evaluation across all game phases
    int mobility_score = ((mob_mg * pos->game_phase) + (mob_eg * (total_phase - pos->game_phase))) / total_phase;
    score += mobility_score;
    
    // Add king safety evaluation (only in middlegame/opening)
    if (pos->game_phase > 12) {  // Only evaluate king safety when enough material
        int white_safety = evaluate_king_safety(pos, WHITE);
        int black_safety = evaluate_king_safety(pos, BLACK);
        
        // Scale king safety by game phase (more important in middlegame)
        int safety_score = ((white_safety - black_safety) * pos->game_phase) / total_phase;
        score += safety_score;
    }
    
    // Return score from side to move perspective
    return (pos->side_to_move == WHITE) ? score : -score;
}

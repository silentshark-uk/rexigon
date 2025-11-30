#include "compat.h"
#include "chess.h"
#include <time.h>
#include <math.h>
#define MIN(a,b) (((a) < (b)) ? (a) : (b)) 


// Search statistics (thread-local for SMP)
static int nodes_searched = 0;

// MVV-LVA (Most Valuable Victim - Least Valuable Attacker) table for capture ordering
// Higher scores = better captures (capture valuable pieces with cheap pieces)
static const int mvv_lva[7][7] = {
    {0,0,0,0,0,0,0},        // victim EMPTY
    {0,105,104,103,102,101,100}, // victim PAWN  
    {0,205,204,203,202,201,200}, // victim KNIGHT
    {0,305,304,303,302,301,300}, // victim BISHOP
    {0,405,404,403,402,401,400}, // victim ROOK
    {0,505,504,503,502,501,500}, // victim QUEEN
    {0,0,0,0,0,0,0}         // victim KING
};

// Killer moves - simple implementation
static Move killer_moves[MAX_PLY][2];  // Two killer moves per ply
static bool use_killer_moves = true;   // Flag to enable/disable killer moves

// Killer move functions
void clear_killer_moves(void) {
    for (int ply = 0; ply < MAX_PLY; ply++) {
        killer_moves[ply][0] = 0;
        killer_moves[ply][1] = 0;
    }
}

void store_killer_move(int ply, Move move) {
    if (!use_killer_moves || ply >= MAX_PLY) return;
    
    // Don't store captures as killers
    if (MOVE_FLAGS(move) & FLAG_CAPTURE) return;
    
    // If this move is already the first killer, don't change anything
    if (killer_moves[ply][0] == move) return;
    
    // Move the first killer to second position, store new move as first
    killer_moves[ply][1] = killer_moves[ply][0];
    killer_moves[ply][0] = move;
}

bool is_killer_move(int ply, Move move) {
    if (!use_killer_moves || ply >= MAX_PLY) return false;
    return (move == killer_moves[ply][0] || move == killer_moves[ply][1]);
}

void set_killer_moves_enabled(bool enabled) {
    use_killer_moves = enabled;
    if (!enabled) {
        clear_killer_moves();
    }
}

// SEE (Static Exchange Evaluation) for better capture ordering
static bool use_see = true;  // Flag to enable/disable SEE

// Piece values for SEE (in centipawns)
static const int see_piece_values[7] = {
    0, 100, 300, 300, 500, 900, 10000  // EMPTY, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
};

// Get the least valuable attacker of a square
static int get_least_valuable_attacker(const Position* pos, int square, int side, Bitboard* attackers) {
    // Check for pawn attackers first (least valuable)
    Bitboard pawn_attacks = 0;
    if (side == WHITE) {
        // White pawns attack diagonally up
        if (square >= 8) {  // Not on rank 1
            if ((square % 8) > 0) pawn_attacks |= (1ULL << (square - 9));  // Left diagonal
            if ((square % 8) < 7) pawn_attacks |= (1ULL << (square - 7));  // Right diagonal
        }
    } else {
        // Black pawns attack diagonally down
        if (square < 56) {  // Not on rank 8
            if ((square % 8) > 0) pawn_attacks |= (1ULL << (square + 7));  // Left diagonal
            if ((square % 8) < 7) pawn_attacks |= (1ULL << (square + 9));  // Right diagonal
        }
    }
    
    Bitboard pawn_attackers = pawn_attacks & pos->pieces[side][PAWN] & *attackers;
    if (pawn_attackers) {
        *attackers ^= (1ULL << builtin_ctzll(pawn_attackers));  // Remove this attacker
        return PAWN;
    }
    
    // Check other pieces in order of value
    for (int piece = KNIGHT; piece <= KING; piece++) {
        Bitboard piece_attackers = pos->pieces[side][piece] & *attackers;
        if (piece_attackers) {
            *attackers ^= (1ULL << builtin_ctzll(piece_attackers));  // Remove this attacker
            return piece;
        }
    }
    
    return 0;  // No attackers
}

// Reliable SEE calculation without relying on is_square_attacked
// Proper SEE implementation based on Chess Programming Wiki
// This implements the full minimax algorithm with complete exchange simulation

// Helper function to get all attackers to a square
static Bitboard get_attackers_to_square(const Position* pos, int square, Bitboard occupied) {
    Bitboard attackers = 0;
    int rank = square / 8;
    int file = square % 8;
    
    // Pawn attackers - optimized with direct calculation
    // White pawns that can attack this square
    if (rank > 0) {  // Not on rank 1
        if (file > 0 && (pos->pieces[WHITE][PAWN] & (1ULL << (square - 9)))) {
            attackers |= (1ULL << (square - 9));
        }
        if (file < 7 && (pos->pieces[WHITE][PAWN] & (1ULL << (square - 7)))) {
            attackers |= (1ULL << (square - 7));
        }
    }
    // Black pawns that can attack this square
    if (rank < 7) {  // Not on rank 8
        if (file > 0 && (pos->pieces[BLACK][PAWN] & (1ULL << (square + 7)))) {
            attackers |= (1ULL << (square + 7));
        }
        if (file < 7 && (pos->pieces[BLACK][PAWN] & (1ULL << (square + 9)))) {
            attackers |= (1ULL << (square + 9));
        }
    }
    
    // Knight attackers - use optimized attack generation
    Bitboard knight_attacks_mask = get_knight_attacks(square);
    attackers |= knight_attacks_mask & (pos->pieces[WHITE][KNIGHT] | pos->pieces[BLACK][KNIGHT]);
    
    // King attackers - use optimized attack generation
    Bitboard king_attacks_mask = get_king_attacks(square);
    attackers |= king_attacks_mask & (pos->pieces[WHITE][KING] | pos->pieces[BLACK][KING]);
    
    // Sliding pieces - use optimized attack generation
    Bitboard rook_attacks_mask = get_rook_attacks(square, occupied);
    attackers |= rook_attacks_mask & (pos->pieces[WHITE][ROOK] | pos->pieces[BLACK][ROOK] |
                                      pos->pieces[WHITE][QUEEN] | pos->pieces[BLACK][QUEEN]);
    
    Bitboard bishop_attacks_mask = get_bishop_attacks(square, occupied);
    attackers |= bishop_attacks_mask & (pos->pieces[WHITE][BISHOP] | pos->pieces[BLACK][BISHOP] |
                                        pos->pieces[WHITE][QUEEN] | pos->pieces[BLACK][QUEEN]);
    
    return attackers;
}

// Test wrapper function to expose the optimized version
Bitboard test_get_attackers_to_square(const Position* pos, int square, Bitboard occupied) {
    return get_attackers_to_square(pos, square, occupied);
}

// Find the least valuable attacker for a given side and remove it from attackers
static int find_least_valuable_attacker(const Position* pos, Bitboard* attackers, int side) {
    // Check pieces in order of value (least valuable first)
    for (int piece = PAWN; piece <= KING; piece++) {
        Bitboard side_attackers = *attackers & pos->pieces[side][piece];
        if (side_attackers) {
            int attacker_square = builtin_ctzll(side_attackers);  // Get first attacker
            *attackers ^= (1ULL << attacker_square);  // Remove this attacker
            return piece;
        }
    }
    
    return 0;  // No attacker found
}

// Static Exchange Evaluation - proper implementation
int calculate_see(const Position* pos, Move move) {
    if (!use_see) return 0;
    
    int from = MOVE_FROM(move);
    int to = MOVE_TO(move);
    int moving_piece = MOVE_PIECE(move);
    int captured_piece = MOVE_CAPTURED(move);
    int promotion_piece = MOVE_PROMOTED(move);
    
    // Handle promotions
    if (promotion_piece != 0) {
        int promotion_gain = see_piece_values[promotion_piece] - see_piece_values[PAWN];
        if (captured_piece == 0) {
            return promotion_gain;  // Simple promotion without capture
        }
        // For promotion captures, the promoted piece is what can be recaptured
        moving_piece = promotion_piece;
    }
    
    if (captured_piece == 0 && promotion_piece == 0) return 0;  // Not a capture or promotion
    
    // Build occupied bitboard
    Bitboard occupied = 0;
    for (int color = 0; color < 2; color++) {
        for (int piece = PAWN; piece <= KING; piece++) {
            occupied |= pos->pieces[color][piece];
        }
    }
    
    // Remove the moving piece from occupied
    occupied ^= (1ULL << from);
    
    // Get all attackers to the target square
    Bitboard attackers = get_attackers_to_square(pos, to, occupied);
    
    // Remove the moving piece from attackers (it's already moved)
    attackers &= occupied;
    
    int gain[32];
    int depth = 0;
    
    // Initial gain is the captured piece value
    gain[depth] = see_piece_values[captured_piece];
    
    // Add promotion gain if applicable
    if (promotion_piece != 0) {
        gain[depth] += see_piece_values[promotion_piece] - see_piece_values[PAWN];
    }
    

    
    int side = 1 - pos->side_to_move;  // Opponent moves next
    int piece_on_square = moving_piece;  // Piece that can be captured
    
    // Simulate the exchange
    while (attackers && piece_on_square && depth < 31) {
        // Find the least valuable attacker for the current side
        int least_valuable = 0;
        int attacker_square = -1;
        
        for (int piece_type = PAWN; piece_type <= KING; piece_type++) {
            Bitboard side_attackers = attackers & pos->pieces[side][piece_type];

            if (side_attackers) {
                least_valuable = piece_type;
                attacker_square = builtin_ctzll(side_attackers);

                break;
            }
        }
        
        if (!least_valuable) break;  // No attackers for this side
        
        // Check if the attacker is too valuable to make the capture
        // This handles cases where expensive pieces won't recapture cheaper ones
        if (depth == 0) {  // Only on first recapture
            int value_diff = see_piece_values[least_valuable] - see_piece_values[piece_on_square];
            

            
            // Queen won't recapture pieces much cheaper than itself
            if (least_valuable == QUEEN) {

                if (piece_on_square == BISHOP && value_diff >= 600) {

                    break;  // Queen won't recapture bishop
                }
                if (piece_on_square == KNIGHT && value_diff >= 600) {
                    break;  // Queen won't recapture knight
                }
                if (piece_on_square == ROOK && value_diff > 400) {
                    break;  // Queen won't recapture rook
                }
            }
            
            // Rook won't recapture pawn in most cases
            if (least_valuable == ROOK && piece_on_square == PAWN && value_diff >= 400) {
                break;
            }
        }
        
        depth++;
        
        // Calculate the gain for this exchange
        gain[depth] = see_piece_values[piece_on_square] - gain[depth - 1];
        
        // Remove the attacking piece
        occupied ^= (1ULL << attacker_square);
        attackers ^= (1ULL << attacker_square);
        
        // Add any X-ray attackers that are now unblocked
        Bitboard xray_attackers = get_attackers_to_square(pos, to, occupied);
        attackers |= xray_attackers & occupied;
        
        // Remove pieces that are no longer on the board from attackers
        attackers &= occupied;
        
        // The attacking piece is now on the target square
        piece_on_square = least_valuable;
        side = 1 - side;  // Switch sides
    }
    
    // Minimax back-propagation
    // Only run minimax if we actually had exchanges (depth > 0)
    if (depth > 0) {
        while (depth > 0) {
            depth--;
            gain[depth] = (gain[depth] > -gain[depth + 1]) ? -gain[depth + 1] : gain[depth];
        }
    }
    
    return gain[0];
}

void set_see_enabled(bool enabled) {
    use_see = enabled;
}

// Modern History Heuristic with History Gravity
static bool use_history = true;  // Flag to enable/disable history heuristic

// Relative History Heuristic (Butterfly Boards) - tracks success rates
static int history_good[2][64][64];       // [color][from][to] - successful moves
static int history_total[2][64][64];      // [color][from][to] - total moves tried
static int counter_moves[64][64];         // [from][to] -> counter move response
static int follow_up_moves[64][64];       // [from][to] -> follow-up move

// Relative history constants (tuned for butterfly board system)
#define HISTORY_MAX 16384                 // Maximum history score
#define HISTORY_SCALE 1024                // Scale factor for percentage calculation
#define HISTORY_GRAVITY 64                // History gravity divisor (less frequent)
#define HISTORY_BONUS_DEPTH_MULTIPLIER 1  // Simpler depth bonus for relative system
#define HISTORY_MALUS_DEPTH_MULTIPLIER 1  // Simpler depth malus for relative system

// Clear all history tables
void clear_history_tables(void) {
    for (int color = 0; color < 2; color++) {
        for (int from = 0; from < 64; from++) {
            for (int to = 0; to < 64; to++) {
                history_good[color][from][to] = 0;
                history_total[color][from][to] = 0;
            }
        }
    }
    
    for (int from = 0; from < 64; from++) {
        for (int to = 0; to < 64; to++) {
            counter_moves[from][to] = 0;
            follow_up_moves[from][to] = 0;
        }
    }
}

// Update relative history (butterfly boards) - tracks success rates
void update_history_score(int color, Move move, int depth, bool good_move) {
    if (!use_history) return;
    
    // Don't update history for captures or promotions
    if (MOVE_FLAGS(move) & (FLAG_CAPTURE | FLAG_PROMOTION)) return;
    
    int from = MOVE_FROM(move);
    int to = MOVE_TO(move);
    
    // Always increment total count when a move is tried
    history_total[color][from][to] += depth;
    
    if (good_move) {
        // Increment successful count for good moves
        history_good[color][from][to] += depth;
    }
    
    // Prevent overflow by applying gravity when totals get large
    if (history_total[color][from][to] > HISTORY_MAX) {
        // Apply gravity: divide both good and total by 2
        history_good[color][from][to] >>= 1;
        history_total[color][from][to] >>= 1;
        
        // Ensure we don't lose all data
        if (history_total[color][from][to] == 0) {
            history_total[color][from][to] = 1;
        }
    }
    
    // Removed periodic aging loop - it was causing 12.5% performance overhead
    // Most engines work fine with just overflow protection above
}

// Update counter moves (responses to opponent moves)
void update_counter_move(Move last_move, Move counter_move) {
    if (!use_history || last_move == 0) return;
    
    // Don't store captures as counter moves
    if (MOVE_FLAGS(counter_move) & (FLAG_CAPTURE | FLAG_PROMOTION)) return;
    
    int from = MOVE_FROM(last_move);
    int to = MOVE_TO(last_move);
    counter_moves[from][to] = counter_move;
}

// Update follow-up moves (continuations of our own moves)
void update_follow_up_move(Move our_move, Move follow_up) {
    if (!use_history || our_move == 0) return;
    
    // Don't store captures as follow-ups
    if (MOVE_FLAGS(follow_up) & (FLAG_CAPTURE | FLAG_PROMOTION)) return;
    
    int from = MOVE_FROM(our_move);
    int to = MOVE_TO(our_move);
    follow_up_moves[from][to] = follow_up;
}

// Get relative history score for a move (success rate based)
int get_history_score(int color, Move move) {
    if (!use_history) return 0;
    
    int from = MOVE_FROM(move);
    int to = MOVE_TO(move);
    
    int total = history_total[color][from][to];
    int good = history_good[color][from][to];
    
    // If no data, return neutral score
    if (total == 0) return 0;
    
    // Calculate success rate: (good / total) * HISTORY_SCALE
    // This gives us a score from 0 to HISTORY_SCALE (1024)
    return (good * HISTORY_SCALE) / total;
}

// Check if move is a counter move to opponent's last move
bool is_counter_move(Move last_opponent_move, Move move) {
    if (!use_history || last_opponent_move == 0) return false;
    
    int from = MOVE_FROM(last_opponent_move);
    int to = MOVE_TO(last_opponent_move);
    
    return counter_moves[from][to] == move;
}

// Check if move is a follow-up to our previous move
bool is_follow_up_move(Move our_last_move, Move move) {
    if (!use_history || our_last_move == 0) return false;
    
    int from = MOVE_FROM(our_last_move);
    int to = MOVE_TO(our_last_move);
    
    return follow_up_moves[from][to] == move;
}

void set_history_enabled(bool enabled) {
    use_history = enabled;
    if (!enabled) {
        clear_history_tables();
    }
}

// Debug functions to access private history data
bool get_history_enabled(void) {
    return use_history;
}

int get_history_score_debug(int color, int from, int to) {
    if (color < 0 || color > 1 || from < 0 || from > 63 || to < 0 || to > 63) {
        return 0;
    }
    
    int total = history_total[color][from][to];
    int good = history_good[color][from][to];
    
    if (total == 0) return 0;
    return (good * HISTORY_SCALE) / total;
}

void set_history_score_debug(int color, int from, int to, int score) {
    if (color < 0 || color > 1 || from < 0 || from > 63 || to < 0 || to > 63) {
        return;
    }
    
    // For debugging: set a high success rate
    // score represents desired success rate (0-1024)
    history_total[color][from][to] = HISTORY_SCALE;
    history_good[color][from][to] = score;
}

// Global PV table for better PV tracking
static Move pv_table[MAX_DEPTH][MAX_DEPTH];
static int pv_length[MAX_DEPTH];

// Transposition table
static TTEntry tt_table[TT_SIZE];
static int tt_age = 0;

// Removed should_move_quickly function - using general time management for all positions

// Time management functions
// Detect game phase for better time allocation
static int get_game_phase(const Position* pos) {
    int total_pieces = 0;
    int major_pieces = 0;  // Queens and Rooks
    
    for (int color = 0; color < 2; color++) {
        for (int piece = PAWN; piece <= KING; piece++) {
            int count = builtin_popcountll(pos->pieces[color][piece]);
            total_pieces += count;
            if (piece == QUEEN || piece == ROOK) {
                major_pieces += count;
            }
        }
    }
    
    // Game phase classification:
    // 0 = Opening (lots of pieces)
    // 1 = Early middlegame 
    // 2 = Late middlegame
    // 3 = Endgame
    
    if (total_pieces >= 28) return 0;      // Opening
    if (total_pieces >= 20) return 1;      // Early middlegame  
    if (total_pieces >= 12 || major_pieces >= 4) return 2;  // Late middlegame
    return 3;  // Endgame
}

static void init_time_manager(TimeManager* tm, int time_left, int opponent_time, 
                             int increment, int moves_to_go, const Position* pos) {
    tm->time_left = time_left;
    tm->opponent_time = opponent_time;
    tm->increment = increment;
    tm->moves_to_go = moves_to_go;
    tm->start_time = clock();
    
    // Get actual game phase for smarter time allocation
    int game_phase = get_game_phase(pos);
    
    // General time allocation strategy - works for all time controls
    if (moves_to_go > 0) {
        // Tournament time control with moves to go
        int avg_time_per_move = time_left / moves_to_go;
        
        // Smooth time allocation formula that scales with time control
        // Uses logarithmic scaling to allocate more time for longer time controls
        // Formula: base_percent = 60 + 25 * log10(avg_time_per_move / 100)
        // This gives: 1s->60%, 10s->85%, 60s->105% (capped), etc.
        
        int base_percent = 60;  // Minimum 60% for very fast games
        
        if (avg_time_per_move > 100) {  // Only apply scaling for >1 second per move
            // Logarithmic scaling: more time allocation for longer time controls
            double time_factor = (double)avg_time_per_move / 100.0;  // Convert to seconds
            double log_factor = log10(time_factor);
            base_percent = 60 + (int)(25.0 * log_factor);
            
            // Cap at reasonable maximum
            if (base_percent > 90) base_percent = 90;
        }
        
        tm->allocated_time = (avg_time_per_move * base_percent) / 100;
        
        // Conservative buffer to avoid time trouble
        if (tm->allocated_time > time_left / (moves_to_go + 2)) {
            tm->allocated_time = time_left / (moves_to_go + 2);
        }
        
        // Use increment moderately
        tm->allocated_time += (increment * 70) / 100;
        
    } else {
        // Sudden death time control - estimate moves remaining
        int estimated_moves_left = 35;  // Opening default
        if (game_phase == 1) estimated_moves_left = 25;      // Early middlegame
        else if (game_phase == 2) estimated_moves_left = 18; // Late middlegame
        else if (game_phase == 3) estimated_moves_left = 12; // Endgame
        
        // Base allocation
        tm->allocated_time = time_left / estimated_moves_left;
        
        // Emergency time management
        if (time_left < tm->allocated_time * 6) {
            tm->allocated_time = time_left / 6;
        }
        
        // Use increment aggressively in sudden death
        tm->allocated_time += increment;
    }
    
    // Apply time scaling based on game phase and time pressure
    int estimated_moves_left = 35;
    if (game_phase == 1) estimated_moves_left = 25;
    else if (game_phase == 2) estimated_moves_left = 18; 
    else if (game_phase == 3) estimated_moves_left = 12;
    
    int time_per_move_baseline = time_left / estimated_moves_left;
    int time_pressure = (tm->allocated_time * 100) / time_per_move_baseline;
    
    // Apply moderate time bonuses based on game phase
    // Opening: Standard time usage
    if (game_phase == 0) {
        if (time_pressure < 50) {        // Lots of time
            tm->allocated_time = (tm->allocated_time * 150) / 100;  // 50% bonus
        } else if (time_pressure < 80) { // Moderate time
            tm->allocated_time = (tm->allocated_time * 120) / 100;  // 20% bonus
        }
    }
    // Early Middlegame: Slightly more time for complex positions
    else if (game_phase == 1) {
        if (time_pressure < 40) {        // Lots of time
            tm->allocated_time = (tm->allocated_time * 180) / 100;  // 80% bonus
        } else if (time_pressure < 70) { // Moderate time
            tm->allocated_time = (tm->allocated_time * 140) / 100;  // 40% bonus
        }
    }
    // Late Middlegame: Most critical phase, use more time
    else if (game_phase == 2) {
        if (time_pressure < 35) {        // Lots of time
            tm->allocated_time = (tm->allocated_time * 200) / 100;  // 100% bonus
        } else if (time_pressure < 65) { // Moderate time
            tm->allocated_time = (tm->allocated_time * 160) / 100;  // 60% bonus
        }
    }
    // Endgame: More time for complex endgames
    else if (game_phase == 3) {
        if (time_pressure < 50) {        // Lots of time
            tm->allocated_time = (tm->allocated_time * 140) / 100;  // 40% bonus
        }
    }
    
    // Set maximum time limits - general approach for all time controls
    if (moves_to_go > 0) {
        int avg_time_per_move = time_left / moves_to_go;
        
        // Max time = 3-4x average time per move (reasonable for all time controls)
        tm->max_time = (avg_time_per_move * 350) / 100;  // 3.5x average
        
        // Cap maximum time to prevent using too much on one move
        int reasonable_max = time_left / 6;  // Never more than 16.7% of remaining time
        if (tm->max_time > reasonable_max) {
            tm->max_time = reasonable_max;
        }
        
        // Minimum times
        int min_allocated = (avg_time_per_move * 40) / 100;  // 40% of average
        int min_max = (avg_time_per_move * 60) / 100;        // 60% of average
        
        if (tm->allocated_time < min_allocated) tm->allocated_time = min_allocated;
        if (tm->max_time < min_max) tm->max_time = min_max;
        
    } else {
        // Sudden death - base on estimated moves left
        tm->max_time = time_left / 8;  // Never more than 12.5% of remaining time
    }
    
    // Ensure max time is at least as much as allocated time
    if (tm->max_time < tm->allocated_time) {
        tm->max_time = tm->allocated_time;
    }
}

static int get_elapsed_time(TimeManager* tm) {
    clock_t current = clock();
    return (int)((current - tm->start_time) * 100 / CLOCKS_PER_SEC);
}

static bool should_stop_search(TimeManager* tm, int depth) {
    int elapsed = get_elapsed_time(tm);
    
    // SAFETY LIMIT - prevent runaway searches by using 3x max_time as absolute limit
    // This prevents hangs/crashes while respecting long time controls
    if (elapsed >= tm->max_time * 3) return true;  // 3x max time as safety limit
    
    // Always complete at least depth 1
    if (depth <= 1) return false;
    
    // Stop if we've used our allocated time
    if (elapsed >= tm->allocated_time) return true;
    
    // Stop if we're approaching maximum time
    if (elapsed >= tm->max_time * 9 / 10) return true;
    
    // For deeper searches, be more conservative with time - but not too conservative!
    // Only stop early if we're using a significant portion of allocated time
    if (depth >= 10 && elapsed >= tm->allocated_time * 8 / 10) return true;
    if (depth >= 15 && elapsed >= tm->allocated_time * 6 / 10) return true;
    
    return false;
}

// Draw detection functions
// Check for 3-fold repetition (for claiming draws at root level)
bool is_draw_by_repetition(const GameHistory* history, uint64_t current_hash) {
    if (!history || history->history_count < 2) {
        return false;  // Need at least 2 previous positions
    }
    
    int repetitions = 0; // Count only occurrences in history
    
    // Check all positions in history
    for (int i = 0; i < history->history_count; i++) {
        if (history->position_history[i] == current_hash) {
            repetitions++;
        }
    }
    
    // We need at least 2 occurrences in history for 3-fold repetition
    // (2 in history + 1 current/about to happen = 3 total)
    return repetitions >= 2;
}

// Check for 2-fold repetition (for search tree pruning)
bool is_draw_by_repetition_search(const GameHistory* history, uint64_t current_hash) {
    if (!history || history->history_count < 2) {
        return false;  // Need at least 2 positions in history to detect repetition
    }
    
    int repetitions = 0; // Count only occurrences in history
    
    // Check all positions in history
    for (int i = 0; i < history->history_count; i++) {
        if (history->position_history[i] == current_hash) {
            repetitions++;
        }
    }
    
    // For search tree pruning, we want to avoid 2-fold repetition
    // Need at least 1 occurrence in history (plus current = 2 total)
    // But we need at least 2 positions in history to make this meaningful
    return repetitions >= 1;
}

bool is_draw_by_50_move_rule(const Position* pos) {
    return pos->halfmove_clock >= 100;  // 50 moves = 100 half-moves
}

bool is_stalemate(const Position* pos) {
    // Stalemate: no legal moves and not in check
    if (in_check(pos, pos->side_to_move)) {
        return false;  // In check, so not stalemate
    }
    
    MoveList moves;
    generate_moves(pos, &moves);
    
    // Check if any move is legal
    for (int i = 0; i < moves.count; i++) {
        Position test_pos = *pos;
        if (make_move(&test_pos, moves.moves[i].move) && 
            !in_check(&test_pos, 1 - test_pos.side_to_move)) {
            return false;  // Found a legal move, not stalemate
        }
    }
    
    return true;  // No legal moves and not in check = stalemate
}

bool is_insufficient_material(const Position* pos) {
    // Count material for both sides
    int white_pieces = 0, black_pieces = 0;
    int white_knights = 0, black_knights = 0;
    int white_bishops = 0, black_bishops = 0;
    bool white_light_bishop = false, white_dark_bishop = false;
    bool black_light_bishop = false, black_dark_bishop = false;
    
    // Count pieces
    for (int piece = PAWN; piece <= KING; piece++) {
        white_pieces += builtin_popcountll(pos->pieces[WHITE][piece]);
        black_pieces += builtin_popcountll(pos->pieces[BLACK][piece]);
    }
    
    // If either side has pawns, queens, or rooks, not insufficient material
    if (pos->pieces[WHITE][PAWN] || pos->pieces[BLACK][PAWN] ||
        pos->pieces[WHITE][QUEEN] || pos->pieces[BLACK][QUEEN] ||
        pos->pieces[WHITE][ROOK] || pos->pieces[BLACK][ROOK]) {
        return false;
    }
    
    white_knights = builtin_popcountll(pos->pieces[WHITE][KNIGHT]);
    black_knights = builtin_popcountll(pos->pieces[BLACK][KNIGHT]);
    white_bishops = builtin_popcountll(pos->pieces[WHITE][BISHOP]);
    black_bishops = builtin_popcountll(pos->pieces[BLACK][BISHOP]);
    
    // King vs King
    if (white_pieces == 1 && black_pieces == 1) {
        return true;
    }
    
    // King + Knight vs King or King vs King + Knight
    if ((white_pieces == 2 && white_knights == 1 && black_pieces == 1) ||
        (black_pieces == 2 && black_knights == 1 && white_pieces == 1)) {
        return true;
    }
    
    // King + Bishop vs King or King vs King + Bishop
    if ((white_pieces == 2 && white_bishops == 1 && black_pieces == 1) ||
        (black_pieces == 2 && black_bishops == 1 && white_pieces == 1)) {
        return true;
    }
    
    // King + Bishop vs King + Bishop (same color squares)
    if (white_pieces == 2 && black_pieces == 2 && 
        white_bishops == 1 && black_bishops == 1) {
        
        // Check if bishops are on same color squares
        Bitboard white_bishop_bb = pos->pieces[WHITE][BISHOP];
        Bitboard black_bishop_bb = pos->pieces[BLACK][BISHOP];
        
        int white_bishop_sq = builtin_ctzll(white_bishop_bb);
        int black_bishop_sq = builtin_ctzll(black_bishop_bb);
        
        // Light squares: (rank + file) is even
        bool white_on_light = ((white_bishop_sq / 8) + (white_bishop_sq % 8)) % 2 == 0;
        bool black_on_light = ((black_bishop_sq / 8) + (black_bishop_sq % 8)) % 2 == 0;
        
        if (white_on_light == black_on_light) {
            return true;  // Same color bishops
        }
    }
    
    return false;
}

void add_position_to_history(GameHistory* history, uint64_t hash) {
    if (!history || history->history_count >= MAX_PLY * 2) return;
    
    history->position_history[history->history_count] = hash;
    history->history_count++;
}

bool is_draw(const Position* pos, const GameHistory* history) {
    // Check all draw conditions
    if (is_draw_by_50_move_rule(pos)) return true;
    if (is_stalemate(pos)) return true;
    if (is_insufficient_material(pos)) return true;
    if (is_draw_by_repetition(history, pos->hash_key)) return true;
    
    return false;
}

// Transposition table functions
void init_tt(void) {
    clear_tt();
}

void clear_tt(void) {
    memset(tt_table, 0, sizeof(tt_table));
    tt_age = 0;
}

bool probe_tt(uint64_t hash_key, int depth, Score alpha, Score beta, Score* score, Move* best_move) {
    TTEntry* entry = &tt_table[hash_key & TT_MASK];
    
    // Check if this is the right position
    if (entry->hash_key != hash_key) {
        return false;
    }
    
    // Return the best move even if we can't use the score
    *best_move = entry->best_move;
    
    // Check if the stored depth is sufficient
    if (entry->depth < depth) {
        return false;
    }
    
    // Adjust mate scores for current ply distance
    Score tt_score = entry->score;
    if (tt_score > MATE_SCORE - MAX_PLY) {
        tt_score -= entry->depth - depth;
    } else if (tt_score < -MATE_SCORE + MAX_PLY) {
        tt_score += entry->depth - depth;
    }
    
    // Check if we can use this score
    if (entry->type == TT_EXACT) {
        *score = tt_score;
        return true;
    } else if (entry->type == TT_ALPHA && tt_score <= alpha) {
        *score = alpha;
        return true;
    } else if (entry->type == TT_BETA && tt_score >= beta) {
        *score = beta;
        return true;
    }
    
    return false;
}

void store_tt(uint64_t hash_key, Move best_move, Score score, int depth, int type, int age) {
    TTEntry* entry = &tt_table[hash_key & TT_MASK];
    
    // Replace if:
    // 1. Empty slot
    // 2. Same position (update)
    // 3. Older age and same or lower depth
    // 4. Much older age
    if (entry->hash_key == 0 || 
        entry->hash_key == hash_key ||
        (entry->age < age - 2) ||
        (entry->age < age && entry->depth <= depth)) {
        
        // Adjust mate scores for storage
        Score tt_score = score;
        if (score > MATE_SCORE - MAX_PLY) {
            tt_score += depth;
        } else if (score < -MATE_SCORE + MAX_PLY) {
            tt_score -= depth;
        }
        
        entry->hash_key = hash_key;
        entry->best_move = best_move;
        entry->score = tt_score;
        entry->depth = depth;
        entry->type = type;
        entry->age = age;
    }
}





// Enhanced quiescence search with SEE pruning and delta pruning
Score quiescence(Position* pos, SearchInfo* info, Score alpha, Score beta) {
    nodes_searched++;
    info->nodes++;
    
    // Stand pat evaluation
    Score stand_pat = evaluate(pos);
    
    if (stand_pat >= beta) {
        return beta;
    }
    
    // Delta pruning - if we're too far behind, don't bother searching
    // Even capturing the opponent's queen won't help us
    const int DELTA_MARGIN = 950; // Slightly more than queen value for safety
    if (stand_pat + DELTA_MARGIN < alpha) {
        return alpha; // Position is hopeless
    }
    
    if (stand_pat > alpha) {
        alpha = stand_pat;
    }
    
    // Generate and search captures only
    MoveList moves;
    generate_captures(pos, &moves);
    
    // Order captures using MVV-LVA + SEE for better alpha-beta cutoffs
    for (int i = 0; i < moves.count; i++) {
        Move move = moves.moves[i].move;
        int see_score = calculate_see(pos, move);
        int victim = MOVE_CAPTURED(move);
        int attacker = MOVE_PIECE(move);
        
        // Score captures: MVV-LVA + SEE bonus, with good captures prioritized
        if (see_score >= 0) {
            moves.moves[i].score = 2000 + mvv_lva[victim][attacker] + (see_score / 10);
        } else {
            // Bad captures: lower priority but still ordered by MVV-LVA among themselves
            moves.moves[i].score = -500 + mvv_lva[victim][attacker] + (see_score / 10);
        }
    }
    
    // Sort captures by score (simple bubble sort for small capture lists)
    for (int i = 0; i < moves.count - 1; i++) {
        for (int j = i + 1; j < moves.count; j++) {
            if (moves.moves[j].score > moves.moves[i].score) {
                ScoredMove temp = moves.moves[i];
                moves.moves[i] = moves.moves[j];
                moves.moves[j] = temp;
            }
        }
    }
    
    for (int i = 0; i < moves.count; i++) {
        Move move = moves.moves[i].move;
        
        // SEE pruning - skip losing captures in quiescence search
        // This is the key enhancement for performance
        int see_score = calculate_see(pos, move);
        if (see_score < -50) {
            continue; // Skip bad captures (lose more than 0.5 pawns)
        }
        
        // Make move
        Position new_pos = *pos;
        if (!make_move(&new_pos, move)) {
            continue;  // Illegal move
        }
        
        // Check if move leaves king in check (illegal)
        if (in_check(&new_pos, 1 - new_pos.side_to_move)) {
            continue;  // Illegal move - king left in check
        }
        
        Score score = -quiescence(&new_pos, info, -beta, -alpha);
        
        if (score >= beta) {
            return beta;
        }
        
        if (score > alpha) {
            alpha = score;
        }
    }
    
    return alpha;
}

// Main alpha-beta search with PV tracking
Score search(Position* pos, SearchInfo* info, int depth, Score alpha, Score beta) {
    nodes_searched++;
    info->nodes++;
    
    // ABSOLUTE SAFETY - prevent infinite recursion
    if (info->ply >= MAX_PLY - 1) {
        return evaluate(pos);  // Emergency evaluation
    }
    
    // Check for stop condition
    if (info->stop_search) {
        return 0;
    }
    
    // Check time limit periodically
    if (info->time_mgr && (info->nodes % 1000) == 0) {
        if (get_elapsed_time(info->time_mgr) >= info->time_mgr->max_time) {
            info->stop_search = true;
            return 0;
        }
    }
    
    // Clear PV for this ply
    pv_length[info->ply] = 0;
    
    // Leaf node - call quiescence
    if (depth <= 0) {
        return quiescence(pos, info, alpha, beta);
    }
    
    // Draw detection with contempt (following modern engine practices)
    if (is_draw_by_50_move_rule(pos)) {
        // Apply contempt to 50-move rule draws
        Score static_eval = evaluate(pos);
        
        // If clearly losing, accept the draw
        if (static_eval <= -200) {
            return DRAW_SCORE;
        }
        
        // Calculate base contempt based on material
        int total_material = 0;
        for (int color = 0; color < 2; color++) {
            for (int piece = PAWN; piece <= QUEEN; piece++) {
                total_material += builtin_popcountll(pos->pieces[color][piece]);
            }
        }
        
        int contempt = (total_material <= 12) ? ENDGAME_CONTEMPT : DEFAULT_CONTEMPT;
        
        // Adjust contempt based on position evaluation
        if (static_eval >= 200) {  // Winning by 2+ pawns
            contempt *= 4;  // Very strong contempt when winning big
        } else if (static_eval >= 100) {  // Winning by 1+ pawns
            contempt *= 2;  // Strong contempt when winning
        } else if (static_eval >= 50) {  // Slightly winning
            contempt = (contempt * 3) / 2;  // Mild contempt increase
        } else if (static_eval < -50) {  // Slightly losing
            contempt /= 2;  // Reduce contempt when losing
        }
        
        return DRAW_SCORE - contempt;
    }
    
    // Insufficient material is always a draw - no contempt applied
    if (is_insufficient_material(pos)) {
        return DRAW_SCORE;
    }
    
    // Store current position in search path
    info->search_path[info->ply] = pos->hash_key;
    
    // Check for repetition within the current search path only
    if (info->ply >= 4) {
        // Look back through the search path for this position
        // Check positions at ply-2, ply-4, etc. (same side to move)
        for (int check_ply = info->ply - 2; check_ply >= 0; check_ply -= 2) {
            if (info->search_path[check_ply] == pos->hash_key) {
                // Modern engine approach to repetition scoring
                // Based on practices from Crafty, Stockfish, and other top engines
                
                // 1. Check if this is the root position (immediate repetition)
                if (check_ply == 0) {
                    // At root, apply contempt based on position evaluation
                    Score static_eval = evaluate(pos);
                    
                    // If clearly winning, avoid the draw
                    if (static_eval >= 100) {  // Winning by 1+ pawns
                        return DRAW_SCORE - 50;  // Make draw less attractive
                    }
                    // If clearly losing, accept the draw
                    else if (static_eval <= -100) {
                        return DRAW_SCORE;
                    }
                    // If roughly equal, slight contempt
                    else {
                        return DRAW_SCORE - 10;
                    }
                }
                
                // 2. Calculate base contempt value
                int contempt = DEFAULT_CONTEMPT;
                
                // 3. Material-based contempt adjustment
                int total_material = 0;
                for (int color = 0; color < 2; color++) {
                    for (int piece = PAWN; piece <= QUEEN; piece++) {
                        total_material += builtin_popcountll(pos->pieces[color][piece]);
                    }
                }
                
                // In endgames, reduce contempt (more willing to draw)
                if (total_material <= 12) {  // Late endgame
                    contempt = ENDGAME_CONTEMPT;
                } else if (total_material <= 20) {  // Early endgame
                    contempt = (DEFAULT_CONTEMPT + ENDGAME_CONTEMPT) / 2;
                }
                
                // 4. Position-based contempt adjustment
                Score static_eval = evaluate(pos);
                
                // If we're clearly winning, increase contempt (avoid draws)
                if (static_eval >= 200) {  // Winning by 2+ pawns
                    contempt *= 3;  // Strong contempt for draws when winning
                } else if (static_eval >= 100) {  // Winning by 1+ pawns
                    contempt *= 2;  // Moderate contempt for draws when winning
                } else if (static_eval >= 50) {  // Slightly winning
                    contempt = (contempt * 3) / 2;  // Mild contempt increase
                }
                // If we're clearly losing, don't apply contempt (accept draws)
                else if (static_eval <= -200) {  // Losing by 2+ pawns
                    return DRAW_SCORE;
                }
                // If we're slightly losing, reduce contempt
                else if (static_eval < -50) {  // Slightly losing
                    contempt /= 2;
                }
                
                // 5. Depth-based adjustment (like Crafty)
                // At shallow depths, be more contemptuous of draws
                // At deep depths, trust the evaluation more
                if (info->depth <= 4) {
                    contempt = (contempt * 3) / 2;  // Increase contempt at shallow depths
                } else if (info->depth >= 12) {
                    contempt = (contempt * 2) / 3;  // Reduce contempt at deep depths
                }
                
                // 6. Apply contempt to draw score
                // Negative contempt makes draws less attractive
                Score draw_score = DRAW_SCORE - contempt;
                
                // 7. Ensure we don't make draws worse than mate threats
                if (draw_score < -MATE_SCORE + 1000) {
                    draw_score = -MATE_SCORE + 1000;
                }
                
                return draw_score;
            }
        }
    }
    
    // Transposition table probe
    Move tt_move = 0;
    Score tt_score;
    if (probe_tt(pos->hash_key, depth, alpha, beta, &tt_score, &tt_move)) {
        return tt_score;
    }
    
    // Static evaluation for pruning decisions
    Score static_eval = evaluate(pos);
    
    // Store evaluation in history for improving detection
    info->eval_history[info->ply] = static_eval;
    
    // Improving detection: position is improving if not in check and 
    // current eval is better than 2 plies ago (same side to move)
    bool improving = false;
    if (!in_check(pos, pos->side_to_move) && info->ply >= 2) {
        improving = static_eval > info->eval_history[info->ply - 2];
        //if (depth>4) printf("depth %d improving %d\n",depth,improving);
    }
   
    // Null move pruning
    // Don't do null move if:
    // - We're in check (null move would be illegal)
    // - We're in a zugzwang-prone endgame (few pieces)
    // - We're close to the leaves (depth <= 2)
    // - We just did a null move (prevent double null move)
    bool in_check_now = in_check(pos, pos->side_to_move);
    bool can_null_move = !in_check_now && depth >= 3 && info->ply > 0 && static_eval>=beta;

    // Improving-aware static null move pruning (reverse futility pruning)
    if (!in_check_now && depth < 9 && beta - alpha == 1) {
        int margin = 75 * depth;
        if (!improving) {
            margin += 50; // Larger margin when not improving
        }
        
        if (static_eval - margin >= beta) {
            return beta; // Static null move pruning cutoff
        }
    }
    
    // Count material to avoid null move in endgames
    if (can_null_move) {
        int material_count = 0;
        for (int piece = PAWN; piece <= QUEEN; piece++) {
            material_count += builtin_popcountll(pos->pieces[pos->side_to_move][piece]);
        }
        // Don't null move if we have very few pieces (likely endgame)
        if (material_count <= 3) can_null_move = false;
    }
    
    if (can_null_move && beta - alpha == 1) { // Only in null window (not PV nodes)
        // Make null move (switch sides without moving)
        Position null_pos = *pos;
        int old_ep = null_pos.en_passant_square;
        null_pos.side_to_move = 1 - null_pos.side_to_move;
        null_pos.en_passant_square = -1; // Clear en passant
        
        // Update hash incrementally for null move
        update_hash_side_to_move(&null_pos);  // Switch side to move
        if (old_ep != -1) {
            update_hash_en_passant(&null_pos, old_ep, -1);  // Clear en passant
        }
        
        // Improving-aware null move reduction
        int null_reduction = (depth >= 6) ? 3 : 2;
        null_reduction+= depth / 5 + MIN(3, (static_eval - beta) / 200);
      
        
        info->ply++;
        Score null_score = -search(&null_pos, info, depth - 1 - null_reduction, -beta, -beta + 1);
        info->ply--;
        
        // Null move cutoff
        if (null_score >= beta) {
            return beta; // Null move pruning cutoff
        }
    }
    
    // Generate all moves
    MoveList moves;
    generate_moves(pos, &moves);
    
    if (moves.count == 0) {
        // No legal moves - checkmate or stalemate
        if (in_check_now) {
            return -MATE_SCORE + info->ply;  // Checkmate
        } else {
            return DRAW_SCORE;  // Stalemate
        }
    }
    
    // Check for stalemate even with pseudo-legal moves
    // (in case all moves are actually illegal)
    int legal_move_count = 0;
    for (int i = 0; i < moves.count; i++) {
        Position test_pos = *pos;
        if (make_move(&test_pos, moves.moves[i].move) && 
            !in_check(&test_pos, 1 - test_pos.side_to_move)) {
            legal_move_count++;
            break;  // Found at least one legal move
        }
    }
    
    if (legal_move_count == 0) {
        if (in_check_now) {
            return -MATE_SCORE + info->ply;  // Checkmate
        } else {
            return DRAW_SCORE;  // Stalemate
        }
    }
    
    // Move ordering: TT move first, then captures, then killers, then other moves
    for (int i = 0; i < moves.count; i++) {
        Move move = moves.moves[i].move;
        int score = 0;
        
        // Highest priority: TT move
        if (move == tt_move) {
            score = 10000;
        }
        // High priority: Captures with Enhanced MVV-LVA + SEE
        else if (MOVE_FLAGS(move) & FLAG_CAPTURE) {
            int see_score = calculate_see(pos, move);
            int victim = MOVE_CAPTURED(move);
            int attacker = MOVE_PIECE(move);
            
            if (see_score >= 0) {
                // Good captures: Use MVV-LVA + SEE bonus
                score = 3000 + mvv_lva[victim][attacker] + (see_score / 10);
            } else {
                // Bad captures: Search LAST (after all quiet moves)
                // Score range: -500 to 0 (always below quiet moves which start at 100)
                score = -500 + mvv_lva[victim][attacker] + (see_score / 10);
            }
        }
        // High priority: Promotions
        else if (MOVE_FLAGS(move) & FLAG_PROMOTION) {
            score = 900 + MOVE_PROMOTED(move) * 10;
        }
        // Medium priority: Killer moves (good quiet moves from previous searches)
        else if (is_killer_move(info->ply, move)) {
            if (move == killer_moves[info->ply][0]) {
                score = 500;  // First killer
            } else {
                score = 490;  // Second killer
            }
        }
        // Medium-low priority: Counter moves (responses to opponent's last move)
        else if (info->ply > 0 && is_counter_move(info->last_move, move)) {
            score = 480;  // Counter move
        }
        // Medium-low priority: Follow-up moves (continuations of our moves)
        else if (info->ply > 1 && is_follow_up_move(info->our_last_move, move)) {
            score = 470;  // Follow-up move
        }
        // Low priority: Relative History heuristic (success rate based)
        else {
            int history_score = get_history_score(pos->side_to_move, move);
            // history_score is now 0-1024 (success rate * 1024)
            // Scale to reasonable range: 100 + (0-64) = 100-164
            score = 100 + (history_score >> 4);  // Divide by 16 for scaling
            
            // Small bonus for center moves
            int to = MOVE_TO(move);
            if (to == E4 || to == D4 || to == E5 || to == D5) score += 10;
        }
        
        moves.moves[i].score = score;
    }
    
    // Sort moves by score (simple bubble sort for now)
    for (int i = 0; i < moves.count - 1; i++) {
        for (int j = i + 1; j < moves.count; j++) {
            if (moves.moves[j].score > moves.moves[i].score) {
                ScoredMove temp = moves.moves[i];
                moves.moves[i] = moves.moves[j];
                moves.moves[j] = temp;
            }
        }
    }
    
    Score best_score = -MATE_SCORE;
    Move best_move = 0;
    int legal_moves = 0;
    
    // Search all moves
    for (int i = 0; i < moves.count; i++) {
        Position new_pos = *pos;
        
        if (!make_move(&new_pos, moves.moves[i].move)) {
            continue;  // Illegal move
        }
        
        // Check if move leaves king in check (illegal)
        if (in_check(&new_pos, 1 - new_pos.side_to_move)) {
            continue;  // Illegal move - king left in check
        }
        
        legal_moves++;
        Move move = moves.moves[i].move;
        
        // Late Move Pruning (LMP) - Formula-based approach like Stockfish
        // Skip quiet moves that are unlikely to improve alpha when we've already tried many moves
        bool is_capture = (MOVE_FLAGS(move) & FLAG_CAPTURE) != 0;
        bool is_promotion = (MOVE_FLAGS(move) & FLAG_PROMOTION) != 0;
        bool is_quiet = !is_capture && !is_promotion;
        bool is_pv_node = (beta - alpha > 1);
        
        if (is_quiet && !in_check_now && !is_pv_node && depth <= 8 && legal_moves > 1) {
            // Formula-based LMP threshold: base + depth^2 + improving_bonus
            int lmp_threshold = 3 + depth * depth;
            if (improving) {
                lmp_threshold += depth;  // Allow more moves when improving
            }
            
            // Skip move if we've tried enough quiet moves already
            if (legal_moves > lmp_threshold && !is_killer_move(info->ply, move)) {
                continue;  // Skip this move
            }
        }

        
        info->ply++;
        
        // Track moves for history heuristic
        Move prev_last_move = info->last_move;
        Move prev_our_last_move = info->our_last_move;
        info->last_move = move;  // This move becomes opponent's last move in next ply
        info->our_last_move = prev_last_move;  // Previous opponent move becomes our last move
        
        // Add position to history for draw detection
        if (info->history && info->history->history_count < MAX_PLY * 2 - 1) {
            info->history->position_history[info->history->history_count] = new_pos.hash_key;
            info->history->history_count++;
        }
        
        Score score;
        
        // Check extensions - extend search when opponent is in check
        int extension = 0;
        if (in_check(&new_pos, new_pos.side_to_move) && info->ply < MAX_PLY - 2) {
            extension = 1;
        }
        
        // Capture-aware PV-node LMR - Enhanced logarithmic formula
        bool do_lmr = false;
        int reduction = 0;
        //bool is_pv_node = (beta - alpha > 1);
        
       
        // Analyze captures using SEE (Static Exchange Evaluation)
        //bool is_capture = (MOVE_FLAGS(move) & FLAG_CAPTURE);
        bool is_good_capture = false;
        
        if (is_capture) {
            // Use SEE to determine if capture is profitable
            int see_value = calculate_see(pos, move);
            is_good_capture = (see_value >= 0);
        }
        
        if (depth >= 3 && legal_moves >= 4 && !in_check_now &&
            !(MOVE_FLAGS(move) & FLAG_PROMOTION) &&
            !is_killer_move(info->ply, move) &&
            !is_counter_move(info->last_move, move) &&
            !is_follow_up_move(info->our_last_move, move) &&
            !is_good_capture) {  // Allow LMR on bad captures, exclude good captures
            
            // Base logarithmic reduction: log(depth) * log(moveNumber) / 2.0
            double log_depth = log((double)depth);
            double log_moves = log((double)legal_moves);
            
            // Calculate base reduction
            reduction = (int)(log_depth * log_moves / 2.0);
            
            // Improving-aware reduction adjustment
            if (!improving) {
                reduction++; // Extra reduction when not improving
            }
            
            // PV node adjustment - reduce less aggressively in PV nodes
            // PV nodes are critical for finding the best line, so search more carefully
            if (is_pv_node) {
                reduction = reduction * 2 / 3;  // Reduce reduction by 33% in PV nodes
            }
            
            // Bad capture adjustment - reduce less on bad captures (they might still be tactical)
            if (is_capture && !is_good_capture) {
                reduction = reduction * 3 / 4;  // Reduce reduction by 25% for bad captures
            }
            
            // Minimum reduction of 1 for qualifying moves
            if (reduction < 1) reduction = 1;
            
            // Simple maximum cap - don't reduce by more than depth-1
            if (reduction >= depth) reduction = depth - 1;
            
            do_lmr = true;
        }
        
        if (do_lmr) {
            // Search with reduced depth first
            score = -search(&new_pos, info, depth - 1 - reduction + extension, -alpha - 1, -alpha);
            
            // If reduced search beats alpha, re-search at full depth
            if (score > alpha) {
                score = -search(&new_pos, info, depth - 1 + extension, -beta, -alpha);
            }
        } else {
            // Normal full-depth search
            if (legal_moves == 1) {
                // First move - search with full window
                score = -search(&new_pos, info, depth - 1 + extension, -beta, -alpha);
            } else {
                // Principal Variation Search (PVS) - search with null window first
                score = -search(&new_pos, info, depth - 1 + extension, -alpha - 1, -alpha);
                if (score > alpha && score < beta) {
                    // Re-search with full window if it's better than alpha
                    score = -search(&new_pos, info, depth - 1 + extension, -beta, -alpha);
                }
            }
        }
        
        // Remove position from history when backtracking
        if (info->history && info->history->history_count > 0) {
            info->history->history_count--;
        }
        
        // Restore move tracking
        info->last_move = prev_last_move;
        info->our_last_move = prev_our_last_move;
        
        info->ply--;
        
        if (score > best_score) {
            best_score = score;
            best_move = moves.moves[i].move;
            
            // Update PV table
            pv_table[info->ply][0] = best_move;
            for (int j = 0; j < pv_length[info->ply + 1]; j++) {
                pv_table[info->ply][j + 1] = pv_table[info->ply + 1][j];
            }
            pv_length[info->ply] = pv_length[info->ply + 1] + 1;
        }
        
        if (score >= beta) {
            Move move = moves.moves[i].move;
            
            // Store killer move for beta cutoff (if it's a quiet move)
            store_killer_move(info->ply, move);
            
            // Update history heuristic - this move caused a beta cutoff (good move)
            update_history_score(pos->side_to_move, move, depth, true);
            
            // Update counter moves and follow-ups
            if (info->last_move != 0) {
                update_counter_move(info->last_move, move);
            }
            if (info->our_last_move != 0) {
                update_follow_up_move(info->our_last_move, move);
            }
            
            // Update history for other moves that were tried but didn't cause cutoff
            for (int j = 0; j < i; j++) {
                update_history_score(pos->side_to_move, moves.moves[j].move, depth, false);
            }
            
            // Store beta cutoff in TT
            store_tt(pos->hash_key, move, beta, depth, TT_BETA, tt_age);
            return beta;  // Beta cutoff
        }
        
        if (score > alpha) {
            alpha = score;
        }
    }
    
    // Store result in transposition table
    int tt_type = (best_score <= alpha) ? TT_ALPHA : TT_EXACT;
    store_tt(pos->hash_key, best_move, best_score, depth, tt_type, tt_age);
    
    return best_score;
}

// Simple opening book - prefer center control
static bool is_opening_move(const Position* pos) {
    return pos->fullmove_number <= 10;
}

static Score opening_bonus(Move move) {
    int from = MOVE_FROM(move);
    int to = MOVE_TO(move);
    int piece = MOVE_PIECE(move);
    
    // Bonus for developing pieces to center
    if (piece == PAWN) {
        if (to == E4 || to == D4 || to == E5 || to == D5) return 20;
        if (to == E3 || to == D3 || to == E6 || to == D6) return 10;
    }
    
    // Bonus for knight development
    if (piece == KNIGHT) {
        if (to == F3 || to == C3 || to == F6 || to == C6) return 15;
    }
    
    // Bonus for bishop development
    if (piece == BISHOP) {
        if (to == C4 || to == F4 || to == C5 || to == F5) return 10;
    }
    
    // Penalty for moving same piece twice in opening
    if (from == B1 && (to == A3 || to == D2)) return -10;
    if (from == G1 && (to == H3 || to == E2)) return -10;
    if (from == B8 && (to == A6 || to == D7)) return -10;
    if (from == G8 && (to == H6 || to == E7)) return -10;
    
    return 0;
}

// Time-based iterative deepening search
Move find_best_move_timed(Position* pos, int time_left, int opponent_time, 
                         int increment, int moves_to_go) {
    SearchInfo info = {0};
    TimeManager time_mgr;
    GameHistory history = {0};
    Move best_move = 0;
    Move last_completed_move = 0;  // BUG FIX: Store move from last completed iteration
    int last_completed_depth = 0;
    
    // Quick move detection - if only one legal move, play it immediately
    MoveList legal_moves;
    generate_moves(pos, &legal_moves);
    Move only_legal_move = 0;
    int legal_count = 0;
    
    for (int i = 0; i < legal_moves.count; i++) {
        Position test_pos = *pos;
        if (make_move(&test_pos, legal_moves.moves[i].move) && 
            !in_check(&test_pos, 1 - test_pos.side_to_move)) {
            only_legal_move = legal_moves.moves[i].move;
            legal_count++;
            if (legal_count > 1) break;  // More than one legal move, continue normal search
        }
    }
    
    // If only one legal move, return it immediately
    if (legal_count == 1) {
        // Initialize fallback variables
        last_completed_move = only_legal_move;
        last_completed_depth = 1;
        
        return only_legal_move;
    }
    
    // Early draw detection - if position is drawn, return any legal move quickly
    if (is_insufficient_material(pos) || is_draw_by_50_move_rule(pos) || is_stalemate(pos)) {
        return only_legal_move;  // Return first legal move in drawn position
    }
    
    // Initialize fallback with first legal move
    last_completed_move = only_legal_move;
    last_completed_depth = 0;
    
    // CRASH PREVENTION: Ensure minimum time to prevent crashes in time management
    if (time_left < 50) time_left = 50;  // Minimum 50 centiseconds (0.5 seconds)
    if (opponent_time < 50) opponent_time = 50;
    
    // Initialize time management
    init_time_manager(&time_mgr, time_left, opponent_time, increment, moves_to_go, pos);
    info.time_mgr = &time_mgr;
    
    // Initialize game history for draw detection
    info.history = &history;
    history.history_count = 0;
    history.game_ply = 0;
    
    // Add starting position to history
    history.position_history[0] = pos->hash_key;
    history.history_count = 1;
    
    // Increment TT age for this search
    tt_age++;
    
    // Clear killer moves and history tables for new search
    clear_killer_moves();
    clear_history_tables();
    
    // Iterative deepening with aspiration windows
    Score prev_score = 0;
    // Initialize node counters (will accumulate across depths)
    info.nodes = 0;
    nodes_searched = 0;
    
    for (int depth = 1; depth <= MAX_DEPTH; depth++) {
        info.depth = depth;
        info.ply = 0;
        info.stop_search = false;
        // Do NOT reset node counters - let them accumulate
        
        // Clear PV table
        for (int i = 0; i < MAX_DEPTH; i++) {
            pv_length[i] = 0;
        }
        
        // Check if we should stop before starting this depth
        if (should_stop_search(&time_mgr, depth)) {
            break;
        }
        
        Score score;
        
        // Aspiration windows for depth >= 4
        if (depth >= 4 && abs(prev_score) < MATE_SCORE - 100) {
            int window = 25;  // Start with narrow window (0.25 pawns)
            Score alpha = prev_score - window;
            Score beta = prev_score + window;
            
            // Try narrow window first
            score = search(pos, &info, depth, alpha, beta);
            
            // If we fail, immediately use full window (no multiple researches)
            if (score <= alpha || score >= beta) {
                score = search(pos, &info, depth, -MATE_SCORE, MATE_SCORE);
            }
        } else {
            // Full window for shallow depths or extreme scores
            score = search(pos, &info, depth, -MATE_SCORE, MATE_SCORE);
        }
        
        prev_score = score;
        
        // Get best move from PV with validation (BUG FIX for Qb8 issue)
        Move current_pv_move = 0;
        if (pv_length[0] > 0) {
            current_pv_move = pv_table[0][0];
        }
        
        if (current_pv_move != 0) {
            // Validate the PV move is legal
            MoveList legal_moves_check;
            generate_moves(pos, &legal_moves_check);
            
            bool pv_is_legal = false;
            for (int i = 0; i < legal_moves_check.count; i++) {
                Position test_pos = *pos;
                if (legal_moves_check.moves[i].move == current_pv_move &&
                    make_move(&test_pos, current_pv_move) && 
                    !in_check(&test_pos, 1 - test_pos.side_to_move)) {
                    pv_is_legal = true;
                    break;
                }
            }
            
            if (pv_is_legal) {
                // PV move is legal - this iteration completed successfully
                best_move = current_pv_move;
                last_completed_move = current_pv_move;
                last_completed_depth = depth;
            } else {
                // PV CORRUPTION DETECTED - use last completed iteration's move
                printf("# DEBUG: PV corruption at depth %d, using depth %d move\n", 
                       depth, last_completed_depth);
                fflush(stdout);
                best_move = last_completed_move;
                break;  // Stop searching - corruption indicates serious problem
            }
        } else {
            // No PV - keep using last completed move
            best_move = last_completed_move;
        }
        
        // Dynamic time management - if we found a clear mate, reduce remaining time
        if (abs(score) > MATE_SCORE - 50) {
            // Clear mate found - STOP SEARCHING IMMEDIATELY
            
            break;  // Exit iterative deepening loop immediately
            
        } else if (score < -MATE_SCORE + 100) {
            // Getting mated - STOP SEARCHING IMMEDIATELY
            
            break;  // Exit iterative deepening loop immediately
            
        } else if (abs(score) <= 2 && depth >= 25) {
            // Only stop for truly drawn positions (±2 centipawns) at very deep search
            // This prevents premature termination in slightly unbalanced positions
            break;  // Exit iterative deepening loop immediately
            
        }
        
        // Calculate elapsed time
        int time_cs = get_elapsed_time(&time_mgr);
        if (time_cs == 0) time_cs = 1; // Avoid zero time
        
        // Debug: show both counters
        // printf("DEBUG: depth=%d nodes_searched=%d info.nodes=%d\n", depth, nodes_searched, info.nodes);
        
        // Output thinking information in proper Winboard format (with cumulative nodes)
        printf("%d %d %d %d", depth, score, time_cs, info.nodes);
        
        // Add PV moves if available
        if (pv_length[0] > 0) {
            for (int i = 0; i < pv_length[0] && i < 20; i++) {
                printf(" ");
                print_move(pv_table[0][i]);
            }
        }
        printf("\n");
        fflush(stdout);
        
        // Stop if search was interrupted or we're out of time
        if (info.stop_search || should_stop_search(&time_mgr, depth + 1)) {
            break;
        }
    }
    
    // CRITICAL FIX: If search timed out before finding any move, return first legal move
    // This prevents the fallback mechanism in main.c from being triggered
    if (best_move == 0 && legal_count > 0) {
        return only_legal_move;  // Return first legal move instead of 0
    }
    
    return best_move;
}

// Version with game history for draw detection
Move find_best_move_with_history(Position* pos, int time_left, int opponent_time, 
                                 int increment, int moves_to_go, GameHistory* history) {
    SearchInfo info = {0};
    TimeManager time_mgr;
    Move best_move = 0;
    Move last_completed_move = 0;  // BUG FIX: Store move from last completed iteration
    int last_completed_depth = 0;
    
    // Quick move detection - if only one legal move, play it immediately
    MoveList legal_moves;
    generate_moves(pos, &legal_moves);
    Move only_legal_move = 0;
    int legal_count = 0;
    
    for (int i = 0; i < legal_moves.count; i++) {
        Position test_pos = *pos;
        if (make_move(&test_pos, legal_moves.moves[i].move) && 
            !in_check(&test_pos, 1 - test_pos.side_to_move)) {
            only_legal_move = legal_moves.moves[i].move;
            legal_count++;
            if (legal_count > 1) break;  // More than one legal move, continue normal search
        }
    }
    
    // If only one legal move, return it immediately
    if (legal_count == 1) {
        // Initialize fallback variables
        last_completed_move = only_legal_move;
        last_completed_depth = 1;
        
        return only_legal_move;
    }
    
    // Early draw detection - if position is drawn, return any legal move quickly
    if (is_insufficient_material(pos) || is_draw_by_50_move_rule(pos) || is_stalemate(pos)) {
        return only_legal_move;  // Return first legal move in drawn position
    }
    
    // Initialize fallback with first legal move
    last_completed_move = only_legal_move;
    last_completed_depth = 0;
    
    // CRASH PREVENTION: Ensure minimum time to prevent crashes in time management
    if (time_left < 50) time_left = 50;  // Minimum 50 centiseconds (0.5 seconds)
    if (opponent_time < 50) opponent_time = 50;
    
    // Initialize time management
    init_time_manager(&time_mgr, time_left, opponent_time, increment, moves_to_go, pos);
    info.time_mgr = &time_mgr;
    info.history = history; // Set game history for draw detection
    
    // Increment TT age for this search
    tt_age++;
    
    // Clear killer moves and history tables for new search
    clear_killer_moves();
    clear_history_tables();
    
    // Iterative deepening with aspiration windows
    Score prev_score = 0;
    // Initialize node counters (will accumulate across depths)
    info.nodes = 0;
    nodes_searched = 0;
    
    for (int depth = 1; depth <= MAX_DEPTH; depth++) {
        info.depth = depth;
        info.ply = 0;
        info.stop_search = false;
        // Do NOT reset node counters - let them accumulate
        
        // Clear PV table
        for (int i = 0; i < MAX_DEPTH; i++) {
            pv_length[i] = 0;
        }
        
        // Check if we should stop before starting this depth
        if (should_stop_search(&time_mgr, depth)) {
            break;
        }
        
        Score score;
        
        // Aspiration windows for depth >= 4
        if (depth >= 4 && abs(prev_score) < MATE_SCORE - 100) {
            int window = 25;  // Start with narrow window (0.25 pawns)
            Score alpha = prev_score - window;
            Score beta = prev_score + window;
            
            // Try narrow window first
            score = search(pos, &info, depth, alpha, beta);
            
            // If we fail, immediately use full window (no multiple researches)
            if (score <= alpha || score >= beta) {
                score = search(pos, &info, depth, -MATE_SCORE, MATE_SCORE);
            }
        } else {
            // Full window for shallow depths or extreme scores
            score = search(pos, &info, depth, -MATE_SCORE, MATE_SCORE);
        }
        
        prev_score = score;
        
        // Get best move from PV with validation (BUG FIX for Qb8 issue)
        Move current_pv_move = 0;
        if (pv_length[0] > 0) {
            current_pv_move = pv_table[0][0];
        }
        
        if (current_pv_move != 0) {
            // Validate the PV move is legal
            MoveList legal_moves_check;
            generate_moves(pos, &legal_moves_check);
            
            bool pv_is_legal = false;
            for (int i = 0; i < legal_moves_check.count; i++) {
                Position test_pos = *pos;
                if (legal_moves_check.moves[i].move == current_pv_move &&
                    make_move(&test_pos, current_pv_move) && 
                    !in_check(&test_pos, 1 - test_pos.side_to_move)) {
                    pv_is_legal = true;
                    break;
                }
            }
            
            if (pv_is_legal) {
                // PV move is legal - this iteration completed successfully
                best_move = current_pv_move;
                last_completed_move = current_pv_move;
                last_completed_depth = depth;
            } else {
                // PV CORRUPTION DETECTED - use last completed iteration's move
                printf("# DEBUG: PV corruption at depth %d, using depth %d move\n", 
                       depth, last_completed_depth);
                fflush(stdout);
                best_move = last_completed_move;
                break;  // Stop searching - corruption indicates serious problem
            }
        } else {
            // No PV - keep using last completed move
            best_move = last_completed_move;
        }
        
        // Dynamic time management - if we found a clear mate, reduce remaining time
        if (abs(score) > MATE_SCORE - 50) {
            // Clear mate found - STOP SEARCHING IMMEDIATELY
            
            break;  // Exit iterative deepening loop immediately
            
        } else if (score < -MATE_SCORE + 100) {
            // Getting mated - STOP SEARCHING IMMEDIATELY
            
            break;  // Exit iterative deepening loop immediately
            
        } else if (abs(score) <= 2 && depth >= 25) {
            // Only stop for truly drawn positions (±2 centipawns) at very deep search
            // This prevents premature termination in slightly unbalanced positions
            break;  // Exit iterative deepening loop immediately
            
        }
        
        // Calculate elapsed time
        int time_cs = get_elapsed_time(&time_mgr);
        if (time_cs == 0) time_cs = 1; // Avoid zero time
        
        // Output thinking information in proper Winboard format
        printf("%d %d %d %d", depth, score, time_cs, info.nodes);
        
        // Add PV moves if available
        if (pv_length[0] > 0) {
            for (int i = 0; i < pv_length[0] && i < 20; i++) {
                printf(" ");
                print_move(pv_table[0][i]);
            }
        }
        printf("\n");
        fflush(stdout);
        
        // Stop if search was interrupted or we're out of time
        if (info.stop_search || should_stop_search(&time_mgr, depth + 1)) {
            break;
        }
        
        // Note: Don't stop on score == 0, as that just means equal position, not draw
    }
    
    // CRITICAL FIX: If search timed out before finding any move, return first legal move
    // This prevents the fallback mechanism in main.c from being triggered
    if (best_move == 0 && legal_count > 0) {
        return only_legal_move;  // Return first legal move instead of 0
    }
    
    return best_move;
}

// Fixed depth search (ignores time limits)
Move find_best_move(Position* pos, int max_depth) {
    SearchInfo info = {0};
    GameHistory history = {0};
    Move best_move = 0;
    Move last_completed_move = 0;  // BUG FIX: Store move from last completed iteration
    int last_completed_depth = 0;
    
    // Quick move detection - if only one legal move, play it immediately
    MoveList legal_moves;
    generate_moves(pos, &legal_moves);
    Move only_legal_move = 0;
    int legal_count = 0;
    
    for (int i = 0; i < legal_moves.count; i++) {
        Position test_pos = *pos;
        if (make_move(&test_pos, legal_moves.moves[i].move) && 
            !in_check(&test_pos, 1 - test_pos.side_to_move)) {
            only_legal_move = legal_moves.moves[i].move;
            legal_count++;
            if (legal_count > 1) break;  // More than one legal move, continue normal search
        }
    }
    
    // If only one legal move, return it immediately
    if (legal_count == 1) {
        // Initialize fallback variables
        last_completed_move = only_legal_move;
        last_completed_depth = 1;
        
        return only_legal_move;
    }
    
    // Initialize fallback with first legal move
    last_completed_move = only_legal_move;
    last_completed_depth = 0;
    
    // Safety check - if no legal moves found, return 0
    if (legal_count == 0) {
        return 0;
    }
    
    // Initialize game history for draw detection
    info.history = &history;
    history.history_count = 0;
    history.game_ply = 0;
    
    // Add starting position to history
    history.position_history[0] = pos->hash_key;
    history.history_count = 1;
    
    // Increment TT age for this search
    tt_age++;
    
    // Clear killer moves and history tables for new search
    clear_killer_moves();
    clear_history_tables();
    
    // Iterative deepening to exact depth (no time limits)
    Score prev_score = 0;
    int total_nodes = 0;
    
    for (int depth = 1; depth <= max_depth; depth++) {
        info.depth = depth;
        info.ply = 0;
        int depth_nodes = 0;
        info.nodes = 0;
        info.stop_search = false;
        nodes_searched = 0;
        
        // Clear PV table
        for (int i = 0; i < MAX_DEPTH; i++) {
            pv_length[i] = 0;
        }
        
        Score score;
        
        // Aspiration windows for depth >= 4
        if (depth >= 4 && abs(prev_score) < MATE_SCORE - 100) {
            int window = 25;  // Start with narrow window (0.25 pawns)
            Score alpha = prev_score - window;
            Score beta = prev_score + window;
            
            // Try narrow window first
            score = search(pos, &info, depth, alpha, beta);
            
            // If we fail, immediately use full window
            if (score <= alpha || score >= beta) {
                score = search(pos, &info, depth, -MATE_SCORE, MATE_SCORE);
            }
        } else {
            // Full window for shallow depths or extreme scores
            score = search(pos, &info, depth, -MATE_SCORE, MATE_SCORE);
        }
        
        prev_score = score;
        
        // Get best move from PV with validation (BUG FIX for Qb8 issue)
        Move current_pv_move = 0;
        if (pv_length[0] > 0) {
            current_pv_move = pv_table[0][0];
        }
        
        if (current_pv_move != 0) {
            // Validate the PV move is legal
            MoveList legal_moves_check;
            generate_moves(pos, &legal_moves_check);
            
            bool pv_is_legal = false;
            for (int i = 0; i < legal_moves_check.count; i++) {
                Position test_pos = *pos;
                if (legal_moves_check.moves[i].move == current_pv_move &&
                    make_move(&test_pos, current_pv_move) && 
                    !in_check(&test_pos, 1 - test_pos.side_to_move)) {
                    pv_is_legal = true;
                    break;
                }
            }
            
            if (pv_is_legal) {
                // PV move is legal - this iteration completed successfully
                best_move = current_pv_move;
                last_completed_move = current_pv_move;
                last_completed_depth = depth;
            } else {
                // PV CORRUPTION DETECTED - use last completed iteration's move
                printf("# DEBUG: PV corruption at depth %d, using depth %d move\n", 
                       depth, last_completed_depth);
                fflush(stdout);
                best_move = last_completed_move;
                break;  // Stop searching - corruption indicates serious problem
            }
        } else {
            // No PV - keep using last completed move
            best_move = last_completed_move;
        }
        
        // Update total node count
        depth_nodes = info.nodes;
        total_nodes += depth_nodes;
        
        // Output thinking information in proper Winboard format
        printf("%d %d %d %d", depth, score, 1, total_nodes);
        
        // Add PV moves if available
        if (pv_length[0] > 0) {
            for (int i = 0; i < pv_length[0] && i < 20; i++) {
                printf(" ");
                print_move(pv_table[0][i]);
            }
        }
        printf("\n");
        fflush(stdout);
    }
    
    // CRITICAL FIX: If search failed to find any move, return first legal move
    // This prevents the fallback mechanism in main.c from being triggered
    if (best_move == 0 && legal_count > 0) {
        return only_legal_move;  // Return first legal move instead of 0
    }
    
    return best_move;
}

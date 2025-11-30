#ifndef CHESS_H
#define CHESS_H

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// Basic types
typedef uint64_t Bitboard;
typedef uint32_t Move;
typedef int16_t Score;

// Move encoding: from_sq | (to_sq << 6) | (piece << 12) | (captured << 16) | (promoted << 20) | (flags << 24)
#define MOVE_FROM(m) ((m) & 0x3F)
#define MOVE_TO(m) (((m) >> 6) & 0x3F)
#define MOVE_PIECE(m) (((m) >> 12) & 0xF)
#define MOVE_CAPTURED(m) (((m) >> 16) & 0xF)
#define MOVE_PROMOTED(m) (((m) >> 20) & 0xF)
#define MOVE_FLAGS(m) (((m) >> 24) & 0xFF)

#define MAKE_MOVE(from, to, piece, captured, promoted, flags) \
    ((from) | ((to) << 6) | ((piece) << 12) | ((captured) << 16) | ((promoted) << 20) | ((flags) << 24))

// Move flags
#define FLAG_CAPTURE 1
#define FLAG_CASTLE 2
#define FLAG_EP 4
#define FLAG_PROMOTION 8

// Constants
#define MAX_MOVES 256
#define MAX_PLY 128
#define MAX_DEPTH 35
#define MATE_SCORE 30000
#define DRAW_SCORE 0

// Contempt settings (following modern engine practices)
#define DEFAULT_CONTEMPT 20    // Default contempt in centipawns
#define ENDGAME_CONTEMPT 10    // Reduced contempt in endgames
#define MAX_CONTEMPT 50        // Maximum contempt value

// Transposition table constants
#define TT_SIZE (1 << 20)  // 1M entries (about 32MB)
#define TT_MASK (TT_SIZE - 1)

// TT entry types
#define TT_EXACT 0
#define TT_ALPHA 1
#define TT_BETA 2

// Piece definitions
enum {
    EMPTY = 0,
    PAWN = 1, KNIGHT = 2, BISHOP = 3, ROOK = 4, QUEEN = 5, KING = 6
};

enum {
    WHITE = 0, BLACK = 1
};

// Square definitions (0-63, a1=0, h8=63)
enum {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8
};

// Castling rights
#define CASTLE_WK 1
#define CASTLE_WQ 2
#define CASTLE_BK 4
#define CASTLE_BQ 8

// Position structure - designed for SMP safety
typedef struct {
    Bitboard pieces[2][7];  // [color][piece_type] - piece 0 is all pieces
    int side_to_move;
    int castle_rights;
    int en_passant_square;
    int halfmove_clock;
    int fullmove_number;
    uint64_t hash_key;      // Zobrist hash for transposition table
    
    // Incremental evaluation
    Score mg_score;         // Middlegame score (from white's perspective)
    Score eg_score;         // Endgame score (from white's perspective)
    int game_phase;         // Current game phase for tapered evaluation
    bool eval_dirty;        // Flag to indicate if evaluation needs recalculation
} Position;

// Move structure
typedef struct {
    Move move;
    Score score;
} ScoredMove;

// Move list
typedef struct {
    ScoredMove moves[MAX_MOVES];
    int count;
} MoveList;

// Transposition table entry
typedef struct {
    uint64_t hash_key;     // Position hash key
    Move best_move;        // Best move found
    Score score;           // Position score
    int depth;             // Search depth
    int age;               // Search age (for replacement)
    uint8_t type;          // Entry type (EXACT, ALPHA, BETA)
} TTEntry;

// Time management structure
typedef struct {
    int time_left;          // Time left in centiseconds
    int opponent_time;      // Opponent's time left
    int increment;          // Time increment per move
    int moves_to_go;        // Moves until next time control (0 = sudden death)
    int allocated_time;     // Time allocated for this move
    int max_time;          // Maximum time for this move
    clock_t start_time;    // When search started
} TimeManager;

// Game history for repetition detection
typedef struct {
    uint64_t position_history[MAX_PLY * 2];  // Store hash keys
    int history_count;
    int game_ply;  // Current game ply (for 50-move rule)
} GameHistory;

// Search info - thread-local data
typedef struct {
    int depth;
    int ply;
    int nodes;
    Move pv[MAX_PLY];
    int pv_length;
    bool stop_search;
    TimeManager* time_mgr;  // Time management
    GameHistory* history;   // Game history for draw detection
    uint64_t search_path[MAX_PLY];  // Position hashes in current search path
    Move last_move;         // Last move played by opponent (for counter moves)
    Move our_last_move;     // Our last move (for follow-up moves)
    Score eval_history[MAX_PLY];  // Evaluation history for improving detection
} SearchInfo;

// Function declarations
void init_engine(void);
void init_zobrist(void);
uint64_t compute_hash(const Position* pos);
void position_from_fen(Position* pos, const char* fen);

// Incremental hash update functions
void update_hash_add_piece(Position* pos, int piece, int square, int color);
void update_hash_remove_piece(Position* pos, int piece, int square, int color);
void update_hash_move_piece(Position* pos, int piece, int from_sq, int to_sq, int color);
void update_hash_castle_rights(Position* pos, int old_rights, int new_rights);
void update_hash_en_passant(Position* pos, int old_ep, int new_ep);
void update_hash_side_to_move(Position* pos);
void print_position(const Position* pos);

// Move generation
void generate_moves(const Position* pos, MoveList* moves);
void generate_captures(const Position* pos, MoveList* moves);
bool is_legal_move(const Position* pos, Move move);
bool make_move(Position* pos, Move move);
void unmake_move(Position* pos, Move move);
bool in_check(const Position* pos, int color);
bool is_square_attacked(const Position* pos, int square, int by_color);

// Attack generation functions
void init_attack_tables(void);
void init_magic_bitboards(void);
Bitboard get_bishop_attacks(int square, Bitboard occupied);
Bitboard get_rook_attacks(int square, Bitboard occupied);
Bitboard get_knight_attacks(int square);
Bitboard get_king_attacks(int square);

// Move parsing
Move parse_move(const Position* pos, const char* move_str);
void print_move(Move move);

// Draw detection
bool is_draw_by_repetition(const GameHistory* history, uint64_t current_hash);
bool is_draw_by_repetition_search(const GameHistory* history, uint64_t current_hash);
bool is_draw_by_50_move_rule(const Position* pos);
bool is_stalemate(const Position* pos);
bool is_insufficient_material(const Position* pos);
void add_position_to_history(GameHistory* history, uint64_t hash);

// Transposition table
void init_tt(void);
void clear_tt(void);
bool probe_tt(uint64_t hash_key, int depth, Score alpha, Score beta, Score* score, Move* best_move);
void store_tt(uint64_t hash_key, Move best_move, Score score, int depth, int type, int age);

// Search
Score search(Position* pos, SearchInfo* info, int depth, Score alpha, Score beta);
Score quiescence(Position* pos, SearchInfo* info, Score alpha, Score beta);
Move find_best_move(Position* pos, int depth);
Move find_best_move_timed(Position* pos, int time_left, int opponent_time, 
                         int increment, int moves_to_go);
Move find_best_move_with_history(Position* pos, int time_left, int opponent_time, 
                                int increment, int moves_to_go, GameHistory* history);

// History heuristic control
void set_history_enabled(bool enabled);
bool get_history_enabled(void);
int get_history_score_debug(int color, int from, int to);
void set_history_score_debug(int color, int from, int to, int score);
int get_history_score(int color, Move move);
void update_history_score(int color, Move move, int depth, bool good_move);

// Evaluation
Score evaluate(const Position* pos);
void init_evaluation(Position* pos);
void update_eval_add_piece(Position* pos, int piece, int square, int color);
void update_eval_remove_piece(Position* pos, int piece, int square, int color);
void update_eval_move_piece(Position* pos, int piece, int from_sq, int to_sq, int color);

// Utility
uint64_t perft(Position* pos, int depth);
void print_move(Move move);

#endif
#include "compat.h"
#include "chess.h"

// Zobrist hash keys (simplified - in real engine these would be random)
static uint64_t piece_keys[2][7][64];
static uint64_t castle_keys[16];
static uint64_t ep_keys[64];
static uint64_t side_key;

void init_zobrist() {
    // Initialize with simple values for now
    for (int color = 0; color < 2; color++) {
        for (int piece = 0; piece < 7; piece++) {
            for (int sq = 0; sq < 64; sq++) {
                piece_keys[color][piece][sq] = (uint64_t)rand() * rand();
            }
        }
    }
    
    for (int i = 0; i < 16; i++) {
        castle_keys[i] = (uint64_t)rand() * rand();
    }
    
    for (int i = 0; i < 64; i++) {
        ep_keys[i] = (uint64_t)rand() * rand();
    }
    
    side_key = (uint64_t)rand() * rand();
}

uint64_t compute_hash(const Position* pos) {
    uint64_t hash = 0;
    
    for (int color = 0; color < 2; color++) {
        for (int piece = PAWN; piece <= KING; piece++) {
            Bitboard bb = pos->pieces[color][piece];
            while (bb) {
                int sq = builtin_ctzll(bb);
                hash ^= piece_keys[color][piece][sq];
                bb &= bb - 1;
            }
        }
    }
    
    hash ^= castle_keys[pos->castle_rights];
    
    if (pos->en_passant_square != -1) {
        hash ^= ep_keys[pos->en_passant_square];
    }
    
    if (pos->side_to_move == BLACK) {
        hash ^= side_key;
    }
    
    return hash;
}

// Incremental hash update functions
void update_hash_add_piece(Position* pos, int piece, int square, int color) {
    pos->hash_key ^= piece_keys[color][piece][square];
}

void update_hash_remove_piece(Position* pos, int piece, int square, int color) {
    pos->hash_key ^= piece_keys[color][piece][square];
}

void update_hash_move_piece(Position* pos, int piece, int from_sq, int to_sq, int color) {
    pos->hash_key ^= piece_keys[color][piece][from_sq];
    pos->hash_key ^= piece_keys[color][piece][to_sq];
}

void update_hash_castle_rights(Position* pos, int old_rights, int new_rights) {
    pos->hash_key ^= castle_keys[old_rights];
    pos->hash_key ^= castle_keys[new_rights];
}

void update_hash_en_passant(Position* pos, int old_ep, int new_ep) {
    if (old_ep != -1) {
        pos->hash_key ^= ep_keys[old_ep];
    }
    if (new_ep != -1) {
        pos->hash_key ^= ep_keys[new_ep];
    }
}

void update_hash_side_to_move(Position* pos) {
    pos->hash_key ^= side_key;
}

void position_from_fen(Position* pos, const char* fen) {
    // Clear position
    memset(pos, 0, sizeof(Position));
    pos->en_passant_square = -1;
    
    const char* p = fen;
    int rank = 7, file = 0;
    
    // Parse piece placement
    while (*p && *p != ' ') {
        if (*p == '/') {
            rank--;
            file = 0;
        } else if (*p >= '1' && *p <= '8') {
            file += *p - '0';
        } else {
            int piece = 0, color = 0;
            
            switch (*p) {
                case 'P': piece = PAWN; color = WHITE; break;
                case 'N': piece = KNIGHT; color = WHITE; break;
                case 'B': piece = BISHOP; color = WHITE; break;
                case 'R': piece = ROOK; color = WHITE; break;
                case 'Q': piece = QUEEN; color = WHITE; break;
                case 'K': piece = KING; color = WHITE; break;
                case 'p': piece = PAWN; color = BLACK; break;
                case 'n': piece = KNIGHT; color = BLACK; break;
                case 'b': piece = BISHOP; color = BLACK; break;
                case 'r': piece = ROOK; color = BLACK; break;
                case 'q': piece = QUEEN; color = BLACK; break;
                case 'k': piece = KING; color = BLACK; break;
            }
            
            if (piece) {
                int sq = rank * 8 + file;
                pos->pieces[color][piece] |= 1ULL << sq;
                pos->pieces[color][0] |= 1ULL << sq;  // All pieces
            }
            file++;
        }
        p++;
    }
    
    // Skip space
    if (*p == ' ') p++;
    
    // Parse side to move
    pos->side_to_move = (*p == 'w') ? WHITE : BLACK;
    p += 2;
    
    // Parse castling rights
    while (*p && *p != ' ') {
        switch (*p) {
            case 'K': pos->castle_rights |= CASTLE_WK; break;
            case 'Q': pos->castle_rights |= CASTLE_WQ; break;
            case 'k': pos->castle_rights |= CASTLE_BK; break;
            case 'q': pos->castle_rights |= CASTLE_BQ; break;
        }
        p++;
    }
    
    // Skip space
    if (*p == ' ') p++;
    
    // Parse en passant
    if (*p != '-') {
        int file = p[0] - 'a';
        int rank = p[1] - '1';
        pos->en_passant_square = rank * 8 + file;
        p += 2;
    } else {
        p++;
    }
    
    // Parse halfmove and fullmove (simplified)
    if (*p == ' ') {
        pos->halfmove_clock = atoi(++p);
        while (*p && *p != ' ') p++;
        if (*p == ' ') {
            pos->fullmove_number = atoi(++p);
        }
    }
    
    pos->hash_key = compute_hash(pos);
    
    // Initialize incremental evaluation
    init_evaluation(pos);
}

void print_position(const Position* pos) {
    printf("\n  a b c d e f g h\n");
    
    for (int rank = 7; rank >= 0; rank--) {
        printf("%d ", rank + 1);
        
        for (int file = 0; file < 8; file++) {
            int sq = rank * 8 + file;
            char piece_char = '.';
            
            for (int color = 0; color < 2; color++) {
                for (int piece = PAWN; piece <= KING; piece++) {
                    if (pos->pieces[color][piece] & (1ULL << sq)) {
                        const char pieces[] = ".pnbrqk";
                        piece_char = pieces[piece];
                        if (color == WHITE) {
                            piece_char = piece_char - 'a' + 'A';
                        }
                        break;
                    }
                }
            }
            
            printf("%c ", piece_char);
        }
        printf("%d\n", rank + 1);
    }
    
    printf("  a b c d e f g h\n");
    printf("Side to move: %s\n", pos->side_to_move == WHITE ? "White" : "Black");
    printf("Castle rights: %s%s%s%s\n",
           (pos->castle_rights & CASTLE_WK) ? "K" : "",
           (pos->castle_rights & CASTLE_WQ) ? "Q" : "",
           (pos->castle_rights & CASTLE_BK) ? "k" : "",
           (pos->castle_rights & CASTLE_BQ) ? "q" : "");
    printf("En passant: %s\n", pos->en_passant_square == -1 ? "-" : "set");
    printf("Hash: %lx\n", (unsigned long)pos->hash_key);
}

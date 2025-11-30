// Rexigon chess
// Developed by Tom King and Kiro AI
// PST values from Pesto engine
// 
// V1.0 Initial Release 30/11/2025
//

#include "compat.h"
#include "chess.h"
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <unistd.h>
#endif

// Global position
Position g_position;

// Global search depth (0 = use time-based search)
int g_search_depth = 0;

// Time control state
int g_time_left = 30000;      // 5 minutes in centiseconds
int g_opponent_time = 30000;  // 5 minutes in centiseconds  
int g_increment = 0;          // No increment by default
int g_moves_to_go = 0;        // Sudden death by default

// Game history for draw detection
GameHistory g_game_history = {0};

// Game state
bool g_force_mode = true;  // Start in force mode
bool g_computer_plays_white = false;  // By default, computer plays black

// Function declarations
void handle_user_move(const char* move_str);

// Winboard protocol handler
void handle_winboard_command(char* line) {
    char* token = strtok(line, " \t\n");
    if (!token) return;
    
    if (strcmp(token, "xboard") == 0) {
        // Enter xboard mode - no output required
        return;
    }
    else if (strcmp(token, "protover") == 0) {
        // Protocol version 2 feature negotiation
        printf("feature ping=1 setboard=1 playother=1 san=0 usermove=0 time=1 draw=0 sigint=0 sigterm=0 reuse=1 analyze=0 myname=\"Rexigon 1.0\" variants=\"normal\" colors=0 ics=0 name=0 pause=0 nps=0 debug=0 memory=0 smp=0 egt=\"\" done=1\n");
        fflush(stdout);
    }
    else if (strcmp(token, "new") == 0) {
        // Start new game - reset to initial position
        position_from_fen(&g_position, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        g_force_mode = false;  // Start ready to play (not in force mode)
        g_computer_plays_white = false;  // Computer plays black by default
        
        // Reset game history
        memset(&g_game_history, 0, sizeof(g_game_history));
        g_game_history.position_history[0] = g_position.hash_key;
        g_game_history.history_count = 1;
        g_game_history.game_ply = 0;
    }
    else if (strcmp(token, "setboard") == 0) {
        char* fen = strtok(NULL, "\n");
        if (fen) {
            position_from_fen(&g_position, fen);
        }
    }
    else if (strcmp(token, "go") == 0) {
        // Leave force mode and make a move for the current side
        g_force_mode = false;
        
        // Set computer to play the current side to move
        g_computer_plays_white = (g_position.side_to_move == WHITE);
        
        // Check if we're in checkmate or stalemate
        MoveList legal_moves;
        generate_moves(&g_position, &legal_moves);
        
        // Filter out illegal moves (those that leave king in check)
        int legal_count = 0;
        for (int i = 0; i < legal_moves.count; i++) {
            Position test_pos = g_position;
            if (make_move(&test_pos, legal_moves.moves[i].move) && 
                !in_check(&test_pos, 1 - test_pos.side_to_move)) {
                legal_count++;
            }
        }
        
        if (legal_count == 0) {
            // Don't automatically claim results - let tournament manager handle it
            return;
        } else {
            Move best_move;
            if (g_search_depth > 0) {
                // Use fixed depth search if sd was set
                best_move = find_best_move(&g_position, g_search_depth);
            } else {
                // Use time-based search
                best_move = find_best_move_with_history(&g_position, g_time_left, g_opponent_time, 
                                                            g_increment, g_moves_to_go, &g_game_history);
            }
            if (best_move) {
                // CRITICAL: Validate move is legal before playing it
                Position test_pos = g_position;
                if (make_move(&test_pos, best_move) && !in_check(&test_pos, 1 - test_pos.side_to_move)) {
                    // Move is legal - play it
                    make_move(&g_position, best_move);
                    
                    // Decrement moves to go for time control
                    if (g_moves_to_go > 0) {
                        g_moves_to_go--;
                    }
                    
                    // Add position to game history
                    if (g_game_history.history_count < MAX_PLY * 2 - 1) {
                        g_game_history.position_history[g_game_history.history_count] = g_position.hash_key;
                        g_game_history.history_count++;
                        g_game_history.game_ply++;
                    }
                    
                    printf("move ");
                    print_move(best_move);
                    printf("\n");
                } else {
                    // Move is illegal - this should never happen, but handle gracefully
                    printf("# Error: Search returned illegal move, falling back to first legal move\n");
                    
                    // Find first legal move as emergency fallback
                    MoveList legal_moves;
                    generate_moves(&g_position, &legal_moves);
                    
                    for (int i = 0; i < legal_moves.count; i++) {
                        Position test_pos2 = g_position;
                        if (make_move(&test_pos2, legal_moves.moves[i].move) && 
                            !in_check(&test_pos2, 1 - test_pos2.side_to_move)) {
                            
                            make_move(&g_position, legal_moves.moves[i].move);
                            
                            // Decrement moves to go for time control
                            if (g_moves_to_go > 0) {
                                g_moves_to_go--;
                            }
                            
                            // Add position to game history
                            if (g_game_history.history_count < MAX_PLY * 2 - 1) {
                                g_game_history.position_history[g_game_history.history_count] = g_position.hash_key;
                                g_game_history.history_count++;
                                g_game_history.game_ply++;
                            }
                            
                            printf("move ");
                            print_move(legal_moves.moves[i].move);
                            printf("\n");
                            break;
                        }
                    }
                }
            } else {
                // Don't automatically claim results - let tournament manager handle it
                return;
            }
        }
        fflush(stdout);
    }
    else if (strcmp(token, "level") == 0) {
        // Parse time control: level moves time increment
        char* moves_str = strtok(NULL, " ");
        char* time_str = strtok(NULL, " ");
        char* inc_str = strtok(NULL, " ");
        
        if (moves_str) g_moves_to_go = atoi(moves_str);
        if (time_str) {
            // Time can be in minutes or minutes:seconds format
            int minutes = atoi(time_str);
            char* colon = strchr(time_str, ':');
            if (colon) {
                int seconds = atoi(colon + 1);
                g_time_left = (minutes * 60 + seconds) * 100; // Convert to centiseconds
            } else {
                g_time_left = minutes * 60 * 100; // Convert minutes to centiseconds
            }
            g_opponent_time = g_time_left; // Assume same time for opponent
        }
        if (inc_str) g_increment = (int)(atof(inc_str) * 100); // Convert to centiseconds
    }
    else if (strcmp(token, "time") == 0) {
        // Time remaining in centiseconds
        char* time_str = strtok(NULL, " ");
        if (time_str) {
            g_time_left = atoi(time_str);
        }
    }
    else if (strcmp(token, "otim") == 0) {
        // Opponent's time remaining in centiseconds
        char* time_str = strtok(NULL, " ");
        if (time_str) {
            g_opponent_time = atoi(time_str);
        }
    }
    else if (strcmp(token, "quit") == 0) {
        exit(0);
    }
    else if (strcmp(token, "force") == 0) {
        // Enter force mode - engine doesn't move automatically
        g_force_mode = true;
    }
    else if (strcmp(token, "undo") == 0) {
        // Undo last move - not implemented, just acknowledge
        return;
    }
    else if (strcmp(token, "remove") == 0) {
        // Remove last two moves - not implemented, just acknowledge  
        return;
    }
    else if (strcmp(token, "result") == 0) {
        // Game result - just acknowledge
        return;
    }
    else if (strcmp(token, "draw") == 0) {
        // Draw offer - accept if position is drawn, otherwise decline
        if (is_draw_by_50_move_rule(&g_position) || 
            is_insufficient_material(&g_position) ||
            is_draw_by_repetition(&g_game_history, g_position.hash_key)) {
            printf("offer draw\n");
        }
        // Otherwise just ignore (implicit decline)
        fflush(stdout);
        return;
    }
    else if (strcmp(token, "accepted") == 0) {
        // Draw accepted - acknowledge
        return;
    }
    else if (strcmp(token, "rejected") == 0) {
        // Draw rejected - acknowledge
        return;
    }
    else if (strcmp(token, "white") == 0) {
        // WHITE will move next - set side to move to white
        g_position.side_to_move = WHITE;
        g_computer_plays_white = false;  // Computer plays black
        // Don't change force mode - stay ready to play
    }
    else if (strcmp(token, "black") == 0) {
        // BLACK will move next - set side to move to black  
        g_position.side_to_move = BLACK;
        g_computer_plays_white = true;   // Computer plays white
        // Don't change force mode - stay ready to play
    }
    else if (strcmp(token, "playother") == 0) {
        // Switch which side the computer plays and leave force mode
        g_computer_plays_white = !g_computer_plays_white;
        g_force_mode = false;
        
        // If it's now the computer's turn, make a move
        if ((g_computer_plays_white && g_position.side_to_move == WHITE) ||
            (!g_computer_plays_white && g_position.side_to_move == BLACK)) {
            
            MoveList legal_moves;
            generate_moves(&g_position, &legal_moves);
            
            int legal_count = 0;
            for (int i = 0; i < legal_moves.count; i++) {
                Position test_pos = g_position;
                if (make_move(&test_pos, legal_moves.moves[i].move) && 
                    !in_check(&test_pos, 1 - test_pos.side_to_move)) {
                    legal_count++;
                }
            }
            
            if (legal_count == 0) {
                // Don't automatically claim results - let tournament manager handle it
                return;
            } else {
                Move best_move;
                if (g_search_depth > 0) {
                    // Use fixed depth search if sd was set
                    best_move = find_best_move(&g_position, g_search_depth);
                } else {
                    // Use time-based search
                    best_move = find_best_move_with_history(&g_position, g_time_left, g_opponent_time, 
                                                                g_increment, g_moves_to_go, &g_game_history);
                }
                if (best_move) {
                    make_move(&g_position, best_move);
                    
                    // Decrement moves to go for time control
                    if (g_moves_to_go > 0) {
                        g_moves_to_go--;
                    }
                    
                    // Add position to game history
                    if (g_game_history.history_count < MAX_PLY * 2 - 1) {
                        g_game_history.position_history[g_game_history.history_count] = g_position.hash_key;
                        g_game_history.history_count++;
                        g_game_history.game_ply++;
                    }
                    
                    printf("move ");
                    print_move(best_move);
                    printf("\n");
                } else {
                    // Don't automatically claim results - let tournament manager handle it
                    return;
                }
            }
            fflush(stdout);
        }
    }
    else if (strcmp(token, "ping") == 0) {
        // Ping-pong for synchronization
        char* ping_num = strtok(NULL, " ");
        if (ping_num) {
            printf("pong %s\n", ping_num);
        } else {
            printf("pong\n");
        }
        fflush(stdout);
    }
    else if (strcmp(token, "hard") == 0) {
        // Turn on pondering (not implemented)
    }
    else if (strcmp(token, "easy") == 0) {
        // Turn off pondering
    }
    else if (strcmp(token, "post") == 0) {
        // Turn on thinking output (already on)
    }
    else if (strcmp(token, "nopost") == 0) {
        // Turn off thinking output (ignore for now)
    }
    else if (strcmp(token, "random") == 0) {
        // Turn on random play (ignore)
    }
    else if (strcmp(token, "computer") == 0) {
        // Opponent is a computer (ignore)
    }
    else if (strcmp(token, "st") == 0) {
        // Set time per move
        char* time_str = strtok(NULL, " ");
        if (time_str) {
            int seconds = atoi(time_str);
            if (seconds < 5) g_search_depth = 3;
            else if (seconds < 15) g_search_depth = 4;
            else if (seconds < 30) g_search_depth = 5;
            else g_search_depth = 6;
        }
    }
    else if (strcmp(token, "sd") == 0) {
        // Set search depth
        char* depth_str = strtok(NULL, " ");
        if (depth_str) {
            int depth = atoi(depth_str);
            if (depth > 0 && depth <= MAX_DEPTH) {
                g_search_depth = depth;
            }
        }
    }
    else if (strcmp(token, "usermove") == 0) {
        // User move with explicit usermove command
        char* move_str = strtok(NULL, " \t\n");
        if (move_str) {
            handle_user_move(move_str);
        }
    }
    else if (strlen(token) >= 4 && token[0] >= 'a' && token[0] <= 'h' && 
             token[1] >= '1' && token[1] <= '8') {
        // User move without usermove command
        handle_user_move(token);
    }
}

// Helper function to handle user moves
void handle_user_move(const char* move_str) {
    // Parse and make the move
    Move move = parse_move(&g_position, move_str);
    if (move) {
        // Test if the move is legal by making it on a copy
        Position test_pos = g_position;
        if (make_move(&test_pos, move) && !in_check(&test_pos, 1 - test_pos.side_to_move)) {
            // Move is legal, make it on the real position
            make_move(&g_position, move);
            
            // Decrement moves to go for time control
            if (g_moves_to_go > 0) {
                g_moves_to_go--;
            }
            
            // Add position to game history
            if (g_game_history.history_count < MAX_PLY * 2 - 1) {
                g_game_history.position_history[g_game_history.history_count] = g_position.hash_key;
                g_game_history.history_count++;
                g_game_history.game_ply++;
            }
            
            // If not in force mode and it's now the computer's turn, engine should respond
            if (!g_force_mode && 
                ((g_computer_plays_white && g_position.side_to_move == WHITE) ||
                 (!g_computer_plays_white && g_position.side_to_move == BLACK))) {
                // Check if we're in checkmate or stalemate
                MoveList legal_moves;
                generate_moves(&g_position, &legal_moves);
                
                // Filter out illegal moves (those that leave king in check)
                int legal_count = 0;
                for (int i = 0; i < legal_moves.count; i++) {
                    Position test_pos2 = g_position;
                    if (make_move(&test_pos2, legal_moves.moves[i].move) && 
                        !in_check(&test_pos2, 1 - test_pos2.side_to_move)) {
                        legal_count++;
                    }
                }
                
                if (legal_count == 0) {
                    // Don't automatically claim results - let tournament manager handle it
                    return;
                } else {
                    Move best_move;
                    if (g_search_depth > 0) {
                        // Use fixed depth search if sd was set
                        best_move = find_best_move(&g_position, g_search_depth);
                    } else {
                        // Use time-based search
                        best_move = find_best_move_with_history(&g_position, g_time_left, g_opponent_time, 
                                                                    g_increment, g_moves_to_go, &g_game_history);
                    }
                    if (best_move) {
                        // Make the engine's move on the position
                        make_move(&g_position, best_move);
                        
                        // Decrement moves to go for time control
                        if (g_moves_to_go > 0) {
                            g_moves_to_go--;
                        }
                        
                        // Add position to game history
                        if (g_game_history.history_count < MAX_PLY * 2 - 1) {
                            g_game_history.position_history[g_game_history.history_count] = g_position.hash_key;
                            g_game_history.history_count++;
                            g_game_history.game_ply++;
                        }
                        
                        printf("move ");
                        print_move(best_move);
                        printf("\n");
                    } else {
                        // Don't automatically claim results - let tournament manager handle it
                        return;
                    }
                }
                fflush(stdout);
            }
        } else {
            printf("Illegal move: %s (move parsed but failed legality test)\n", move_str);
            fflush(stdout);
        }
    } else {
        printf("Illegal move: %s (move parsing failed)\n", move_str);
        fflush(stdout);
    }
}

int main(void) {
    char line[1024];
    
    init_engine();
    position_from_fen(&g_position, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    
    // Start in force mode as per Winboard protocol
    g_force_mode = true;
    g_computer_plays_white = false;
    
    // Main command loop
    while (fgets(line, sizeof(line), stdin)) {
        handle_winboard_command(line);
    }
    
    return 0;
}
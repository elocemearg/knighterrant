#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <getopt.h>
#include <errno.h>
#include <error.h>
#include <pthread.h>

#include "knighterrant.h"

const SQUARE INVALID_SQUARE = -1;

/* Precomputed: for each square, the IDs of the two lines that square is on. */
LINE square_to_lines[NUM_SQUARES][2];

/* Precomputed: for each line ID, the squares that lie on it. */
SQUARE line_to_squares[NUM_LINES][LINE_LENGTH];

/* Precomputed: for each square, the <= 8 possible next squares the knight can
 * move to.
 * If there are fewer than 8, excess cells are filled with INVALID_SQUARE. */
SQUARE moves[NUM_SQUARES][8];

/* Precomputed: knight_adjacent_squares[x][y] is true if and only if x and y
 * are a knight's move apart. */
bool knight_adjacent_squares[NUM_SQUARES][NUM_SQUARES];

#if ENFORCE_SUM_RULE
/* sum_minimums:
 *
 * The array index is the number of unfilled spaces in a horizontal
 * or vertical line from one edge to the middle. The sum of the numbers in
 * such a line, in a completed tour, must equal LINE_SUM.
 *
 * The value is the minimum that a partial sum can possibly be in a
 * partially-completed tour that might complete. For example,
 * sum_minimums[0] is LINE_SUM, because if there are no unfilled squares in a
 * line, its sum must be LINE_SUM.
 */

const short sum_minimums[] = {
    /* 0 unfilled squares: sum must be exactly LINE_SUM. */
    LINE_SUM,

    /* Only one space left on the line.
     * The sum so far on the line must be at least LINE_SUM - NUM_SQUARES.
     * If it's less, it would require a step number greater than the maximum
     * step number to reach LINE_SUM.*/
    LINE_SUM - NUM_SQUARES,

    /* Two spaces left on the line. */
#if BOARD_DIM == 8 && RESTRICT_START_AND_END
    /* Special case for an 8x8 board where the start and end must be on
     * opposite sides of the board...
     *
     * If a 4x4 quadrant has a line with two numbers filled in, and we must
     * start on the left and finish on the right (RESTRICT_START_AND_END),
     * these two numbers must sum to at least 8.
     *
     * Proof:
     *
     * Every line of four within a quadrant must sum to 130 when the tour is
     * complete. The nature of the knight's move is that any line of four will
     * have two odd numbers and two even.
     *
     * The possible cases for a line with two numbers which sum to less than 8
     * are (1, 3), (1, 4), (2, 3), (1, 5), (2, 4), (1, 6), and (2, 5).
     * Cases like (3, 4) are not possible: consecutively-numbered squares must
     * be a knight's move apart so cannot be on the same line.
     *
     * If a line of four contains 1 and 3, then to sum to 130 the other two
     * would have to be 62 and 64, which is not possible because the 1 and 64
     * are enforced to be on opposite edges of the board. A symmetrical
     * argument applies if the line's two numbers are 62 and 64.
     *
     * If a line of four contains 1 and 4, then the other two must be 61 and 64
     * or 62 and 63, and the same argument applies: 1 must be on a different
     * half from 63 and 64.
     *
     * If a line of four contains 2 and 3 then the other two must be 61
     * and 64 or 62 and 63. But this is impossible - 2 and 63 must be a knight's
     * move from the left and right edges of the board respectively, so they
     * must be on different halves of the board and therefore not in the same
     * quadrant.
     *
     * If a line of four contains 1 and 5, then to sum to 130 the other two
     * must be 60 and 64, but as before, 1 and 64 can't be on the same line.
     *
     * If a line of four contains 2 and 4, then to sum to 130 the other two
     * must be 61 and 63, but as before, 2 and 63 can't be on the same line.
     *
     * If a line of four contains 1 and 6, the cases for the other two are:
     *     59 and 64 (impossible because 1 and 64 can't share a line)
     *     60 and 63 (impossible because 1 and 63 can't share a line)
     *     61 and 62 (impossible because consecutive numbers can't share a line)
     * 
     * Finally, if a line of four contains 2 and 5, the cases for the other
     * two numbers are the same as for 1 and 6, and a similar argument rules
     * them out: 2 can't share a line with 64 or 63, and consecutive numbers
     * can't share a line.
     *     
     * Therefore, any two numbers in a line of four must sum to at least 8,
     * and, by symmetry, at most 122. */
    8,
#else
    /* Non-8*8 board or unrestricted start and end: do not enforce a minimum
     * for sum_minimums[2]. */
    0,
#endif
    /* No minimum sum for any line with three or more empty squares */
    0, 0, 0, 0, 0
};

/* sum_maximums:
 * Same as sum_minimums, except this enforces a maximum limit on any partially
 * completed line with 0, 1 or 2 squares yet to be filled.
 */
const short sum_maximums[] = {
    /* Completed line: sum must be exactly LINE_SUM. */
    LINE_SUM,

    /* sum_maximums[1]: if there's one more square to fill in, the sum so far
     * depends on what the next step is, so no limit is enforced here. */
    32767,

    /* sum_maximums[2]: see argument above for the 8*8 case, no maximum for
     * a non-8*8 case. */
#if BOARD_DIM == 8 && RESTRICT_START_AND_END
    122,
#else
    32767,
#endif
    /* No maximum sum for any line with three or more empty squares */
    32767,
    32767,
    32767,
    32767,
    32767,
};
#endif

static void
precompute(void) {
    /* square_to_lines[square][0] and square_to_lines[square][1] must contain
     * the two IDs of the lines which this square is part of. */
    LINE line = 0;
    for (int qr = 0; qr < 2; qr++) {
        for (int qc = 0; qc < 2; qc++) {
            SQUARE quadrant_top_left = qc * (BOARD_DIM / 2) + qr * (NUM_SQUARES / 2);
            /* BOARD_DIM / 2 horizontal lines of BOARD_DIM / 2 squares each */
            for (int r = 0; r < BOARD_DIM / 2; r++) {
                for (int c = 0; c < BOARD_DIM / 2; c++) {
                    SQUARE sq = quadrant_top_left + r * BOARD_DIM + c;
                    square_to_lines[sq][0] = line;
                    line_to_squares[line][c] = sq;
                }
                line++;
            }
            /* BOARD_DIM / 2 vertical lines of BOARD_DIM / 2 squares each */
            for (int c = 0; c < BOARD_DIM / 2; c++) {
                for (int r = 0; r < BOARD_DIM / 2; r++) {
                    SQUARE sq = quadrant_top_left + r * BOARD_DIM + c;
                    square_to_lines[sq][1] = line;
                    line_to_squares[line][r] = sq;
                }
                line++;
            }
        }
    }

    /* moves[s][0..7] must contain all possible knight destination squares
     * from square s. If there are fewer than eight, pad with INVALID_SQUARE. */
    for (SQUARE square = 0; square < NUM_SQUARES; square++) {
        int r = square / BOARD_DIM;
        int c = square % BOARD_DIM;
        int m = 0;
        for (int diry = -1; diry <= 1; diry += 2) {
            for (int dirx = -1; dirx <= 1; dirx += 2) {
                for (int a = 0; a < 2; a++) {
                    int new_r = r, new_c = c;
                    if (a == 0) {
                        /* Two vertical, one horizontal */
                        new_r += 2 * diry;
                        new_c += dirx;
                    }
                    else {
                        /* Two horizontal, one vertical */
                        new_r += diry;
                        new_c += 2 * dirx;
                    }
                    if (new_r >= 0 && new_r < BOARD_DIM && new_c >= 0 && new_c < BOARD_DIM) {
                        SQUARE new_square = (SQUARE) (new_r * BOARD_DIM + new_c);
                        moves[square][m++] = new_square;
                        knight_adjacent_squares[square][new_square] = true;
                    }
                }
            }
        }

        /* Pad out the rest of moves[square] with INVALID_SQUARE */
        while (m < 8) {
            moves[square][m++] = INVALID_SQUARE;
        }
    }
}


/* Functions for adjusting the adjacent_unvisited and adjacent_unvisited_counts
 * arrays. */
static void
decrement_adjacent_unvisited(struct ke_pos *pos, SQUARE square) {
    if (!bb_test(pos->visited, square)) {
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]--;
        pos->adjacent_unvisited[square]--;
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]++;
    }
}

static void
increment_adjacent_unvisited(struct ke_pos *pos, SQUARE square) {
    if (!bb_test(pos->visited, square)) {
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]--;
        pos->adjacent_unvisited[square]++;
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]++;
    }
}

/* Unplays the last move. */
static void
unmake_step(struct ke_pos *pos) {
    STEP unplay_step;
    SQUARE square;

    if (pos->next_step <= 1) {
        return;
    }
    unplay_step = pos->next_step - 1;
    square = pos->step_to_square[unplay_step];
    bb_unset(&pos->visited, square);
    pos->line_sum[square_to_lines[square][0]] -= unplay_step;
    pos->line_sum[square_to_lines[square][1]] -= unplay_step;
    pos->line_count[square_to_lines[square][0]]--;
    pos->line_count[square_to_lines[square][1]]--;
    pos->square_to_step[square] = 0;
    pos->step_to_square[unplay_step] = INVALID_SQUARE;
    for (int i = 0; i < 8; i++) {
        SQUARE adj = moves[square][i];
        if (adj == INVALID_SQUARE)
            break;
        increment_adjacent_unvisited(pos, adj);
    }
    pos->adjacent_unvisited[square] *= -1;
    pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]++;
    pos->pos = pos->step_to_square[unplay_step - 1];
    pos->next_step--;
}


/* Plays step number "step" onto square number "square" and updates everything
 * in the ke_pos accordingly.
 * This function makes no attempt to check whether the move is legal.
 */
static void
make_step(struct ke_pos *pos, SQUARE square) {
    STEP step = pos->next_step;
    bb_set(&pos->visited, square);

    pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]--;
    pos->adjacent_unvisited[square] *= -1;

    /* Decrement the adjacent unvisited count for all nodes adjacent to this one */
    for (int i = 0; i < 8; i++) {
        SQUARE adj = moves[square][i];
        if (adj == INVALID_SQUARE)
            break;
        decrement_adjacent_unvisited(pos, adj);
    }
    pos->line_sum[square_to_lines[square][0]] += step;
    pos->line_sum[square_to_lines[square][1]] += step;
    pos->line_count[square_to_lines[square][0]]++;
    pos->line_count[square_to_lines[square][1]]++;
    pos->square_to_step[square] = step;
    pos->step_to_square[step] = square;
    pos->pos = square;
    pos->next_step++;
}

static void
ke_pos_init(struct ke_pos *pos, SQUARE initial_square) {
    memset(pos, 0, sizeof(*pos));
    for (SQUARE sq = 0; sq < NUM_SQUARES; sq++) {
        pos->adjacent_unvisited[sq] = 0;
        for (int i = 0; i < 8; i++) {
            if (moves[sq][i] == INVALID_SQUARE)
                break;
            else
                pos->adjacent_unvisited[sq]++;
        }
    }
    for (SQUARE sq = 0; sq < NUM_SQUARES; sq++) {
        int count = pos->adjacent_unvisited[sq];
        pos->adjacent_unvisited_counts[count]++;
    }

    for (STEP step = 0; step <= NUM_SQUARES; step++) {
#if LAST_SPACE_OPTIMISATION
        pos->step_to_square_required_by_sum[step] = INVALID_SQUARE;
#endif
        pos->step_to_square[step] = INVALID_SQUARE;
    }

    pos->next_step = 1;
    make_step(pos, initial_square);
}

#if !TOUR_TEST
static void
print_top_row_border() {
    printf(BOX_TOP_LEFT);
    for (int i = 0; i < BOARD_DIM; ++i) {
        if (i > 0) {
            printf(BOX_T);
        }
        printf(BOX_HORIZONTAL BOX_HORIZONTAL);
    }
    printf(BOX_TOP_RIGHT "\n");
}

static void
print_mid_row_border() {
    printf(BOX_LEFT_JUNC);
    for (int i = 0; i < BOARD_DIM; ++i) {
        if (i > 0)
            printf(BOX_CROSS);
        printf(BOX_HORIZONTAL BOX_HORIZONTAL);
    }
    printf(BOX_RIGHT_JUNC "\n");
}

static void
print_bottom_row_border() {
    printf(BOX_BOTTOM_LEFT);
    for (int i = 0; i < BOARD_DIM; ++i) {
        if (i > 0)
            printf(BOX_INVERTED_T);
        printf(BOX_HORIZONTAL BOX_HORIZONTAL);
    }
    printf(BOX_BOTTOM_RIGHT "\n");
}

static void
print_tour(const struct ke_pos *pos) {
    print_top_row_border();
    for (int r = 0; r < BOARD_DIM; r++) {
        if (r > 0) {
            print_mid_row_border();
        }
        for (int c = 0; c < BOARD_DIM; c++) {
            SQUARE sq = r * BOARD_DIM + c;
            if (!bb_test(pos->visited, sq)) {
                printf(BOX_VERTICAL "  ");
            }
            else {
                printf(BOX_VERTICAL "%2d", pos->square_to_step[sq]);
            }
        }
        printf(BOX_VERTICAL "\n");
    }
    print_bottom_row_border();
    printf("\n");
}
#endif

/* Progress output and result output */
pthread_mutex_t emit_mutex = PTHREAD_MUTEX_INITIALIZER;
long long num_tours_found = 0;
long long num_closed_tours_found = 0;
bool show_progress = true;

/* Called with job_buffer->mutex held */
static void
update_progress(struct job_buffer *job_buffer) {
    /* Number of jobs completed is the number of jobs dequeued minus the number
     * of threads still working. */
    int progress_pc;

    if (job_buffer->update_progress == NULL)
        return;

    progress_pc = ((job_buffer->jobs_dequeued - job_buffer->num_threads_running) * 100) / job_buffer->num_expected_jobs;
    if (progress_pc < 0)
        progress_pc = 0;
    MUTEX_LOCK(&emit_mutex);
    fprintf(stderr, " %10lld tours found   %3d threads running   %3d%%\r",
            num_tours_found, job_buffer->num_threads_running, progress_pc);
    MUTEX_UNLOCK(&emit_mutex);
}

static void
check_tour(const struct ke_pos *pos) {
    /* Check that every square has been visited */
#if BOARD_DIM == 8
    assert(pos->visited == (BOARDBITMAP) -1);
#else
    assert(pos->visited == (1ULL << NUM_SQUARES) - 1);
#endif

#if ENFORCE_SUM_RULE
    /* Check that every half-row and half-column sums to LINE_SUM */
    for (LINE line = 0; line < NUM_LINES; line++) {
        short sum = 0;
        for (int i = 0; i < LINE_LENGTH; i++) {
            sum += pos->square_to_step[line_to_squares[line][i]];
        }
        assert(sum == LINE_SUM);
    }
#endif

    /* Check that it's actually a knight's tour */
    for (STEP step = 1; step <= NUM_SQUARES; step++) {
        SQUARE current_square = pos->step_to_square[step];
        if (step > 1) {
            assert(knight_adjacent_squares[pos->step_to_square[step - 1]][current_square]);
        }
        assert(pos->square_to_step[current_square] == step);
    }
}

static void
emit_completed_tour(void *cookie, const struct ke_pos *completed_state) {
    MUTEX_LOCK(&emit_mutex);
    num_tours_found++;
    if (knight_adjacent_squares[completed_state->step_to_square[1]][completed_state->step_to_square[NUM_SQUARES]]) {
        /* The ending square is a knight's move from the starting square, so
         * this is also a closed tour */
        num_closed_tours_found++;
    }
#if !TOUR_TEST
    printf("\nFound tour #%lld...\n", num_tours_found);
    print_tour(completed_state);
    fflush(stdout);
#endif
    check_tour(completed_state);
    MUTEX_UNLOCK(&emit_mutex);
}

/* Search for completed tours from the given position until pos->next_step is
 * greater than max_steps. Return when we have emitted all completed tours
 * reachable from the given position.
 * When we find a completed tour, call complete_callback(cookie, position). */
void
tour(struct ke_pos *pos, int max_steps,
        void (*complete_callback)(void *cookie, const struct ke_pos *completed_state), void *cookie) {
    SQUARE current_pos = pos->pos;
    SQUARE *move_array;
    SQUARE singleton_move;
    int num_moves;
#if LAST_SPACE_OPTIMISATION
    SQUARE required_square;
#endif

    if (pos->next_step > max_steps) {
        /* We've already taken max_steps steps, so emit our tour using the
         * callback function. If this was called by the main thread it will
         * enqueue this state to a worker to search the rest of the tree from
         * this point. Otherwise, we are a worker thread and we've found a
         * complete knight's tour, so this will emit it. */
        complete_callback(cookie, pos);
        return;
    }

    /* If all end squares are full before we've made this step, give up. */
    if ((pos->visited & END_SQUARE_MASK) == END_SQUARE_MASK)
        return;

    /* Unless we override this below, we'll check the (up to) 8 squares listed
     * in move_array. */
    move_array = &moves[current_pos][0];
    num_moves = 8;

    /* If any square we can move to has an adjacent_unvisited count of 1,
     * and it is not an ending square, we must visit it next. If we don't, then
     * it'll be left with only one route into it and no route out. If there's
     * more than one such square, fail. */
    for (int move_idx = 0; move_idx < 8; move_idx++) {
        SQUARE dest = move_array[move_idx];
        if (dest == INVALID_SQUARE)
            break;
        if (!bb_test(pos->visited, dest) && pos->adjacent_unvisited[dest] <= 1 && !IS_END_SQUARE(dest)) {
            if (num_moves == 1) {
                /* Two squares telling us we must visit them next */
                return;
            }
            /* We will set move_array to &singleton_move, but not while we're
             * iterating over move_array */
            singleton_move = dest;
            num_moves = 1;
        }
    }
#if LAST_SPACE_OPTIMISATION
    /* If a previous sum calculation requires us to move to a particular square
     * on this step, enforce that now. */
    required_square = pos->step_to_square_required_by_sum[pos->next_step];
    if (required_square != INVALID_SQUARE) {
        if (num_moves == 1) {
            /* We're already restricted to only one possible move - it had
             * better correspond with this requirement or we'll give up */
            if (required_square != singleton_move) {
                return;
            }
        }
        if (!knight_adjacent_squares[current_pos][required_square]) {
            /* Required square isn't a knight's move away from where we are */
            return;
        }
        singleton_move = required_square;
        num_moves = 1;
    }
#endif
    if (num_moves == 1) {
        move_array = &singleton_move;
    }

    /* Iterate over all possible destinations from the current position */
    for (int move_idx = 0; move_idx < num_moves; move_idx++) {
        bool invalid = false;
#if LAST_SPACE_OPTIMISATION
        SQUARE sum_force_square[2] = { INVALID_SQUARE, INVALID_SQUARE };
        STEP sum_force_step[2] = {0, 0};
#endif

        SQUARE dest = move_array[move_idx];
        if (dest == INVALID_SQUARE) {
            /* No more valid moves from this square */
            break;
        }

        if (bb_test(pos->visited, dest)) {
            /* We've already visited dest */
            continue;
        }

        /* Is it impossible to visit all nodes from here? */
        if (pos->adjacent_unvisited_counts[0] > (pos->adjacent_unvisited[dest] == 0) ||
                pos->adjacent_unvisited_counts[1] > 1 + (pos->adjacent_unvisited[dest] == 1)) {
            continue;
        }

#if ENFORCE_SUM_RULE
        /* For each of the two lines shared by this square, check that the
         * line's sum so far doesn't stray outside the relevant limits. */
        for (int line_index = 0; !invalid && line_index < 2; line_index++) {
            LINE line = square_to_lines[dest][line_index];
            short new_sum = pos->line_sum[line] + pos->next_step;
            invalid = (new_sum < sum_minimums[LINE_LENGTH - (pos->line_count[line] + 1)]);
            if (!invalid) {
                if (pos->line_count[line] == LINE_LENGTH - 2) {
                    /* Special case: if there would be one more space in this
                     * line after filling in this square with pos->next_step,
                     * its sum so far can't be greater than
                     * LINE_SUM - (pos->next_step + 1). Since we know that
                     * consecutive numbers can't be on the same line, the max
                     * is actually LINE_SUM - (pos->next_step + 2). */
                    invalid = (new_sum > LINE_SUM - (pos->next_step + 2));
                }
                else {
                    invalid = (new_sum > sum_maximums[LINE_LENGTH - (pos->line_count[line] + 1)]);
                }
            }

            if (invalid) {
                //sum_opt_elim_count++;
                break;
            }
#if LAST_SPACE_OPTIMISATION
            else if (pos->line_count[line] == LINE_LENGTH - 2) {
                /* If we place this number here, this line will only have one
                 * more space in it. We can now trivially work out what that
                 * number must be. */

                /* Find the one remaining space in this line */
                for (int i = 0; i < LINE_LENGTH; i++) {
                    SQUARE sq = line_to_squares[line][i];
                    if (sq != dest && !bb_test(pos->visited, sq)) {
                        /* sq is the last space in this line after dest is filled */
                        STEP forced_step_no = LINE_SUM - new_sum;
                        if (forced_step_no < 1 || forced_step_no > NUM_SQUARES) {
                            invalid = true;
                            //last_space_opt_count++;
                            printf("forced_step_no %d? sq %d, dest %d\n", (int) forced_step_no, (int) sq, (int) dest);
                            break;
                        }
                        sum_force_square[line_index] = sq;
                        sum_force_step[line_index] = forced_step_no;
                        if (pos->step_to_square_required_by_sum[forced_step_no] != sq && pos->step_to_square_required_by_sum[forced_step_no] != INVALID_SQUARE) {
                            /* This step number has already been required
                             * someplace else! */
                            invalid = true;
                            //last_space_opt_count++;
                            break;
                        }

                        /* If the other line shared by square sq *also* has
                         * three numbers in it, check that the number we're
                         * forcing ourselves to put there also makes the right
                         * sum for that line. */
                        LINE other_line = (square_to_lines[sq][0] == line ? square_to_lines[sq][1] : square_to_lines[sq][0]);
                        if (pos->line_count[other_line] == LINE_LENGTH - 1 && pos->line_sum[other_line] + forced_step_no != LINE_SUM) {
                            invalid = true;
                            //last_space_opt_count++;
                            break;
                        }

                        /* If we want to force forced_step_no to be on sq,
                         * forced_step_no - 1 and forced_step_no + 1 (if set)
                         * must be a knight's move away from sq. */
                        /* step_to_square_required_by_sum[0] is always INVALID_SQUARE */
                        SQUARE prev_forced_square = pos->step_to_square_required_by_sum[forced_step_no - 1];
                        invalid |= (prev_forced_square != INVALID_SQUARE && !knight_adjacent_squares[prev_forced_square][sq]);
                        if (forced_step_no < NUM_SQUARES) {
                            SQUARE next_forced_square = pos->step_to_square_required_by_sum[forced_step_no + 1];
                            invalid |= (next_forced_square != INVALID_SQUARE && !knight_adjacent_squares[next_forced_square][sq]);
                        }

                        break;
                    }
                }
            }
#endif
        }
#endif
        if (!invalid) {
#if REUSE_POS
            struct ke_pos *next_pos_p = pos;
#else
            struct ke_pos next_pos = *pos;
            struct ke_pos *next_pos_p = &next_pos;
#endif

#if LAST_SPACE_OPTIMISATION
            /* If the sums of any quadrant lines dictated that a particular
             * square must contain a particular number, enforce that now. */
            for (int line_index = 0; line_index < 2; line_index++) {
                SQUARE sq = sum_force_square[line_index];
                STEP step = sum_force_step[line_index];
                if (sq != INVALID_SQUARE) {
                    assert(!bb_test(next_pos_p->visited, sq));
                    next_pos_p->step_to_square_required_by_sum[step] = sq;
                }
            }
#endif

            /* Put pos->next_step at square dest */
            make_step(next_pos_p, dest);

            /* Recurse */
            tour(next_pos_p, max_steps, complete_callback, cookie);

#if REUSE_POS
            /* Unmake the step, putting pos back how it was */
            unmake_step(pos);

#if LAST_SPACE_OPTIMISATION
            /* Undo any modification we just made to step_to_square_required_by_sum */
            for (int line_index = 0; line_index < 2; line_index++) {
                SQUARE sq = sum_force_square[line_index];
                STEP step = sum_force_step[line_index];
                if (sq != INVALID_SQUARE) {
                    pos->step_to_square_required_by_sum[step] = INVALID_SQUARE;
                }
            }
#endif
#endif
        }
    }
}


/* Simple callback to increment an integer, called to get the number of
 * N-step tours from a starting position. We use this with tour() in the main
 * thread so we know how many jobs will be given to workers, so we can display
 * a progress percentage. */
static void count(void *vp, const struct ke_pos *pos) {
    (*(int *) vp)++;
}

void
print_help(FILE *f) {
    fprintf(f, "knighterrant - find nested semimagic knight's tours\n");

#if TOUR_TEST
    fprintf(f, "\n");
    fprintf(f, "This is a test build which will instead count all the ordinary knight's tours\n"
            "on a %d*%d board, including rotations and reflections.\n", BOARD_DIM, BOARD_DIM);
#if BOARD_DIM == 6
    fprintf(f, "The expected result is 6637920 tours.\n");
#elif BOARD_DIM == 5
    fprintf(f, "The expected result is 1728 tours.\n");
#endif
#endif

    fprintf(f, "\n");
    fprintf(f, "Compile-time options:\n");
    fprintf(f, "    Board size: %d*%d (%d squares)\n", BOARD_DIM, BOARD_DIM, NUM_SQUARES);
#if ENFORCE_SUM_RULE
    fprintf(f, "    Each half-row and half-column must sum to: %d\n", LINE_SUM);

#if LAST_SPACE_OPTIMISATION
    fprintf(f, "    Last space in line optimisation: on.\n");
#else
    fprintf(f, "    Last space in line optimisation: off.\n");
#endif

#else
    fprintf(f, "    No requirement on row or column sum.\n");
#endif

#if TOUR_TEST
    fprintf(f, "    Starting squares: any.\n");
    fprintf(f, "    Ending squares: any.\n");
#elif RESTRICT_START_AND_END
    fprintf(f, "    Starting squares: top half of leftmost column.\n");
    fprintf(f, "    Ending squares: rightmost column.\n");
#else
    fprintf(f, "    Starting squares: any in lower-left diagonal half of top-left quadrant.\n");
    fprintf(f, "    Ending squares: any.\n");
#endif

    fprintf(f, "\n");
    fprintf(f, "Usage: knighterrant [options]\n");
    fprintf(f, "Options:\n");
    fprintf(f, "    -q        Don't show progress output.\n");
    fprintf(f, "    -s <N>    Search N steps deep before handing the state to a worker thread.\n");
    fprintf(f, "    -t <N>    Use N worker threads. If N is 0, -s has no effect.\n");
}

int main(int argc, char **argv) {
    int num_threads = 4;
    int steps_before_parallelism = 4;
    int num_starting_squares = 0;
#if BOARD_DIM == 8

#if RESTRICT_START_AND_END
    /* Only start on squares 0, 8, 16 and 24, the four squares in the top half
     * of the leftmost column. */
    SQUARE starting_squares[] = { 0, 8, 16, 24 };
    num_starting_squares = 4;
#else
    /* We don't need to try every starting square or we'll just get rotations
     * and reflections of other solutions. Instead, divide the top-left
     * quadrant in half diagonally and just start on the squares in one half
     * of that quadrant, including the squares on the diagonal. Tours starting
     * from other squares will just be rotations and/or reflections of these. */
    SQUARE starting_squares[] = { 0, 8, 9, 16, 17, 18, 24, 25, 26, 27 };
    num_starting_squares = 10;

    /* Note that we'll still get some duplicates, because the above will still
     * find solutions which start on the diagonal line and are diagonal
     * reflections of each other. */
#endif

#else
    /* Test mode: iterate over every square. */
    SQUARE starting_squares[NUM_SQUARES];
    for (SQUARE i = 0; i < NUM_SQUARES; i++) {
        starting_squares[i] = i;
    }
    num_starting_squares = NUM_SQUARES;
#endif
    struct worker_thread *worker_threads = NULL;
    struct job_buffer job_buffer;
    int num_expected_jobs = 0;
    void (*tour_callback)(void *, const struct ke_pos *);
    void *tour_callback_cookie;
    int c;

    while ((c = getopt(argc, argv, "t:s:hq")) != -1) {
        switch (c) {
            case 't':
                num_threads = atoi(optarg);
                break;

            case 's':
                steps_before_parallelism = atoi(optarg);
                break;

            case 'h':
                print_help(stdout);
                exit(0);
                break;

            case 'q':
                show_progress = false;
                break;

            default:
                exit(1);
        }
    }

    /* Precompute various useful global lookup tables */
    precompute();

    printf("Using board size %d*%d and %d threads\n", BOARD_DIM, BOARD_DIM, num_threads);

    if (num_threads == 0) {
        /* Do everything sequentially */
        steps_before_parallelism = NUM_SQUARES;
        tour_callback = emit_completed_tour;
        tour_callback_cookie = NULL;
    }
    else {
        /* Do it sequentially up to steps_before_parallelism steps, then
         * hand off the state from there to a worker thread */
        tour_callback = job_buffer_enqueue;
        tour_callback_cookie = &job_buffer;

        if (show_progress) {
            /* Count how many paths there are up to steps_before_parallelism
             * steps, so our progress output gives an accurate percentage of
             * jobs dequeued / total jobs to do. */
            for (int i = 0; i < num_starting_squares; ++i) {
                SQUARE start = starting_squares[i];
                struct ke_pos pos;
                ke_pos_init(&pos, start);
                tour(&pos, steps_before_parallelism, count, &num_expected_jobs);
            }
        }
    }

    if (num_threads > 0) {
        /* Start our worker threads */
        worker_threads = malloc(sizeof(struct worker_thread) * num_threads);
        memset(worker_threads, 0, sizeof(struct worker_thread) * num_threads);
        job_buffer_init(&job_buffer, num_threads, num_expected_jobs, show_progress ? update_progress : NULL);
        for (int i = 0; i < num_threads; i++) {
            int ret;
            worker_threads[i].job_buffer = &job_buffer;
            worker_threads[i].thread_index = i;
            worker_threads[i].emit = emit_completed_tour;
            worker_threads[i].emit_cookie = NULL;
            ret = pthread_create(&worker_threads[i].thread_id, NULL, ke_worker_thread, &worker_threads[i]);
            if (ret != 0) {
                error(1, ret, "pthread_create");
            }
        }
    }

    for (int i = 0; i < num_starting_squares; ++i) {
        /* Position the knight on our starting square */
        SQUARE start = starting_squares[i];
        struct ke_pos pos;
        ke_pos_init(&pos, start);

        /* Find all the tour prefixes up to steps_before_parallelism steps
         * long, and hand each such state to a worker thread. Or if num_threads
         * is zero, steps_before_parallelism is the total number of steps,
         * we never hand anything to a worker thread, and tour_callback prints
         * the completed tour. */
        tour(&pos, steps_before_parallelism, tour_callback, tour_callback_cookie);
    }

    if (num_threads > 0) {
        /* Tell the threads there's no more work to do */
        job_buffer_finish(&job_buffer);

        for (int i = 0; i < num_threads; i++) {
            pthread_join(worker_threads[i].thread_id, NULL);
        }
        free(worker_threads);
        update_progress(&job_buffer);
        job_buffer_destroy(&job_buffer);
    }

    printf("\n");
    printf("Found %lld tours\n", num_tours_found);
    if (num_closed_tours_found) {
        printf("    of which %lld are closed tours\n", num_closed_tours_found);
    }

    return 0;
}

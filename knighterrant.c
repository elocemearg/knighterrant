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

typedef int8_t STEP;
typedef int8_t SQUARE;
typedef int8_t LINE;
typedef uint64_t BOARDBITMAP;

const SQUARE INVALID_SQUARE = -1;

/* If TEST6 is defined, we use a 6*6 board and run in a test mode where we find
 * all knight's tours, regardless of row/column sum and regardless of starting
 * square, and output the total number of tours found.
 *
 * For a 5*5 board the correct number of tours is 1728, and for a 6*6 board
 * the correct number of tours is 6637920. Source: htps://oeis.org/A165134
 */
//#define TEST6

#define REUSE_POS 1

#ifdef TEST6
#define BOARD_DIM 6
#else
#define BOARD_DIM 8
#endif

#define NUM_SQUARES (BOARD_DIM * BOARD_DIM)

/* On an 8*8 board, in each 4*4 quadrant, all four rows must add up to 130 and
 * all four columns must add up to 130.
 * We'll call these four-element rows "lines". There are 32 of them.
 * Each square is on two different lines. */

/* BOARD_DIM lines per quadrant */
#define NUM_LINES (BOARD_DIM * 4)

#define LINE_SUM (NUM_SQUARES * 2 + 2)
#define LINE_LENGTH (BOARD_DIM / 2)

#if BOARD_DIM == 8
/* Tour must end on the right hand side */
#define END_SQUARE_MASK 0x8080808080808080ULL
#define ENFORCE_SUM_RULE 1
#define LAST_SPACE_OPTIMISATION 1
#else
#define END_SQUARE_MASK 0xffffffffffffffffULL
#endif

#define UTF8_BOX_LINES 1

#define IS_END_SQUARE(sq) (bb_test(END_SQUARE_MASK, (sq)))

/* Characters with which to draw the grid when we output a completed tour */
#ifdef UTF8_BOX_LINES
#define BOX_TOP_LEFT "┌"
#define BOX_TOP_RIGHT "┐"
#define BOX_T "┬"
#define BOX_INVERTED_T "┴"
#define BOX_LEFT_JUNC "├"
#define BOX_RIGHT_JUNC "┤"
#define BOX_BOTTOM_LEFT "└"
#define BOX_BOTTOM_RIGHT "┘"
#define BOX_CROSS "┼"
#define BOX_HORIZONTAL "─"
#define BOX_VERTICAL "│"
#else
#define BOX_TOP_LEFT "+"
#define BOX_TOP_RIGHT "+"
#define BOX_T "+"
#define BOX_INVERTED_T "+"
#define BOX_LEFT_JUNC "+"
#define BOX_RIGHT_JUNC "+"
#define BOX_BOTTOM_LEFT "+"
#define BOX_BOTTOM_RIGHT "+"
#define BOX_CROSS "+"
#define BOX_HORIZONTAL "-"
#define BOX_VERTICAL "|"
#endif

/* Precomputed: for each square, the IDs of the two lines that square is on. */
LINE square_to_lines[NUM_SQUARES][2];

/* Precomputed: for each line ID, the squares that lie on it. */
SQUARE line_to_squares[NUM_LINES][LINE_LENGTH];

/* Precomputed: for each square, the <= 8 possible next squares the knight can
 * move to.
 * If there are fewer than 8, excess cells are filled with INVALID_SQUARE. */
SQUARE moves[NUM_SQUARES][8];

/* Precomputed: knight_adjacent_squares[x][y] is true if x and y are a knight's
 * move apart. */
bool knight_adjacent_squares[NUM_SQUARES][NUM_SQUARES];

struct ke_pos {
    /* For each line, the sum of the numbers currently present in that line. */
    short line_sum[NUM_LINES];

    /* For each line, the number of visited squares in that line. */
    int8_t line_count[NUM_LINES];

    /* For each square, the number in that square, or 0 if not yet filled in. */
    STEP square_to_step[NUM_SQUARES];

    /* For each step number, step_to_square[step] is the square containing
     * that step number. */
    SQUARE step_to_square[NUM_SQUARES + 1];

#ifdef LAST_SPACE_OPTIMISATION
    /* If we fill in a number leaving only one space left on a line, we
     * already know what that number must be.
     * step_to_square_required_by_sum[step] = sq if some future step number
     * "step" must be on "sq", or INVALID_SQUARE otherwise. */
    SQUARE step_to_square_required_by_sum[NUM_SQUARES + 1];
#endif

    /* For each unvisited square, this stores the number of unvisited squares
     * which are one knight's move away. If sq is visited,
     * adjacent_unvisited[sq] is -1 times the value it had when the square
     * was last unvisited. */
    int8_t adjacent_unvisited[NUM_SQUARES];

    /* Lookup table containing the number of 0s, 1s, 2s, ... etc in the above
     * array. adjacent_unvisited_counts[1], for example, gives the number of
     * unvisited squares which have exactly one knight-adjacent unvisited
     * square. */
    int8_t adjacent_unvisited_counts[9];

    /* The next number to enter into the grid. */
    STEP next_step;

    /* Which square the last number was entered into. */
    SQUARE pos;

    /* The bitmap of visited squares. */
    BOARDBITMAP visited;
};

#ifdef ENFORCE_SUM_RULE
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
#if BOARD_DIM == 8
    /* Special case for an 8x8 board...
     *
     * If a 4x4 quadrant has a line with two numbers filled in, these two
     * numbers must sum to at least 8.
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
    /* Non-8*8 board: do not enforce a minimum for sum_minimums[2]. */
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
#if BOARD_DIM == 8
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

/* Macros for setting and testing bits in a BOARDBITMAP */
#define bb_set(bp, sq) *(bp) |= (1ULL << (sq))
#define bb_unset(bp, sq) *(bp) &= ~(1ULL << (sq))
#define bb_test(b, sq) (((b) & (1ULL << (sq))) != 0)

void
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


/* Thread handling stuff */
pthread_mutex_t emit_mutex = PTHREAD_MUTEX_INITIALIZER;
long long num_tours_found = 0;
bool show_progress = true;

#define MUTEX_LOCK(m) while (pthread_mutex_lock(m) == EINTR);
#define MUTEX_UNLOCK(m) pthread_mutex_unlock(m)
#define CV_WAIT(c, m) pthread_cond_wait(c, m)
#define CV_SIGNAL(c) pthread_cond_signal(c)

/* Job buffer: meeting point for the main thread and the worker threads.
 * The main thread will put a job in here (a board position with a knight
 * having done a small number of steps) and a worker thread will take it
 * from there. */
struct job_buffer {
    pthread_mutex_t mutex;
    pthread_cond_t empty_cv;
    pthread_cond_t full_cv;

    int jobs_dequeued;
    int num_expected_jobs;
    int num_threads_running;
    bool full;
    bool finished;
    struct ke_pos ke_state;
};

struct worker_thread {
    pthread_t thread_id;
    int thread_index;
    struct job_buffer *job_buffer;
};

void
job_buffer_init(struct job_buffer *buf, int num_threads, int num_expected_jobs) {
    memset(buf, 0, sizeof(*buf));
    pthread_mutex_init(&buf->mutex, NULL);
    pthread_cond_init(&buf->empty_cv, NULL);
    pthread_cond_init(&buf->full_cv, NULL);
    buf->num_threads_running = num_threads;
    buf->num_expected_jobs = num_expected_jobs;
}

void
job_buffer_destroy(struct job_buffer *buf) {
    pthread_mutex_destroy(&buf->mutex);
    pthread_cond_destroy(&buf->empty_cv);
    pthread_cond_destroy(&buf->full_cv);
}

/* Must be called with job_buffer->mutex held */
void
update_progress(struct job_buffer *job_buffer) {
    /* Number of jobs completed is the number of jobs dequeued minus the number
     * of threads still working. */
    int progress_pc;

    if (!show_progress)
        return;

    progress_pc = ((job_buffer->jobs_dequeued - job_buffer->num_threads_running) * 100) / job_buffer->num_expected_jobs;
    if (progress_pc < 0)
        progress_pc = 0;
    MUTEX_LOCK(&emit_mutex);
    fprintf(stderr, " %10lld tours found   %3d threads running   %3d%%\r",
            num_tours_found, job_buffer->num_threads_running, progress_pc);
    MUTEX_UNLOCK(&emit_mutex);
}

void
job_buffer_enqueue(void *job_buffer_vp, const struct ke_pos *ke_pos) {
    struct job_buffer *job_buffer = (struct job_buffer *) job_buffer_vp;
    MUTEX_LOCK(&job_buffer->mutex);
    while (job_buffer->full) {
        CV_WAIT(&job_buffer->empty_cv, &job_buffer->mutex);
    }

    /* Buffer is empty. Copy our board position in to it. */
    if (ke_pos == NULL) {
        /* No new work - we've finished. */
        job_buffer->finished = true;
    }
    else {
        job_buffer->ke_state = *ke_pos;
    }

    /* Indicate the buffer is full and wake up anything waiting for work. */
    job_buffer->full = true;
    CV_SIGNAL(&job_buffer->full_cv);

    MUTEX_UNLOCK(&job_buffer->mutex);
}

void
job_buffer_finish(struct job_buffer *buf) {
    job_buffer_enqueue(buf, NULL);
}

bool
job_buffer_dequeue(struct job_buffer *job_buffer, struct ke_pos *returned_state) {
    bool job_dequeued = false;
    MUTEX_LOCK(&job_buffer->mutex);
    while (!job_buffer->full) {
        CV_WAIT(&job_buffer->full_cv, &job_buffer->mutex);
    }

    /* Buffer is full. Take the payload if there is one, or we've finished. */
    if (!job_buffer->finished) {
        *returned_state = job_buffer->ke_state;
        job_dequeued = true;
    }
    job_buffer->full = false;
    if (job_dequeued) {
        job_buffer->jobs_dequeued++;
        if (show_progress) {
            update_progress(job_buffer);
        }
    }
    CV_SIGNAL(&job_buffer->empty_cv);
    MUTEX_UNLOCK(&job_buffer->mutex);
    return job_dequeued;
}

/* Functions for adjusting the adjacent_unvisited and adjacent_unvisited_counts
 * arrays. */
void
decrement_adjacent_unvisited(struct ke_pos *pos, SQUARE square) {
    if (!bb_test(pos->visited, square)) {
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]--;
        pos->adjacent_unvisited[square]--;
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]++;
    }
}

void
increment_adjacent_unvisited(struct ke_pos *pos, SQUARE square) {
    if (!bb_test(pos->visited, square)) {
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]--;
        pos->adjacent_unvisited[square]++;
        pos->adjacent_unvisited_counts[pos->adjacent_unvisited[square]]++;
    }
}

/* Unplays the last move. */
void
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
void
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

void
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
#ifdef LAST_SPACE_OPTIMISATION
        pos->step_to_square_required_by_sum[step] = INVALID_SQUARE;
#endif
        pos->step_to_square[step] = INVALID_SQUARE;
    }

    pos->next_step = 1;
    make_step(pos, initial_square);
}

void
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

void
print_mid_row_border() {
    printf(BOX_LEFT_JUNC);
    for (int i = 0; i < BOARD_DIM; ++i) {
        if (i > 0)
            printf(BOX_CROSS);
        printf(BOX_HORIZONTAL BOX_HORIZONTAL);
    }
    printf(BOX_RIGHT_JUNC "\n");
}

void
print_bottom_row_border() {
    printf(BOX_BOTTOM_LEFT);
    for (int i = 0; i < BOARD_DIM; ++i) {
        if (i > 0)
            printf(BOX_INVERTED_T);
        printf(BOX_HORIZONTAL BOX_HORIZONTAL);
    }
    printf(BOX_BOTTOM_RIGHT "\n");
}

void
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

void
check_tour(const struct ke_pos *pos) {
    /* Check that every square has been visited */
#if BOARD_DIM == 8
    assert(pos->visited == (BOARDBITMAP) -1);
#else
    assert(pos->visited == (1ULL << NUM_SQUARES) - 1);
#endif

#ifdef ENFORCE_SUM_RULE
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

void
emit_completed_tour(void *cookie, const struct ke_pos *completed_state) {
    MUTEX_LOCK(&emit_mutex);
    num_tours_found++;
#if BOARD_DIM == 8
    printf("\nFound tour #%lld...\n", num_tours_found);
    print_tour(completed_state);
#endif
    check_tour(completed_state);
    MUTEX_UNLOCK(&emit_mutex);
}

void
tour(struct ke_pos *pos, int max_steps,
        void (*complete_callback)(void *cookie, const struct ke_pos *completed_state), void *cookie) {
    SQUARE current_pos = pos->pos;
    SQUARE *move_array;
    SQUARE singleton_move;
    int num_moves;
    SQUARE required_square;

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
#ifdef LAST_SPACE_OPTIMISATION
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
        SQUARE sum_force_square[2] = { INVALID_SQUARE, INVALID_SQUARE };
        STEP sum_force_step[2] = {0, 0};

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

#ifdef ENFORCE_SUM_RULE
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
#ifdef LAST_SPACE_OPTIMISATION
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
#ifdef REUSE_POS
            struct ke_pos *next_pos_p = pos;
#else
            struct ke_pos next_pos = *pos;
            struct ke_pos *next_pos_p = &next_pos;
#endif

#ifdef LAST_SPACE_OPTIMISATION
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

#ifdef REUSE_POS
            /* Unmake the step, putting pos back how it was */
            unmake_step(pos);

#ifdef LAST_SPACE_OPTIMISATION
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

/* A worker thread.
 *
 * We'll keep dequeueing jobs (struct ke_pos of a partial tour) from the
 * job buffer, doing each job (search all possible moves from that position),
 * until there are no more jobs (job_buffer_dequeue() returns false).
 */
void *
ke_worker_thread(void *arg) {
    struct worker_thread *context = (struct worker_thread *) arg;
    struct ke_pos position;

    while (job_buffer_dequeue(context->job_buffer, &position)) {
        /* Search from this position up to NUM_SQUARES steps */
        tour(&position, NUM_SQUARES, emit_completed_tour, NULL);
    }
    MUTEX_LOCK(&context->job_buffer->mutex);
    context->job_buffer->num_threads_running--;
    update_progress(context->job_buffer);
    MUTEX_UNLOCK(&context->job_buffer->mutex);
    return NULL;
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
    fprintf(f, "knighterrant - find magic knight's tours\n");
#if BOARD_DIM != 8
    fprintf(f, "This is a test build which will instead count all the ordinary knight's tours\n"
            "on a %d*%d board, including rotations and reflections.\n", BOARD_DIM, BOARD_DIM);
#endif
    fprintf(f, "\n");
    fprintf(f, "Usage: knighterrant [options]\n");
    fprintf(f, "Options:\n");
    fprintf(f, "    -q        Don't show progress output.\n");
    fprintf(f, "    -s <N>    Search N steps deep before handing the state to a worker thread.\n");
    fprintf(f, "    -t <N>    Use N worker threads. If N is 0, -s has no effect.\n");
}

int main(int argc, char **argv) {
    SQUARE start_square = 0;
    int num_threads = 4;
    int steps_before_parallelism = 4;
#if BOARD_DIM == 8
    /* Only start on squares 0, 8, 16 and 24, the four squares in the top half
     * of the leftmost column. */
    int8_t start_square_step = 8;
    SQUARE last_square = 24;
#else
    /* Test mode: iterate over every square. */
    int8_t start_square_step = 1;
    SQUARE last_square = NUM_SQUARES - 1;
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

#if BOARD_DIM == 8
    /* If it's an 8*8 board, we make certain assumptions that we're not
     * finding all tours, we're only finding those that start on the leftmost
     * column and end on the rightmost column. */
    if (start_square % 8 != 0) {
        fprintf(stderr, "start square must be a multiple of 8 (left column of board)\n");
        exit(1);
    }
#endif

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
            for (SQUARE start = start_square; start <= last_square; start += start_square_step) {
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
        job_buffer_init(&job_buffer, num_threads, num_expected_jobs);
        for (int i = 0; i < num_threads; i++) {
            int ret;
            worker_threads[i].job_buffer = &job_buffer;
            worker_threads[i].thread_index = i;
            ret = pthread_create(&worker_threads[i].thread_id, NULL, ke_worker_thread, &worker_threads[i]);
            if (ret != 0) {
                error(1, ret, "pthread_create");
            }
        }
    }

    for (SQUARE start = start_square; start <= last_square; start += start_square_step) {
        /* Position the knight on our starting square */
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
        for (int i = 0; i < num_threads; i++) {
            job_buffer_finish(&job_buffer);
        }

        for (int i = 0; i < num_threads; i++) {
            pthread_join(worker_threads[i].thread_id, NULL);
        }
        free(worker_threads);
        update_progress(&job_buffer);
        job_buffer_destroy(&job_buffer);
    }

    printf("\n");
    printf("Found %lld tours\n", num_tours_found);
    //printf("sum_opt_elim_count %lld\n", sum_opt_elim_count);
    //printf("last_space_opt_count %lld\n", last_space_opt_count);

    return 0;
}

#ifndef _KNIGHTERRANT_H
#define _KNIGHTERRANT_H

#include <stdint.h>
#include <stdbool.h>
#include <errno.h>
#include <pthread.h>

typedef int8_t STEP;
typedef int8_t SQUARE;
typedef int8_t LINE;
typedef uint64_t BOARDBITMAP;

/* If REUSE_POS is true, then we reuse the same struct ke_pos object for each
 * recursive step, unmaking the move when we've finished. Otherwise, we copy
 * the ke_pos object to a new object and pass that down to the next step. */
#ifndef REUSE_POS
    #define REUSE_POS 1
#endif

/* If TOUR_TEST is true, we use a 6*6 board and run in a test mode where we
 * find all knight's tours, regardless of row/column sum and regardless of
 * starting square, and output the total number of tours found.
 *
 * For a 5*5 board the correct number of tours is 1728, and for a 6*6 board
 * the correct number of tours is 6637920. Source: htps://oeis.org/A165134
 */
#ifndef TOUR_TEST
    #define TOUR_TEST 0
#endif

#if TOUR_TEST
    /* 6*6 board, test mode */
    #ifndef BOARD_DIM
        #define BOARD_DIM 6
    #endif
    #define ENFORCE_SUM_RULE 0
    #define LAST_SPACE_OPTIMISATION 0
#else
    /* 8*8 board, enforce sum rule */
    #define BOARD_DIM 8

    /* If ENFORCE_SUM_RULE is true, then we enforce that the numbers in each
     * half-row and half-column sum to LINE_SUM, defined below. We always
     * enforce this unless TOUR_TEST is true. */
    #define ENFORCE_SUM_RULE 1

    /* If LAST_SPACE_OPTIMISATION is true, then when a line has only one more
     * empty square in it, we pencil in what number must go there, which
     * restricts our options later on and helps rule out fruitless paths. */
    #define LAST_SPACE_OPTIMISATION 1
#endif

/* If RESTRICT_START_AND_END is true, the tour must start in the leftmost
 * column and finish in the rightmost column. */
#ifndef RESTRICT_START_AND_END
#define RESTRICT_START_AND_END 1
#endif

/* Number of squares on the board */
#define NUM_SQUARES (BOARD_DIM * BOARD_DIM)

/* On an 8*8 board, in each 4*4 quadrant, all four rows must add up to 130 and
 * all four columns must add up to 130.
 * We'll call these four-element rows "lines". There are 32 of them.
 * Each square is on two different lines. */

/* BOARD_DIM lines per quadrant, four quadrants on the board */
#define NUM_LINES (BOARD_DIM * 4)

#if BOARD_DIM % 2 != 0 && ENFORCE_SUM_RULE
#error "Can't enforce half-row and half column sum if board side length is odd"
#endif

/* Each half-row and half column must sum to half the magic constant for the
 * whole square. The magic constant for the whole square is the sum of the
 * numbers from 1 to NUM_SQUARES divided by the number of rows, so the half-row
 * sum is half that:
 *    ((((1 + NUM_SQUARES) * NUM_SQUARES) / 2) / BOARD_DIM) / 2
 * simplifying:
 *    ((1 + NUM_SQUARES) * NUM_SQUARES) / (4 * BOARD_DIM)
 * NUM_SQUARES = BOARD_DIM * BOARD_DIM, so this simplifies to:
 *    ((1 + NUM_SQUARES) * BOARD_DIM) / 4
 */
#define LINE_SUM (((1 + NUM_SQUARES) * BOARD_DIM) / 4)
#define LINE_LENGTH (BOARD_DIM / 2)

/* Define the set of squares on which the tour is permitted to finish. */
#if BOARD_DIM == 8
    #if RESTRICT_START_AND_END
        /* Tour must end on the right hand side */
        #define END_SQUARE_MASK 0x8080808080808080ULL
    #else
        /* Tour may end anywhere */
        #define END_SQUARE_MASK 0xffffffffffffffffULL
    #endif
#else
    #define END_SQUARE_MASK 0xffffffffffffffffULL
#endif

#ifndef UTF8_BOX_LINES
    #define UTF8_BOX_LINES 1
#endif

#define IS_END_SQUARE(sq) (bb_test(END_SQUARE_MASK, (sq)))

/* Characters with which to draw the grid when we output a completed tour */
#if UTF8_BOX_LINES
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

/* Macros for setting and testing bits in a BOARDBITMAP */
#define bb_set(bp, sq) *(bp) |= (1ULL << (sq))
#define bb_unset(bp, sq) *(bp) &= ~(1ULL << (sq))
#define bb_test(b, sq) (((b) & (1ULL << (sq))) != 0)

/* The state of a partial path, which contains the current position of the
 * knight, the position of all the numbers already placed, and various sums
 * and other statistics that help us determine when a path can no longer lead
 * to a solution. */
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

#if LAST_SPACE_OPTIMISATION
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

/* Job buffer: meeting point for the main thread and the worker threads.
 * The main thread will put a job in here (a board position with a knight
 * having done a small number of steps) and a worker thread will take it
 * from there. */
struct job_buffer {
    /* Mutex protecting the state in this job_buffer, but especially
     * ke_state, full, finished */
    pthread_mutex_t mutex;

    /* Condition variable which will be signalled when full is false */
    pthread_cond_t empty_cv;

    /* Condition variable which will be signalled when full is true or
     * finished is true */
    pthread_cond_t full_cv;

    /* The number of jobs dequeued since the job buffer was initialised */
    int jobs_dequeued;

    /* The number of jobs expected to be enqueued, for progress estimates */
    int num_expected_jobs;

    /* The number of threads still running. job_buffer_init() initialises this
     * to the number of worker threads, and when each worker thread exits it
     * decrements num_threads_remaining. */
    int num_threads_running;

    /* True if a position has been copied into ke_state by the main thread */
    bool full;

    /* True if the main thread has signalled that there are no more jobs to
     * enqueue, and the worker threads should exit */
    bool finished;

    /* Progress indicator function to call whenever a job is dequeued or a
     * thread exits. If this is NULL, it has no effect. */
    void (*update_progress)(struct job_buffer *);

    /* Position to be passed to a worker thread. A worker thread will find all
     * solutions which start with the sequence of steps already placed here. */
    struct ke_pos ke_state;
};

/* Pointer to this is given as an argument to each worker thread via
 * pthread_create(). */
struct worker_thread {
    /* Thread ID as returned by pthread_create */
    pthread_t thread_id;

    /* Thread number, [0, num_threads) */
    int thread_index;

    /* Pointer to the job buffer, with which the main thread gives us work and
     * the worker threads take it. All worker threads and the main thread have
     * a reference to the same struct job_buffer. */
    struct job_buffer *job_buffer;

    /* Callback for the worker thread to call when it has found a completed
     * tour from the position it has been given. The first parameter is
     * emit_cookie, and the second is a pointer to the struct ke_pos describing
     * the completed tour. */
    void (*emit)(void *cookie, const struct ke_pos *completed_state);

    /* Pointer to pass as the first argument to emit(). This has no meaning to
     * the worker thread. */
    void *emit_cookie;
};

#define MUTEX_LOCK(m) while (pthread_mutex_lock(m) == EINTR);
#define MUTEX_UNLOCK(m) pthread_mutex_unlock(m)
#define CV_WAIT(c, m) pthread_cond_wait(c, m)
#define CV_SIGNAL(c) pthread_cond_signal(c)
#define CV_BROADCAST(c) pthread_cond_broadcast(c)

/* Recursively solve the problem starting from "pos", up to max_steps in total.
 * If we find any solutions, call complete_callback() with the given cookie
 * and a pointer to the solved state.
 * Return when we have output all solutions that exist from the given position,
 * which may be none. */
void
tour(struct ke_pos *pos, int max_steps,
        void (*complete_callback)(void *cookie, const struct ke_pos *completed_state), void *cookie);

/* Initialise a job buffer object.
 * num_threads: the number of worker threads.
 * num_expected_jobs: the estimated number of jobs that will be enqueued before
 *     there is no more work. This is used to print the progress indicator.
 * update_prgoress: if not NULL, this is called every time a worker thread
 *     dequeues a job or a "no more work" message. */
void
job_buffer_init(struct job_buffer *buf, int num_threads, int num_expected_jobs,
        void (*update_progress)(struct job_buffer *));

/* Destroy a previously initialised job buffer object, freeing any resources
 * associated with it. This should only be called after all worker threads
 * have finished. */
void
job_buffer_destroy(struct job_buffer *buf);

/* Enqueue a job to the job buffer. If the buffer already has a job waiting
 * in it, this function will block until a thread takes that job.
 * job_buffer_vp must be a pointer to a struct job_buffer.
 * ke_pos represents the starting position. The worker thread will find and
 * output all solutions reachable from that position.
 */
void
job_buffer_enqueue(void *job_buffer_vp, const struct ke_pos *ke_pos);

/* Tell the worker threads that there is no more work. Once the main thread
 * does not want to enqueue any more jobs, it should call this function. It
 * will enqueue num_threads "no more work" messages. When a worker thread
 * receives this message it will exit and can be reaped with pthread_join(). */
void
job_buffer_finish(struct job_buffer *buf);

/* Called by a worker thread to dequeue a job from the job buffer. If there
 * is no job waiting in the job buffer, this function blocks until there is.
 *
 * If job_buffer_dequeue() returns true, the position from which the worker
 * should search for solutions has been copied to *returned_state.
 *
 * If job_buffer_dequeue() returns false, it means there are no more jobs, this
 * worker thread must not call job_buffer_dequeue() again, and the worker
 * thread must exit.
 */
bool
job_buffer_dequeue(struct job_buffer *job_buffer, struct ke_pos *returned_state);

/* Worker thread function. "arg" must be a pointer to a struct worker_thread
 * object.
 * The thread calls job_buffer_dequeue() in a loop, solving each job given
 * to it. It will continue to do this until job_buffer_dequeue() returns
 * false (because job_buffer_finish() has been called), at which point it will
 * exit. */
void *
ke_worker_thread(void *arg);

#endif

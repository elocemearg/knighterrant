/* Code for the worker threads and the job buffer used as communication
 * between the main thread and worker threads. */

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <pthread.h>

#include "knighterrant.h"

void
job_buffer_init(struct job_buffer *buf, int num_threads, int num_expected_jobs,
        void (*update_progress)(struct job_buffer *)) {
    memset(buf, 0, sizeof(*buf));
    pthread_mutex_init(&buf->mutex, NULL);
    pthread_cond_init(&buf->empty_cv, NULL);
    pthread_cond_init(&buf->full_cv, NULL);
    buf->num_threads_running = num_threads;
    buf->num_expected_jobs = num_expected_jobs;
    buf->update_progress = update_progress;
}

void
job_buffer_destroy(struct job_buffer *buf) {
    pthread_mutex_destroy(&buf->mutex);
    pthread_cond_destroy(&buf->empty_cv);
    pthread_cond_destroy(&buf->full_cv);
}

void
job_buffer_enqueue(void *job_buffer_vp, const struct ke_pos *ke_pos) {
    struct job_buffer *job_buffer = (struct job_buffer *) job_buffer_vp;
    MUTEX_LOCK(&job_buffer->mutex);

    /* Wait for the buffer to be empty, if it isn't already */
    while (job_buffer->full) {
        CV_WAIT(&job_buffer->empty_cv, &job_buffer->mutex);
    }

    /* Buffer is empty. Copy our board position in to it. */
    if (ke_pos == NULL) {
        /* No new work - we've finished. */
        job_buffer->finished = true;
    }
    else {
        /* Put the new position in the buffer and indicate that it's full. */
        job_buffer->ke_state = *ke_pos;
        job_buffer->full = true;
    }

    /* Wake up anything waiting for work. */
    if (job_buffer->finished) {
        /* Wake up all threads waiting on the full_cv condition variable */
        CV_BROADCAST(&job_buffer->full_cv);
    }
    else {
        /* Wake up one thread waiting on the full_cv condition variable */
        CV_SIGNAL(&job_buffer->full_cv);
    }
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

    /* Wait until there's a job in the buffer or there are no more jobs */
    while (!job_buffer->full && !job_buffer->finished) {
        CV_WAIT(&job_buffer->full_cv, &job_buffer->mutex);
    }

    if (!job_buffer->finished) {
        /* Buffer is full. Take the payload. */
        *returned_state = job_buffer->ke_state;
        job_dequeued = true;
        job_buffer->full = false;
    }
    if (job_dequeued) {
        job_buffer->jobs_dequeued++;
        if (job_buffer->update_progress) {
            job_buffer->update_progress(job_buffer);
        }
    }

    /* If the main thread is waiting to enqueue another job, signal that it
     * can do so. */
    CV_SIGNAL(&job_buffer->empty_cv);
    MUTEX_UNLOCK(&job_buffer->mutex);

    /* Return true if we dequeued a job, false if we've finished */
    return job_dequeued;
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
        tour(&position, NUM_SQUARES, context->emit, context->emit_cookie);
    }

    /* No more work to do; count ourselves out */
    MUTEX_LOCK(&context->job_buffer->mutex);
    context->job_buffer->num_threads_running--;
    if (context->job_buffer->update_progress)
        context->job_buffer->update_progress(context->job_buffer);
    MUTEX_UNLOCK(&context->job_buffer->mutex);
    return NULL;
}

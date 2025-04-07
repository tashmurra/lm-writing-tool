

export type Task = {
    run: () => Promise<void>;
    abort: () => Promise<void>;
    priority: number;
    group: string;
    timeoutSeconds?: number;
}

export class TaskScheduler {
    concurrentTasks: number;
    /** Map from a unique identifier of the pending tasks to their task object */
    pendingTasks: Map<string, Task>;
    /** Map from a unique identifier of the running tasks to their task object */
    runningTasks: Map<string, Task>;
    constructor(concurrentTasks: number) {
        this.concurrentTasks = concurrentTasks;
        this.pendingTasks = new Map();
        this.runningTasks = new Map();
    }
    /**
     * Declaratively set the tasks that should be completed by the scheduler with the given group name.
     * This will
     * - Abort any running tasks in the group that are not in the new tasks list (identified by hash)
     * - Add any new tasks to the pending tasks list (identified by hash)
     * - Do nothing for tasks that are already running (identified by hash)
     * - Update the priority of tasks that are already pending (identified by hash), overwriting the old priority
     * - Remove any tasks that are no longer in the pending list (identified by hash)
     * - Start tasks in the pending list if there are fewer than concurrentTasks running tasks
     * @param tasks A map from a unique identifier of the task to the task object
     * @param group
     */
    async setTasks(tasks: Map<string, Task>, group: string) {
        for (const [hash, task] of this.runningTasks) {
            if (task.group === group && !tasks.has(hash)) {
                task.abort().finally(() => {
                    this.runningTasks.delete(hash);
                });
            }

        }
        for (const [id,t] of tasks) {
            if(!this.runningTasks.has(id)){
                // If the task is not in the running tasks list, add it to the pending tasks list
                // This overwrites the old priority if it exists
                this.pendingTasks.set(id,t);
            }
        }
        for (const [id,t] of this.pendingTasks) {
            // If the task is not in the new tasks list, remove it from the pending tasks list
            if(t.group === group && !tasks.has(id)){
                this.pendingTasks.delete(id);
            }
        }
        this.reschedule();
    }

    reschedule() {
        while (this.runningTasks.size < this.concurrentTasks && this.pendingTasks.size > 0) {
            let taskToRunId: string | null = null;
            for(const [id,task] of this.pendingTasks){
                if(!taskToRunId || task.priority > (this.pendingTasks.get(taskToRunId)?.priority||0)){
                    taskToRunId = id;
                }
            }
            if (!taskToRunId) {
                break;
            }
            const taskToRun = this.pendingTasks.get(taskToRunId)!;
            this.pendingTasks.delete(taskToRunId);
            this.runningTasks.set(taskToRunId, taskToRun);
            if(taskToRun.timeoutSeconds){
                setTimeout(() => {
                    if(this.runningTasks.has(taskToRunId)){
                        taskToRun.abort().finally(() => {
                            this.runningTasks.delete(taskToRunId);
                            this.reschedule();
                        });
                    }
                },taskToRun.timeoutSeconds * 1000);
            }
            taskToRun.run().finally(() => {
                this.runningTasks.delete(taskToRunId);
                this.reschedule();
            });
        }
    }
    /**
     * Abort all running tasks
     */
    async abortAll() {
        for (const [id, task] of this.runningTasks) {
            await task.abort();
            this.runningTasks.delete(id);
        }
    }
}
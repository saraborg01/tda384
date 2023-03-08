package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    private static ConcurrentSkipListSet<Integer> concurrentVisited = new ConcurrentSkipListSet<>();
    private List<ForkJoinSolver> childSolvers = new ArrayList<>();
    private static AtomicBoolean finished = new AtomicBoolean();
    private static Maze staticMaze;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        staticMaze = maze;
        this.forkAfter = forkAfter;
    }

    /**
     * A constructor used for forking.
     * Creates a child solver that searches in staticMaze from the
     * start node to a goal. ForkAfter is ignored.
     *
     * @param start       the start node for this child thread.
     */
    public ForkJoinSolver( int start){
		this(staticMaze);
		this.start=start;
	}

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    /**
     * Steps through the maze.
     * Forks when more than one path is available.
     *
     * @return a list containing the path from start to goal, or null if no goal was found.
     */
    private List<Integer> parallelSearch()
    {
        // initiates a player on the maze on the start node
        int player = staticMaze.newPlayer(start);

        // start with starting node
        frontier.push(start);

        // explore all nodes in frontier or until goal has been found
        while(!frontier.empty() && !finished.get()){
            
            int current = frontier.pop();

            // if current node hasn't been explored, or if node is the starting node
            // (already checked for the starting node of new solvers)
            if (concurrentVisited.add(current) || current==start) {
                
                staticMaze.move(player, current);

                // if node is the goal
                if (maze.hasGoal(current)) {
                    // signals all ForkJoinSolvers that goal has been found
                    finished.compareAndSet(false, true);
                    // reconstructs and returns path taken.
                    return pathFromTo(start, current);
                }

                // checks all adjacent neigbors to the current node
                for (int nb: staticMaze.neighbors(current)){

                    // if neigbor hasn't been visited, the neighbor
                    // can then be reached from current node.
                    // And we add them to the frontier.
                    if (!concurrentVisited.contains(nb)){
                        predecessor.put(nb, current);
                        frontier.push(nb);
                    }
                }
            }

            // No need to spawn child if there is only one path to take.
            // Otherwise spawn a child solver for each adjacent path.
            if(frontier.size() > 1){
                while(!frontier.isEmpty()) {
                    int neighbor = frontier.pop();
                    spawnChild(neighbor);
                }
            }
            
        }

        // reached a dead end or no more nodes to explore or another solver has signaled 
        // that the goal has been found.
        return joinChildren();
    }

    /**
     * Spawns a child solver and forks them.
     * Only spawns if the starting node has not been visited yet.
     *
     * @param nb - The neighbor node that the child will spawn on
     */
    private void spawnChild(int nb) {
        if( concurrentVisited.add(nb)) {
            ForkJoinSolver child = new ForkJoinSolver(nb);
            childSolvers.add(child);
            child.fork();
        }
    }

    /**
     * Solver is done with their tasks. Their frontier is empty or
     * another ForkJoinSolver has signaled that they found the goal.
     * It iterates through their children and joins, to see if any of them 
     * has found the goal.
     * The reconstructed path is a joined path from each child that has contributed 
     * to have found the goal.
     * @return - result if the goal was found. Otherwise null.
     */
    private List<Integer> joinChildren() {
        for (ForkJoinSolver solver: childSolvers) {
            List<Integer> result = solver.join();
            if(result!=null) {
                List<Integer> path = pathFromTo(start, predecessor.get(solver.start));
                path.addAll(result);
                return path;
            }
        }
        return null;
    }
}


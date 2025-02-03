package spatial.nodes;

import spatial.exceptions.UnimplementedMethodException;
import spatial.kdpoint.KDPoint;
import spatial.knnutils.BoundedPriorityQueue;
import spatial.knnutils.NNData;

import java.util.Collection;

/**
 * <p>{@link KDTreeNode} is an abstraction over nodes of a KD-Tree. It is used extensively by
 * {@link spatial.trees.KDTree} to implement its functionality.</p>
 *
 * <p><b>YOU ***** MUST ***** IMPLEMENT THIS CLASS!</b></p>
 *
 * @author  ---- YOUR NAME HERE! -----
 *
 * @see spatial.trees.KDTree
 */
public class KDTreeNode {


    /* *************************************************************************** */
    /* ************* WE PROVIDE THESE FIELDS TO GET YOU STARTED.  **************** */
    /* ************************************************************************** */
    private KDPoint p;
    private int height;
    private KDTreeNode left, right;

    /* *************************************************************************************** */
    /* *************  PLACE ANY OTHER PRIVATE FIELDS AND YOUR PRIVATE METHODS HERE: ************ */
    /* ************************************************************************************* */


    /* *********************************************************************** */
    /* ***************  IMPLEMENT THE FOLLOWING PUBLIC METHODS:  ************ */
    /* *********************************************************************** */


    /**
     * 1-arg constructor. Stores the provided {@link KDPoint} inside the freshly created node.
     * @param p The {@link KDPoint} to store inside this. Just a reminder: {@link KDPoint}s are
     *          <b>mutable!!!</b>.
     */
    public KDTreeNode(KDPoint p){
        this.p = p;
    }

    /**
     * <p>Inserts the provided {@link KDPoint} in the tree rooted at this. To select which subtree to recurse to,
     * the KD-Tree acts as a Binary Search Tree on currDim; it will examine the value of the provided {@link KDPoint}
     * at currDim and determine whether it is larger than or equal to the contained {@link KDPoint}'s relevant dimension
     * value. If so, we recurse right, like a regular BST, otherwise left.</p>
     * @param currDim The current dimension to consider
     * @param dims The total number of dimensions that the space considers.
     * @param pIn The {@link KDPoint} to insert into the node.
     * @see #delete(KDPoint, int, int)
     */
    public  void insert(KDPoint pIn, int currDim, int dims){
        insert_aux(pIn, this, 0, dims);
    }
    
    private void insert_aux(KDPoint pIn, KDTreeNode curr, int currDim, int dims) {
    	//if current dimension is greater than or equal to dims, reset to wrap around to 0
    	if (currDim >= dims) {
    		currDim = 0;
    	}
    
    	//determine which way to go
    	//if current node coord val is less than point to insert, GO RIGHT
    	//if (curr.p.coords[currDim] < pIn.coords[currDim]) {
    	if (pIn.coords[currDim] >= curr.p.coords[currDim]) {
    		//System.out.println("DIM: " + currDim+" " +curr.p.coords[currDim] + " is less than " +  pIn.coords[currDim]);
    		//check to see if right node is null
    		if (curr.right == null) {
    			//if it is null, make a new node
    			KDTreeNode x = new KDTreeNode(pIn);
    			curr.right = x;
    			return;
    		}
    		//go to right subtree
    		else {    			    			
    			insert_aux(pIn, curr.right, currDim += 1, dims);
    		}
    	}
    	//GO LEFT
    	else {
    		
    		if (curr.left == null) {
    			KDTreeNode x = new KDTreeNode(pIn);
    			curr.left = x;
    			return;
    		}
    		else {
    			insert_aux(pIn, curr.left, currDim += 1, dims);
    		}
    	}
    }
    

    /**
     * <p>Deletes the provided {@link KDPoint} from the tree rooted at this. To select which subtree to recurse to,
     * the KD-Tree acts as a Binary Search Tree on currDim; it will examine the value of the provided {@link KDPoint}
     * at currDim and determine whether it is larger than or equal to the contained {@link KDPoint}'s relevant dimension
     * value. If so, we recurse right, like a regular BST, otherwise left. There exist two special cases of deletion,
     * depending on whether we are deleting a {@link KDPoint} from a node who either:</p>
     *
     * <ul>
     *      <li>Has a NON-null subtree as a right child.</li>
     *      <li>Has a NULL subtree as a right child.</li>
     * </ul>
     *
     * <p>You should consult the class slides, your notes, and the textbook about what you need to do in those two
     * special cases.</p>
     * @param currDim The current dimension to consider.
     * @param dims The total number of dimensions that the space considers.
     * @param pIn The {@link KDPoint} to delete from the node.
     * @see #insert(KDPoint, int, int)
     * @return A reference to this after the deletion takes place.
     */
    public KDTreeNode delete(KDPoint pIn, int currDim, int dims){
    	//search is already done in method calling delete
    	//iif our root equals point to delete, then look at 2 cases
    	if (this.p.equals(pIn)) {
    		//if both sub trees are null, then just return null
    		if (left == null && right == null) {
    			return null;
    		}
    		//if both nodes aren't null, then one or the other is null or neither are null
    		else {
    			//a bit more complex here...DO DELETION HERE
    			if (right != null) {
    				//non null right tree so replace with INORDER successor
    				//get inorder successor
    				
    				//KDPoint pn = dim_minimum(right.p, right, currDim);
    				KDPoint pn = (dim_minimum2( right, currDim)).p;
    				//replace our point with inorder successor
    				this.p = pn;
    				//recursively delete point
    				right = delete_aux(pn, right, currDim + 1, dims);
    				
    			} else {
    				//left deletion case...right subtree is null but left subtree is not null
    				//KDPoint min = dim_minimum(left.p, left, currDim);
    				KDPoint min = (dim_minimum2(left, currDim)).p;
    				//System.out.println("Minimum point is" + min);
    				this.p = min;
    				//right = left;
    				//left = null;
    				right = delete_aux(min, right, currDim + 1, dims);	
    			}
    			return this;
    		}
    	}
    	else {
    		//if our main point is not equal, then we have to determine which way to go
    		
    		//if our point is less than point we want to delete, then go right
    		//if (this.p.coords[currDim] < pIn.coords[currDim]) {
    		if (pIn.coords[currDim] >= this.p.coords[currDim]) {
    			right = delete_aux(pIn, right, currDim + 1, dims);
    		} 
    		//else go left
    		else {
    			left = delete_aux(pIn, left, currDim + 1, dims);
    		}
    		return this;
    	}
    }
    
    private KDTreeNode delete_aux(KDPoint pIn, KDTreeNode curr, int currDim, int dims) {
    	if (curr == null) {
    		return null;
    	}
    	//first check to see if we have to wrap around with our dimension
    	if (currDim >= dims) {
    		currDim = 0;
    	}
    	//now check to see if we are at node that has to be deleted
    	if (curr.p.equals(pIn)) {
    		//if we are in here, then that means we are at node to be deleted
    		//worry about 3 cases here
    		//1. easiest case, both subtrees are null so just return null
    		if (curr.left == null && curr.right == null) {
        		return null;
        	}
    		//2. right non null case, if right subtree is not null, replace with inorder successor
    		//and then recurisvely delete the other node
        	if (curr.right != null) {
        		//KDPoint inorder_successor = inorder_successor(curr.p, curr.right.p, curr.right, currDim);
        		//KDPoint inorder_successor = dim_minimum(curr.right.p, curr.right, currDim);
        		
        		KDPoint inorder_successor = (dim_minimum2( curr.right, currDim)).p;
        		curr.p = inorder_successor;
        		//MIGHT HAVE TO DEBUG HERE <-------------------------------------------------------
        		curr.right = delete_aux(inorder_successor, curr.right, currDim + 1, dims);
        	}
        	//3 case where we have non-null left sub tree but we have null right sub tree
        	else {
        		//left deletion case...right subtree is null but left subtree is not null
				//KDPoint min = dim_minimum(curr.left.p, curr.left, currDim);
        		KDPoint min = (dim_minimum2( curr.left, currDim)).p;
        		
				curr.p = min;
				//curr.right = curr.left;
				//curr.left = null;
				curr.right = delete_aux(min, curr.right, currDim + 1, dims);
        	}
    	}
    	else {
    		//determine which way to go to find node to delete
    		//if (curr.p.coords[currDim] < pIn.coords[currDim]) {
    		if (pIn.coords[currDim] >= curr.p.coords[currDim]) {
    			//go right
    			//System.out.println("going right");
    			curr.right = delete_aux(pIn, curr.right, currDim + 1, dims);
    		}
    		else {
    			//go left
    			//System.out.println("going left " + curr.p);
    			curr.left = delete_aux(pIn, curr.left, currDim + 1, dims);    			
    		}
    	}
    	return curr;
    }
    private KDTreeNode dim_minimum2(KDTreeNode curr_node, int dim) {
    	if (curr_node == null) {
    		return null;
    	}
    	if (curr_node.left == null && curr_node.right == null) {
    		return curr_node;
    	}
    	KDTreeNode left = dim_minimum2(curr_node.left, dim);
    	KDTreeNode right = dim_minimum2(curr_node.right, dim);
    	
    	return min3(left, right, curr_node, dim);
    }
    private KDTreeNode min3(KDTreeNode left, KDTreeNode right, KDTreeNode middle, int dim) {
    	//need to handle null cases here...
    	KDTreeNode ret = middle;
    	if (right != null && right.p.coords[dim] <= ret.p.coords[dim]) {
    		
    		ret = right;
    	}
    	if (left != null && left.p.coords[dim] <= ret.p.coords[dim]) {
    		
    		ret = left;
    	}
    	return ret;
    }
    
    /**
     * Searches the subtree rooted at the current node for the provided {@link KDPoint}.
     * @param pIn The {@link KDPoint} to search for.
     * @param currDim The current dimension considered.
     * @param dims The total number of dimensions considered.
     * @return true iff pIn was found in the subtree rooted at this, false otherwise.
     */
    public  boolean search(KDPoint pIn, int currDim, int dims){
        return search_aux(pIn, this, currDim, dims);
    }

    private boolean search_aux(KDPoint pIn, KDTreeNode curr, int currDim, int dims) {
    	//if we reach null, our base case null, then we have failed to find point in our tree
    	if (curr == null) {
    		return false;
    	}
    	//else if we are not null, then check for 2 things
    	else {
    		//first check to see if current node point is equal to search point
    		if (curr.p.equals(pIn)) {
    			return true;
    		}
    		//if we are not equal, then we have to see which way to go
    		else {
    			//check to see if current dimension needs to wrap around back to start
    			if (currDim >= dims) {
    				currDim = 0;
    			}
    			//then compare to see if we have to go to the left or right
    			//if (curr.p.coords[currDim] < pIn.coords[currDim]) {
    			if (pIn.coords[currDim] >= curr.p.coords[currDim]) {	
    				//if our current node is less than the node to check, then go right
    				return search_aux(pIn, curr.right, currDim + 1, dims);
    			}
    			else {
    				//if not less, than go left if equal to or greater
    				return search_aux(pIn, curr.left, currDim + 1, dims);
    			}
    		}
    	}
    }
    /**
     * <p>Executes a range query in the given {@link KDTreeNode}. Given an &quot;anchor&quot; {@link KDPoint},
     * all {@link KDPoint}s that have a {@link KDPoint#euclideanDistance(KDPoint) euclideanDistance} of <b>at most</b> range
     * <b>INCLUSIVE</b> from the anchor point <b>except</b> for the anchor itself should be inserted into the {@link Collection}
     * that is passed.</p>
     *
     * <p>Remember: range queries behave <em>greedily</em> as we go down (approaching the anchor as &quot;fast&quot;
     * as our currDim allows and <em>prune subtrees</em> that we <b>don't</b> have to visit as we backtrack. Consult
     * all of our resources if you need a reminder of how these should work.</p>
     *
     * @param anchor The centroid of the hypersphere that the range query implicitly creates.
     * @param results A {@link Collection} that accumulates all the {@link }
     * @param currDim The current dimension examined by the {@link KDTreeNode}.
     * @param dims The total number of dimensions of our {@link KDPoint}s.
     * @param range The <b>INCLUSIVE</b> range from the &quot;anchor&quot; {@link KDPoint}, within which all the
     *              {@link KDPoint}s that satisfy our query will fall. The euclideanDistance metric used} is defined by
     *              {@link KDPoint#euclideanDistance(KDPoint)}.
     */
    //revised idea:
    //so, we start off at some node x, and have a current dimension y we're on
    //we are doing something similar to a greedy algorithm, so we see 
    //if our anchor point is to our left or to our right, and visit the corresponding 
    //side first that the anchor point falls on, then the other side
    //but maybe before visiting some points, we might also have to take into account 
    //any pruning that we might have to do at a current node
    //if we are at current node x, and at dimension y, and if 
    //our current is outside the range of the corresponding y dimension on anchor point
    //we don't visit nodes that go in that direction because they won't be within range
    public void range(KDPoint anchor, Collection<KDPoint> results,
                      double range, int currDim , int dims){
        //we start at this root
    	//let's first check to see if it's within euclidian distance of anchor
    	double euc_dist = anchor.euclideanDistance(this.p);
    	//if it's within range, then let's add it to collection
    	if (euc_dist <= range) {
    		results.add(this.p);
    	}
    	//let's see which way we should go
    	
    	if (this.p.coords[currDim] < anchor.coords[currDim]) {
    		//if our coord is less than the anchor coords at currDim, then we have to go right first, then left
    		//go right first
    		results = range_aux(anchor, results, range, currDim + 1, dims, right);
    		
    		//then go left, but check to see if we have to prune first
    		//idea for pruning, we have anchor point and we have range, and current point
    		//get distance at currdim from anchor point to current point
    		double dist = Math.abs(Math.abs(this.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    		//if the distance from the anchor point dim to our current point dim is less than 
    		//or equal to the range, then we will visit the tree on the left and don't prune
    		if (dist <= range) {
    			results = range_aux(anchor, results, range, currDim + 1, dims, left);
    		} 
    		
    		//if we didn't enter previous branch then we pruned
    	}
    	else {
    		//if our coord is not strictly less than, it must be greater than or equal to, so go left first
    		
    		//go left first
    		results = range_aux(anchor, results, range, currDim + 1, dims, left);
    		//then go right
    		
    		double dist = Math.abs(Math.abs(this.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    		if (dist <= range) {
    			results = range_aux(anchor, results, range, currDim + 1, dims, right);
    		}
    	}
    }
    private Collection<KDPoint> range_aux(KDPoint anchor, Collection<KDPoint> results,
    					double range, int currDim, int dims, KDTreeNode curr){
    	//if we reach our base case of null, just return whatever we have in results
    	if (curr == null) {
    		return results;
    	}
    	//if not null, then we are at some valid node
    	//first check to see if currDim needs to wrap around
    	if (currDim >= dims) {
    		currDim = 0;
    	}
    	//now check to see if point is within range
    	double euc_dist = anchor.euclideanDistance(curr.p);
    	if (euc_dist <= range) {
    		//if within range, add to collection
    		results.add(curr.p);
    	}
    	//now let's see which way to go
    	if (curr.p.coords[currDim] < anchor.coords[currDim]) {
    		//go right first then left
    		results = range_aux(anchor, results, range, currDim + 1, dims, curr.right);
    		double dist = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    		//if the distance from the anchor point dim to our current point dim is less than 
    		//or equal to the range, then we will visit the tree on the left and don't prune
    		if (dist <= range) {
    			results = range_aux(anchor, results, range, currDim + 1, dims, curr.left);
    		} 
    		
    	}
    	else {
    		//go left first then right
    		results = range_aux(anchor, results, range, currDim + 1, dims, curr.left);
    		double dist = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    		//if the distance from the anchor point dim to our current point dim is less than 
    		//or equal to the range, then we will visit the tree on the left and don't prune
    		if (dist <= range) {
    			results = range_aux(anchor, results, range, currDim + 1, dims, curr.right);
    		} 
    	}
    	
    	return results;
    }
    

    /**
     * <p>Executes a nearest neighbor query, which returns the nearest neighbor, in terms of
     * {@link KDPoint#euclideanDistance(KDPoint)}, from the &quot;anchor&quot; point.</p>
     *
     * <p>Recall that, in the descending phase, a NN query behaves <em>greedily</em>, approaching our
     * &quot;anchor&quot; point as fast as currDim allows. While doing so, it implicitly
     * <b>bounds</b> the acceptable solutions under the current <b>best solution</b>, which is passed as
     * an argument. This approach is known in Computer Science as &quot;branch-and-bound&quot; and it helps us solve an
     * otherwise exponential complexity problem (nearest neighbors) efficiently. Remember that when we want to determine
     * if we need to recurse to a different subtree, it is <b>necessary</b> to compare the euclideanDistance reported by
     * {@link KDPoint#euclideanDistance(KDPoint)} and coordinate differences! Those are comparable with each other because they
     * are the same data type ({@link Double}).</p>
     *
     * @return An object of type {@link NNData}, which exposes the pair (distance_of_NN_from_anchor, NN),
     * where NN is the nearest {@link KDPoint} to the anchor {@link KDPoint} that we found.
     *
     * @param anchor The &quot;ancor&quot; {@link KDPoint}of the nearest neighbor query.
     * @param currDim The current dimension considered.
     * @param dims The total number of dimensions considered.
     * @param n An object of type {@link NNData}, which will define a nearest neighbor as a pair (distance_of_NN_from_anchor, NN),
     *      * where NN is the nearest neighbor found.
     *
     * @see NNData
     * @see #kNearestNeighbors(int, KDPoint, BoundedPriorityQueue, int, int)
     */
    //so we still do pruning here
    //idea, update our point and see which way to go
    public  NNData<KDPoint> nearestNeighbor(KDPoint anchor, int currDim,
                                            NNData<KDPoint> n, int dims){
    	//first let's update our best guess to our root
    	double best_dist = anchor.euclideanDistance(this.p);
    	if (best_dist != 0) {
    		n.update(this.p, best_dist);
    	}
    	//we should have updated our nn data, unless we were given the exact same point
    	//now we check which way our anchor lies
    	if (this.p.coords[currDim] < anchor.coords[currDim]) {
    		//if our dimension is less than anchor, anchor is on right so go right
    		n = nearestNeighbor_aux(anchor, right, currDim + 1, n, dims);
    		//check to see if we have to prune left
    		double new_range = n.getBestDist();
    		//System.out.println("entering this branch, go right tree");
    		double dist = Math.abs(Math.abs(this.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    		if (dist < new_range || new_range <= 0) {
    			n = nearestNeighbor_aux(anchor, left, currDim + 1, n, dims);
    		}
    	}
    	else {
    		//our current point is greater than or equal to, so go left first
    		n = nearestNeighbor_aux(anchor, left, currDim + 1, n, dims);
    		//System.out.println("entering this branch, go left tree");
    		//check to see if we have to prune right
    		double new_range = n.getBestDist();
    		double dist = Math.abs(Math.abs(this.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    		if (dist < new_range || new_range <= 0) {
    			//System.out.println("right should not be pruned");
    			n = nearestNeighbor_aux(anchor, right, currDim + 1, n, dims);
    		}
    	}
    	return n;
    }
    
    private NNData<KDPoint> nearestNeighbor_aux(KDPoint anchor, KDTreeNode curr, int currDim, 
    								NNData<KDPoint> n, int dims){
    	//if we reached our base case of null, return nndata
    	if (curr == null) {
    		return n;
    	} 
    	else {
    		//wrap around currDim if necessary
    		if (currDim >= dims) {
    			currDim = 0;
    		}
    		//get distance from anchor to our current point
    		double dist = anchor.euclideanDistance(curr.p);
    		if (dist < n.getBestDist() && dist != 0 || n.getBestDist() <= 0) {
    			//if the distance is smaller than our best, then just update it
    			n.update(curr.p, dist);
    		}
    		//now see which way we have to go
    		if (curr.p.coords[currDim] < anchor.coords[currDim]) {
    			//if we are less than, go right first
    			n = nearestNeighbor_aux(anchor, curr.right, currDim + 1, n, dims);
    			double new_range = n.getBestDist();
        		double ndist = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
        		if (dist < new_range) {
        			n = nearestNeighbor_aux(anchor, curr.left, currDim + 1, n, dims);
        		}
    			
    		}
    		else {
    			//otherwise if we're greater than or equal to, go left first
    			n = nearestNeighbor_aux(anchor, curr.left, currDim + 1, n, dims);
    			double new_range = n.getBestDist();
        		double ndist = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
        		if (dist < new_range) {
        			n = nearestNeighbor_aux(anchor, curr.right, currDim + 1, n, dims);
        		}
    		}
    		return n;
    	}
    }

    /**
     * <p>Executes a nearest neighbor query, which returns the nearest neighbor, in terms of
     * {@link KDPoint#euclideanDistance(KDPoint)}, from the &quot;anchor&quot; point.</p>
     *
     * <p>Recall that, in the descending phase, a NN query behaves <em>greedily</em>, approaching our
     * &quot;anchor&quot; point as fast as currDim allows. While doing so, it implicitly
     * <b>bounds</b> the acceptable solutions under the current <b>worst solution</b>, which is maintained as the
     * last element of the provided {@link BoundedPriorityQueue}. This is another instance of &quot;branch-and-bound&quot;
     * Remember that when we want to determine if we need to recurse to a different subtree, it is <b>necessary</b>
     * to compare the euclideanDistance reported by* {@link KDPoint#euclideanDistance(KDPoint)} and coordinate differences!
     * Those are comparable with each other because they are the same data type ({@link Double}).</p>
     *
     * <p>The main difference of the implementation of this method and the implementation of
     * {@link #nearestNeighbor(KDPoint, int, NNData, int)} is the necessity of using the class
     * {@link BoundedPriorityQueue} effectively. Consult your various resources
     * to understand how you should be using this class.</p>
     *
     * @param k The total number of neighbors to retrieve. It is better if this quantity is an odd number, to
     *          avoid ties in Binary Classification tasks.
     * @param anchor The &quot;anchor&quot; {@link KDPoint} of the nearest neighbor query.
     * @param currDim The current dimension considered.
     * @param dims The total number of dimensions considered.
     * @param queue A {@link BoundedPriorityQueue} that will maintain at most k nearest neighbors of
     *              the anchor point at all times, sorted by euclideanDistance to the point.
     *
     * @see BoundedPriorityQueue
     */
    //so base case idea is this
    //we check to see if our root can be added to queue or not, if it can be added, then add it
    //otherwise, don't add it and follow greedy algorithm to determine which way to go first
    public  void kNearestNeighbors(int k, KDPoint anchor, BoundedPriorityQueue<KDPoint> queue, int currDim, int dims){
        //first check to see if this point has a distance greater than 0 to anchor
    	double dist = anchor.euclideanDistance(this.p);
    	if (dist > 0) {
    		//distance will be priority, smaller distances get higher priority and are moved to the front
    		queue.enqueue(this.p, dist);
    	}
    	//just a quick test to see something
    	//this is with no pruning, need to rework it
    	kNNAux2(k, anchor, queue, currDim, dims, left);
    	kNNAux2(k, anchor, queue, currDim, dims, right);
    	/*
    	//now let's decide which way to go first based on where anchor is
    	if (anchor.coords[currDim] >= this.p.coords[currDim]) {
    	//if (this.p.coords[currDim] < anchor.coords[currDim]) {
    		//if our point is less than anchor, then anchor is going to be in right sub tree so go
    		//there first
    		//go right
    		kNNAux(k, anchor, queue, currDim + 1, dims, right);
    		//then check to see if we have to go left, to prune or not
    		//idea of pruning here, two cases
    		if (queue.size() == k) {
    			//if queue is full, then check to see if we have to prune
    			//need to get last element in priority queue to get biggest distance
    			double new_range = queue.getLastPriority();	
    			//now calculate distance from our point to anchor
    			double dist2 = Math.abs(Math.abs(this.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    			//testing with no pruning
    			//if (dist2 < new_range || new_range < 0) {
    				kNNAux(k, anchor, queue, currDim + 1, dims, left);
    			//}
    			//if we didn't enter previous branch, then it was pruned
    		} 
    		else {
    			//if queue is not null, then just check left anyways to see if we can add more 
    			//points to our nn queue
    			kNNAux(k, anchor, queue, currDim + 1, dims, left);
    		}
    	}
    	else {
    		//go left first
    		kNNAux(k, anchor, queue, currDim + 1, dims, left);
    		//check to see if we have to prune right
    		if (queue.size() == k) {
    			//if queue is full, then check to see if we have to prune
    			//need to get last element in priority queue to get biggest distance
    			double new_range = queue.getLastPriority();	
    			//now calculate distance from our point to anchor
    			double dist2 = Math.abs(Math.abs(this.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    			//if (dist2 < new_range || new_range < 0) {
    				kNNAux(k, anchor, queue, currDim + 1, dims, right);
    			//}
    			//if we didn't enter previous branch, then it was pruned
    		} 
    		else {
    			//if queue is not null, then just check right anyways to see if we can add more 
    			//points to our nn queue
    			kNNAux(k, anchor, queue, currDim + 1, dims, right);
    		}
    	}
    	*/
    }

    private void kNNAux2(int k, KDPoint anchor, BoundedPriorityQueue<KDPoint> queue, int currDim, int dims,
			KDTreeNode curr) {
    	if (curr != null) {
    		double dist = anchor.euclideanDistance(curr.p);
    		if (dist > 0) {
    			queue.enqueue(curr.p, dist);
    		}
    		//this is with no pruning
    		kNNAux2(k, anchor, queue, currDim, dims, curr.left);
        	kNNAux2(k, anchor, queue, currDim, dims, curr.right);
    	}
    }
    
    private void kNNAux(int k, KDPoint anchor, BoundedPriorityQueue<KDPoint> queue, int currDim, int dims,
    			KDTreeNode curr) {
    	//rewrap currDim
    	if (currDim >= dims) {
    		currDim = 0;
    	}
    	//ok so now we are here, first thing to do is to check to see if we are NOT at a null node
    	if (curr != null) {
    		//if our node is not a null node, then we have not reached base case
    		//now there are two cases here, we either have a queue that is full or not full 
    		if (k == queue.size()) {
    			//here we are full, so we need to see if we have to eject the last element
    			double new_range = queue.getLastPriority();				//get last distance
    			double curr_dist = anchor.euclideanDistance(curr.p);	//get current distance
    			//if the current point is closer, enqueue it
    			if (curr_dist < new_range) {
    				queue.enqueue(curr.p, curr_dist);					//push into queue and then eject other
    			}
    			//now determine which way to go
    			if (anchor.coords[currDim] >= curr.p.coords[currDim]) {
    			//if (curr.p.coords[currDim] < anchor.coords[currDim]) {
    				//go right first
    				kNNAux(k, anchor, queue, currDim + 1, dims, curr.right);
    				//check to see if we have to prune left
    				double new_range2 = queue.getLastPriority();
    				double dist2 = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    				//if (dist2 < new_range2 || new_range2 < 0) {
    					kNNAux(k, anchor, queue, currDim + 1, dims, curr.left);
    				//}
    			}
    			else {
    				//go left first
    				kNNAux(k, anchor, queue, currDim + 1, dims, curr.left);
    				//check to see if we have to prune right
    				double new_range2 = queue.getLastPriority();
    				double dist2 = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    				//if (dist2 < new_range2 || new_range2 < 0) {
    					kNNAux(k, anchor, queue, currDim + 1, dims, curr.right);
    				//}
    			}
    			
    			
    		}
    		else {
    			//here queue is not full, so we just add the point that we landed on
    			double dist = anchor.euclideanDistance(curr.p);
    			queue.enqueue(curr.p, dist);
    			//now determine which way to go
    			if (anchor.coords[currDim] >= curr.p.coords[currDim]) {
    			//if (curr.p.coords[currDim] < anchor.coords[currDim]) {
    				//go right first
    				kNNAux(k, anchor, queue, currDim + 1, dims, curr.right);
    				//check to see if we have to prune left
    				double new_range = queue.getLastPriority();
    				double dist2 = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    				//if (dist2 < new_range || new_range < 0) {
    					kNNAux(k, anchor, queue, currDim + 1, dims, curr.left);
    				//}
    			} 
    			else {
    				//go left first
    				kNNAux(k, anchor, queue, currDim + 1, dims, curr.left);
    				//check to see if we have to prune right
    				double new_range = queue.getLastPriority();
    				double dist2 = Math.abs(Math.abs(curr.p.coords[currDim])  - Math.abs(anchor.coords[currDim]));
    				//if (dist2 < new_range || new_range < 0) {
    					kNNAux(k, anchor, queue, currDim + 1, dims, curr.right);
    				//}
    			}
    		}
    	}
    }
    /**
     * Returns the height of the subtree rooted at the current node. Recall our definition of height for binary trees:
     * <ol>
     *     <li>A null tree has a height of -1.</li>
     *     <li>A non-null tree has a height equal to max(height(left_subtree), height(right_subtree))+1</li>
     * </ol>
     * @return the height of the subtree rooted at the current node.
     */
    public int height(){
        if (left == null && right == null) {
        	return 0;
        }
        else {
        	/*
        	if ((gheight (left)) > (gheight(right))) {
        		return  gheight(left);
        	}
        	else {
        		return  gheight(right);
        	}
        	*/
        	//error has to be here somewhere in calculating height
        	return max(gheight(left), gheight(right));
        }
    }
    
    private int gheight(KDTreeNode x) {
    	if (x == null) {
    		return 0;
    	}
    	else {
    		return 1 + max((gheight(x.left)), (gheight(x.right)));
    	}
    }
    private int max (int x, int y) {
    	if (x > y) {
    		return x;
    	} else {
    		return y;
    	}
    }
    /**
     * A simple getter for the {@link KDPoint} held by the current node. Remember: {@link KDPoint}s ARE
     * MUTABLE, SO WE NEED TO DO DEEP COPIES!!!
     * @return The {@link KDPoint} held inside this.
     */
    public KDPoint getPoint(){
        return p;
    }

    public KDTreeNode getLeft(){
        return left;
    }

    public KDTreeNode getRight(){
        return right;
    }
}
package spatial.nodes;

import spatial.exceptions.UnimplementedMethodException;
import spatial.kdpoint.KDPoint;
import spatial.knnutils.BoundedPriorityQueue;
import spatial.knnutils.NNData;
import spatial.trees.CentroidAccuracyException;
import spatial.trees.PRQuadTree;

import java.util.Collection;
import java.util.LinkedList;

/** <p>A {@link PRQuadGrayNode} is a gray (&quot;mixed&quot;) {@link PRQuadNode}. It
 * maintains the following invariants: </p>
 * <ul>
 *      <li>Its children pointer buffer is non-null and has a length of 4.</li>
 *      <li>If there is at least one black node child, the total number of {@link KDPoint}s stored
 *      by <b>all</b> of the children is greater than the bucketing parameter (because if it is equal to it
 *      or smaller, we can prune the node.</li>
 * </ul>
 *
 * <p><b>YOU ***** MUST ***** IMPLEMENT THIS CLASS!</b></p>
 *
 *  @author --- YOUR NAME HERE! ---
 */
public class PRQuadGrayNode extends PRQuadNode{


    /* ******************************************************************** */
    /* *************  PLACE ANY  PRIVATE FIELDS AND METHODS HERE: ************ */
    /* ********************************************************************** */
	//so we want a reference to 4 nodes
	PRQuadNode northwest;
	PRQuadNode northeast;
	PRQuadNode southwest;
	PRQuadNode southeast;
    /* *********************************************************************** */
    /* ***************  IMPLEMENT THE FOLLOWING PUBLIC METHODS:  ************ */
    /* *********************************************************************** */

    /**
     * Creates a {@link PRQuadGrayNode}  with the provided {@link KDPoint} as a centroid;
     * @param centroid A {@link KDPoint} that will act as the centroid of the space spanned by the current
     *                 node.
     * @param k The See {@link PRQuadTree#PRQuadTree(int, int)} for more information on how this parameter works.
     * @param bucketingParam The bucketing parameter fed to this by {@link PRQuadTree}.
     * @see PRQuadTree#PRQuadTree(int, int)
     */
    public PRQuadGrayNode(KDPoint centroid, int k, int bucketingParam){
        super(centroid, k, bucketingParam); // Call to the super class' protected constructor to properly initialize the object!
        northwest = null;
        northeast = null;
        southwest = null;
        southeast = null;
    }


    /**
     * <p>Insertion into a {@link PRQuadGrayNode} consists of navigating to the appropriate child
     * and recursively inserting elements into it. If the child is a white node, memory should be allocated for a
     * {@link PRQuadBlackNode} which will contain the provided {@link KDPoint} If it's a {@link PRQuadBlackNode},
     * refer to {@link PRQuadBlackNode#insert(KDPoint, int)} for details on how the insertion is performed. If it's a {@link PRQuadGrayNode},
     * the current method would be called recursively. Polymorphism will allow for the appropriate insert to be called
     * based on the child object's runtime object.</p>
     * @param p A {@link KDPoint} to insert into the subtree rooted at the current {@link PRQuadGrayNode}.
     * @param k The side length of the quadrant spanned by the <b>current</b> {@link PRQuadGrayNode}. It will need to be updated
     *          per recursive call to help guide the input {@link KDPoint}  to the appropriate subtree.
     * @return The subtree rooted at the current node, potentially adjusted after insertion.
     * @see PRQuadBlackNode#insert(KDPoint, int)
     */
    @Override
    public PRQuadNode insert(KDPoint p, int k) {
    	if (k <= -1) {
    		throw new CentroidAccuracyException("gray node too small");
    	}
    	//ok so determine which way to go
    	//first determine whether to go left or right
    	if (p.coords[0] < centroid.coords[0]) {
    		//if our point is STRICTLY less then we GO LEFT...WEST
    		if (p.coords[1] < centroid.coords[1]) {
    			//if our second point IS STRICTLY less, then we GO DOWN...SOUTH
    			//now check to see if node doesn't exist, if it doesn't, create it
    			if (southwest == null) {
    				int x = centroid.coords[0] - (int) Math.pow(2, this.k - 1);
    				int y = centroid.coords[1] - (int) Math.pow(2, this.k - 1);
    				southwest = new PRQuadBlackNode(new KDPoint(x, y), k - 1, bucketingParam);
    			}
    			southwest = southwest.insert(p, k - 1);
    		}
    		else {
    			//if our second point is not smaller, then we GO UP...NORTH
    			//now check to see if node doesn't exist, if it doesn't, create it
    			if (northwest == null) {
    				int x = centroid.coords[0] - (int) Math.pow(2, this.k - 1);
    				int y = centroid.coords[1] + (int) Math.pow(2, this.k - 1);
    				northwest = new PRQuadBlackNode(new KDPoint(x, y), k - 1, bucketingParam);
    			}
    			northwest = northwest.insert(p,  k - 1);
    		}
    	}
    	else {
    		//otherwise we go RIGHT...EAST
    		if (p.coords[1] < centroid.coords[1]) {
    			//go SOUTH
    			if (southeast == null) {
    				int x = centroid.coords[0] + (int) Math.pow(2, this.k - 1);
    				int y = centroid.coords[1] - (int) Math.pow(2, this.k - 1);
    				southeast = new PRQuadBlackNode(new KDPoint(x, y), k - 1, bucketingParam);
    			}
    			southeast = southeast.insert(p, k - 1);
    		}
    		else {
    			//go NORTH
    			if (northeast == null) {
    				int x = centroid.coords[0] + (int) Math.pow(2, this.k - 1);
    				int y = centroid.coords[1] + (int) Math.pow(2, this.k - 1);
    				northeast = new PRQuadBlackNode(new KDPoint(x, y), k - 1, bucketingParam);
    			}
    			northeast = northeast.insert(p, k - 1);
    		}
    	}
        return this;
    }

    /**
     * <p>Deleting a {@link KDPoint} from a {@link PRQuadGrayNode} consists of recursing to the appropriate
     * {@link PRQuadBlackNode} child to find the provided {@link KDPoint}. If no such child exists, the search has
     * <b>necessarily failed</b>; <b>no changes should then be made to the subtree rooted at the current node!</b></p>
     *
     * <p>Polymorphism will allow for the recursive call to be made into the appropriate delete method.
     * Importantly, after the recursive deletion call, it needs to be determined if the current {@link PRQuadGrayNode}
     * needs to be collapsed into a {@link PRQuadBlackNode}. This can only happen if it has no gray children, and one of the
     * following two conditions are satisfied:</p>
     *
     * <ol>
     *     <li>The deletion left it with a single black child. Then, there is no reason to further subdivide the quadrant,
     *     and we can replace this with a {@link PRQuadBlackNode} that contains the {@link KDPoint}s that the single
     *     black child contains.</li>
     *     <li>After the deletion, the <b>total</b> number of {@link KDPoint}s contained by <b>all</b> the black children
     *     is <b>equal to or smaller than</b> the bucketing parameter. We can then similarly replace this with a
     *     {@link PRQuadBlackNode} over the {@link KDPoint}s contained by the black children.</li>
     *  </ol>
     *
     * @param p A {@link KDPoint} to delete from the tree rooted at the current node.
     * @return The subtree rooted at the current node, potentially adjusted after deletion.
     */
    @Override
    public PRQuadNode delete(KDPoint p) {
    	
        //ok, so we start at our grey node and determine which way to go
    	if (p.coords[0] < centroid.coords[0]) {
    		//go west
    		if (p.coords[1] < centroid.coords[1]) {
    			//go south
    			if (southwest != null) {
    				southwest = southwest.delete(p);
    			}
    		}
    		else {
    			//go north
    			if (northwest != null) {
    				northwest = northwest.delete(p);
    			}
    		}
    	}
    	else {
    		//go east
    		if (p.coords[1] < centroid.coords[1]) {
    			//go south
    			if (southeast != null) {
    				southeast = southeast.delete(p);
    			}
    		}
    		else {
    			//go north
    			if (northeast != null) {
    				northeast = northeast.delete(p);
    			}
    			
    		}
    	}
    	//maybe buggy below this point
    	//check to see if we have one black node and 3 white nodes
    	//might be buggy because what if we just return a gray node with wrong centroid?
    	//BUG WITH CENTROID...HAVE TO RETURN THE RIGHT CENTROID WHEN DELETING...maybe fixed
    	if (northwest != null && northeast == null && southwest == null && southeast == null) {
    		northwest.centroid = this.centroid;
    		return northwest;
    	}
    	if (northeast != null && northwest == null && southwest == null && southeast == null) {
    		northeast.centroid = this.centroid;
    		return northeast;
    	}
    	if (southwest != null && northwest == null && northeast == null && southeast == null) {
    		southwest.centroid = this.centroid;
    		return southwest;
    	}
    	if (southeast != null && northwest == null && northeast == null && southwest == null) {
    		southeast.centroid = this.centroid;
    		return southeast;
    	}
    	//ok, if we didn't enter any of the previous branches then we have to count
    	//and see if we have some black nodes that have points that add up to less than bucketing param
    	int count = 0;
    	if (northwest != null) {
    		count += northwest.count();
    	}
    	if (northeast != null) {
    		count += northeast.count();
    	}
    	if (southwest != null) {
    		count +=  southwest.count();
    	}
    	if (southeast != null) {
    		count += southeast.count();
    	}
    	//now let's do a final check
    	//if our count is less than bucketing param, get all the points and create a new black node and 
    	//insert into black node and return that node
    	if (count <= bucketingParam) {
    		//if less than, let's get all the points from the non null nodes and add it in here
    		Collection<KDPoint> points_to_add = new LinkedList<KDPoint>();
    		if (northwest != null) {
    			//northwest is either a black node or a gray node
    			if (northwest instanceof PRQuadBlackNode) {
    				Collection<KDPoint> points = ((PRQuadBlackNode) northwest).getPoints();
    				for (KDPoint point: points) {
    					points_to_add.add(point);
    				}
    			}
    		}
    		if (northeast != null) {
    			if (northeast instanceof PRQuadBlackNode) {
    				Collection<KDPoint> points = ((PRQuadBlackNode) northeast).getPoints();
    				for (KDPoint point: points) {
    					points_to_add.add(point);
    				}
    			}
    		}
    		if (southwest != null) {
    			if (southwest instanceof PRQuadBlackNode) {
    				Collection<KDPoint> points = ((PRQuadBlackNode) southwest).getPoints();
    				for (KDPoint point: points) {
    					points_to_add.add(point);
    				}
    			}
    		}
    		if (southeast != null) {
    			if (southeast instanceof PRQuadBlackNode) {
    				Collection<KDPoint> points = ((PRQuadBlackNode) southeast).getPoints();
    				for (KDPoint point: points) {
    					points_to_add.add(point);
    				}
    			}
    		}
    		//now we have all of our necessary points to add
    		//so just make a new black node and insert them. 
    		PRQuadBlackNode x = new PRQuadBlackNode(this.centroid, this.k, this.bucketingParam);
    		for (KDPoint new_p : points_to_add) {
    			x.insert(new_p, this.k);
    		}
    		return x;
    	}
    	
    	return this;
    }

    @Override
    public boolean search(KDPoint p){
        //determine which way to go
    	if (p.coords[0] < centroid.coords[0]) {
    		//go west
    		if (p.coords[1] < centroid.coords[1]) {
    			//go down
    			if (southwest != null) {
    				return southwest.search(p);
    			}
    			return false;
    		}
    		else {
    			//go up
    			if (northwest != null) {
    				return northwest.search(p);
    			}
    			return false;
    		}
    	}
    	else {
    		//go east
    		if (p.coords[1] < centroid.coords[1]) {
    			//go down
    			if (southeast != null) {
    				return southeast.search(p);
    			}
    			return false;
    		}
    		else {
    			//go up
    			if (northeast != null) {
    				return northeast.search(p);
    			}
    			return false;
    		}
    	}
    }

    @Override
    public int height(){
        return Math.max(Math.max(gheight(northwest), gheight(northeast)), 
        		Math.max(gheight(southwest), gheight(southeast)));
    }
    private int gheight(PRQuadNode x) {
    	if (x == null) {
    		return 0;
    	}
    	else {
    		return 1 + x.height();
    	}
    }

    @Override
    public int count(){
        //ok so idea here is this
    	//we visit each node and add all the counts up, if it's null add 0 if not get count 
    	return getCount(northwest) + getCount(northeast) + getCount(southwest) + getCount(southeast);
    }
    private int getCount(PRQuadNode x) {
    	if (x == null) {
    		return 0;
    	}
    	else {
    		return x.count();
    	}
    }

    /**
     * Returns the children of the current node in the form of a Z-ordered 1-D array.
     * @return An array of references to the children of {@code this}. The order is Z (Morton), like so:
     * <ol>
     *     <li>0 is NW</li>
     *     <li>1 is NE</li>
     *     <li>2 is SW</li>
     *     <li>3 is SE</li>
     * </ol>
     */
    public PRQuadNode[] getChildren(){
    	PRQuadNode[] x = new PRQuadNode[4];
    	x[0] = northwest;
    	x[1] = northeast;
    	x[2] = southwest;
    	x[3] = southeast;
    	return x;
    }

    @Override
    public void range(KDPoint anchor, Collection<KDPoint> results,
                      double range) {
    	//ok so idea from top level here is this, we see where anchor lies compared to centroid
    	
    	//quick test to see something
        if (northwest != null) {
        	northwest.range(anchor, results, range);
        }
        if (northeast != null) {
        	northeast.range(anchor, results, range);
        }
        if (southwest != null) {
        	southwest.range(anchor, results, range);
        }
        if (southeast != null) {
        	southeast.range(anchor, results, range);
        }
    	/*
    	if (anchor.coords[0] < centroid.coords[0]) {
        	//go west
        	if (anchor.coords[1] < centroid.coords[1]) {
        		//go south
        		//SO IF WE ENTER HERE...we visit sw, nw, ne, se
        		//we have to visit southwest since that is where our anchor lies
        		if (southwest != null) {
        			southwest.range(anchor, results, range);
        		}        		
        		//now we have to check to see if the other regions overalap
        		if (northwest != null) {
        			if (northwest.doesQuadIntersectAnchorRange(anchor, range)) {
            			//if northwest intersects, then visit it
            			northwest.range(anchor, results, range);
            		}	
        		}
        		if (northeast != null) {
        			if (northeast.doesQuadIntersectAnchorRange(anchor, range)) {
            			//if northeast intersects, then visit it
            			northeast.range(anchor, results, range);
            		}	
        		}
        		if (southeast != null) {
        			if (southeast.doesQuadIntersectAnchorRange(anchor, range)) {
            			//if southeast intersects, then visit it
            			southeast.range(anchor, results, range);
            		}	
        		}
        	}
        	else {
        		//go north
        		//SO IF WE ENTER HERE...we visit nw, ne, sw, se
        		if (northwest != null) {
        			northwest.range(anchor, results, range);	
        		}
        		if (northeast != null) {
        			if (northeast.doesQuadIntersectAnchorRange(anchor, range)) {
            			northeast.range(anchor, results, range);
            		}	
        		}        		
        		if (southwest != null) {
        			if (southwest.doesQuadIntersectAnchorRange(anchor, range)) {
            			southwest.range(anchor, results, range);
            		}	
        		}
        		if (southeast != null) {
        			if (southeast.doesQuadIntersectAnchorRange(anchor, range)) {
            			southeast.range(anchor, results, range);
            		}	
        		}        		
        	}
        }
        else {
        	//go east
        	if (anchor.coords[1] < centroid.coords[1]) {
        		//go south
        		//SO IF WE ENTER HERE...se, nw, ne, sw
        		if (southeast != null) {
        			southeast.range(anchor, results, range);	
        		}
        		if (northwest != null) {
        			if (northwest.doesQuadIntersectAnchorRange(anchor, range)) {
            			northwest.range(anchor, results, range);
            		}	
        		}
        		if (northeast != null) {
        			if (northeast.doesQuadIntersectAnchorRange(anchor, range)) {
            			northeast.range(anchor, results, range);
            		}	
        		}
        		if (southwest != null) {
        			if (southwest.doesQuadIntersectAnchorRange(anchor, range)) {
            			southwest.range(anchor, results, range);
            		}	
        		}        		
        	}
        	else {
        		//go north
        		//SO IF WE ENTER HERE...ne, nw, sw, se
        		if (northeast != null) {
        			northeast.range(anchor, results, range);	
        		}
        		if (northwest != null) {
        			if (northwest.doesQuadIntersectAnchorRange(anchor, range)) {
            			northwest.range(anchor, results, range);
            		}	
        		}        		
        		if (southwest != null) {
        			if (southwest.doesQuadIntersectAnchorRange(anchor, range)) {
            			southwest.range(anchor, results, range);
            		}	
        		}
        		if (southeast != null) {
        			if (southeast.doesQuadIntersectAnchorRange(anchor, range)) {
            			southeast.range(anchor, results, range);
            		}	
        		}
        	}
        }
        */
    }

    @Override
    public NNData<KDPoint> nearestNeighbor(KDPoint anchor, NNData<KDPoint> n)  {
    	//ok so we are looking at this from the top level
    	//so what we do here is this conceptually
    	//we see where our anchor is at and go in that direction first
    	
    	
    	//just testing a quick thing without pruning
    	if (northwest != null) {
    		northwest.nearestNeighbor(anchor, n);
    	}
    	if (northeast != null) {
    		northeast.nearestNeighbor(anchor, n);
    	}
    	if (southwest != null) {
    		southwest.nearestNeighbor(anchor, n);
    	}
    	if (southeast != null) {
    		southeast.nearestNeighbor(anchor, n);
    	}
    	/*
    	if (anchor.coords[0] < centroid.coords[0]) {
    		//go west
    		if (anchor.coords[1] < centroid.coords[1]) {
    			//go south
    			//ok so now we have a point in this region
    			//if not null, visit it
    			if (southwest != null) {
    				southwest.nearestNeighbor(anchor, n);
    			} 
    			//now we have to check to see if the other regions overalap
        		if (northwest != null) {
        			if (northwest.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if northwest intersects, then visit it
            			northwest.nearestNeighbor(anchor, n);
            		}
        		}
        		if (northeast != null) {
        			if (northeast.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if northeast intersects, then visit it
            			northeast.nearestNeighbor(anchor, n);
            		}	
        		}
        		if (southeast != null) {
        			if (southeast.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if southeast intersects, then visit it
            			southeast.nearestNeighbor(anchor, n);
            		}	
        		}
    		}
    		else {
    			//go north
    			//SO IF WE ENTER HERE...we visit nw, ne, sw, se
    			if (northwest != null) {
    				northwest.nearestNeighbor(anchor, n);
    			}
    			if (northeast != null) {
        			if (northeast.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if northeast intersects, then visit it
            			northeast.nearestNeighbor(anchor, n);
            		}	
        		}
    			if (southwest != null) {
    				if (southwest.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
    					southwest.nearestNeighbor(anchor, n);
    				}
    			}
    			if (southeast != null) {
        			if (southeast.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if southeast intersects, then visit it
            			southeast.nearestNeighbor(anchor, n);
            		}	
        		}
    		}
    	}
    	else {
    		//go east
    		if (anchor.coords[1] < centroid.coords[1]) {
    			//go south
    			//SO IF WE ENTER HERE...se, nw, ne, sw
    			if (southeast != null) {
    				southeast.nearestNeighbor(anchor, n);
    			}
    			if (northwest != null) {
        			if (northwest.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if northwest intersects, then visit it
            			northwest.nearestNeighbor(anchor, n);
            		}
        		}
    			if (northeast != null) {
        			if (northeast.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if northeast intersects, then visit it
            			northeast.nearestNeighbor(anchor, n);
            		}	
        		}
    			if (southwest != null) {
    				if (southwest.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
    					southwest.nearestNeighbor(anchor, n);
    				}
    			}
    		}
    		else {
    			//go north
    			//SO IF WE ENTER HERE...ne, nw, sw, se
    			if (northeast != null) {
    				northeast.nearestNeighbor(anchor, n);
    			}
    			if (northwest != null) {
        			if (northwest.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if northwest intersects, then visit it
            			northwest.nearestNeighbor(anchor, n);
            		}
        		}
    			if (southwest != null) {
    				if (southwest.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
    					southwest.nearestNeighbor(anchor, n);
    				}
    			}
    			if (southeast != null) {
        			if (southeast.doesQuadIntersectAnchorRange(anchor, n.getBestDist())) {
            			//if southeast intersects, then visit it
            			southeast.nearestNeighbor(anchor, n);
        			}
        		}
    		}
    	}
    	*/
        return n;
    }

    @Override
    public void kNearestNeighbors(int k, KDPoint anchor, BoundedPriorityQueue<KDPoint> queue) {
    	//NOTE: WE MIGHT HAVE A SLIGHT BUG HERE....WHAT HAPPENS IF WE START OFF BY VISITNG A NULL NODE 
    	//AND THEN DON'T GET ANY VALUE FOR GETTING THE LAST VALUE OF THE QUEUE?
        //same idea as with nearest neighbor
    	//look at which way to go first
    	
    	
    	//just a quick test
    	//no pruning here
    	if (northwest != null) {
    		northwest.kNearestNeighbors(k, anchor, queue);
    	}
    	if (northeast != null) {
    		northeast.kNearestNeighbors(k, anchor, queue);
    	}
    	if (southwest != null) {
    		southwest.kNearestNeighbors(k, anchor, queue);
    	}
    	if (southeast != null) {
    		southeast.kNearestNeighbors(k, anchor, queue);
    	}
    	/*
    	if (anchor.coords[0] < centroid.coords[0]) {
    		//go west
    		if (anchor.coords[1] < centroid.coords[1]) {
    			//go south
    			//here, we visit sw, nw, ne, se
    			//SW-----------------------------------------------------------------
    			if (southwest != null) {
    				southwest.kNearestNeighbors(k, anchor, queue);
    			}
    			//NW------------------------------------------------------------------
    			//visited sw first, but what if it was null? then we don't have last best guess
    			//so we check some stuff here
    			if (queue.isEmpty()) {
    				//if it's empty, visit nw anyways
    				if (northwest != null) {
    					northwest.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				//if it was not empty, then get the last best guess and see if our region overlaps
    				//with new range
    				if (northwest != null) {
    					double n_range = queue.getLastPriority();
        				if (northwest.doesQuadIntersectAnchorRange(anchor, n_range)) {
        					northwest.kNearestNeighbors(k, anchor, queue);
        				}	
    				}    				
    			}
    			//NE------------------------------------------------------------------
    			//ok now we have to repeat previous steps for all next quadrants
    			//say we visited sw and nw and both were empty
    			if (queue.isEmpty()) {
    				//if it's empty, visit ne anyways
    				if (northeast != null) {
    					northeast.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				//if it was not empty, then get best guess and check if it overlaps with region
    				if (northeast != null) {
    					double n_range = queue.getLastPriority();
        				if (northeast.doesQuadIntersectAnchorRange(anchor, n_range)) {
        					northeast.kNearestNeighbors(k, anchor, queue);
        				}
    				}
    			}
    			//SE----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (southeast != null) {
    					southeast.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (southeast != null) {
    					double n_range = queue.getLastPriority();
    					if (southeast.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						southeast.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    		}
    		else {
    			//go north
    			//NW-----------------------------------------------------------------FIRST
    			if (northwest != null) {
    				northwest.kNearestNeighbors(k, anchor, queue);
    			}
    			//NE-----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (northeast != null) {
    					northeast.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (northeast != null) {
    					double n_range = queue.getLastPriority();
    					if (northeast.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						northeast.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    			//SW-----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (southwest != null) {
    					southwest.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (southwest != null) {
    					double n_range = queue.getLastPriority();
    					if (southwest.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						southwest.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    			//SE-----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (southeast != null) {
    					southeast.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (southeast != null) {
    					double n_range = queue.getLastPriority();
    					if (southeast.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						southeast.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    		}
    	}
    	else {
    		//go east
    		if (anchor.coords[1] < centroid.coords[1]) {
    			//go south
    			if (southeast != null) {
    				southeast.kNearestNeighbors(k, anchor, queue);
    			}
    			//NW----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				//if it's empty, visit nw anyways
    				if (northwest != null) {
    					northwest.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				//if it was not empty, then get the last best guess and see if our region overlaps
    				//with new range
    				if (northwest != null) {
    					double n_range = queue.getLastPriority();
        				if (northwest.doesQuadIntersectAnchorRange(anchor, n_range)) {
        					northwest.kNearestNeighbors(k, anchor, queue);
        				}	
    				}    				
    			}
    			//NE----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (northeast != null) {
    					northeast.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (northeast != null) {
    					double n_range = queue.getLastPriority();
    					if (northeast.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						northeast.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    			//SW----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (southwest != null) {
    					southwest.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (southwest != null) {
    					double n_range = queue.getLastPriority();
    					if (southwest.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						southwest.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    			
    		}
    		else {
    			//go north
    			if (northeast != null) {
    				northeast.kNearestNeighbors(k, anchor, queue);
    			}
    			//NW----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				//if it's empty, visit nw anyways
    				if (northwest != null) {
    					northwest.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				//if it was not empty, then get the last best guess and see if our region overlaps
    				//with new range
    				if (northwest != null) {
    					double n_range = queue.getLastPriority();
        				if (northwest.doesQuadIntersectAnchorRange(anchor, n_range)) {
        					northwest.kNearestNeighbors(k, anchor, queue);
        				}	
    				}    				
    			}
    			//SW----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (southwest != null) {
    					southwest.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (southwest != null) {
    					double n_range = queue.getLastPriority();
    					if (southwest.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						southwest.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    			//SE----------------------------------------------------------------
    			if (queue.isEmpty()) {
    				if (southeast != null) {
    					southeast.kNearestNeighbors(k, anchor, queue);
    				}
    			} else {
    				if (southeast != null) {
    					double n_range = queue.getLastPriority();
    					if (southeast.doesQuadIntersectAnchorRange(anchor, n_range)) {
    						southeast.kNearestNeighbors(k, anchor, queue);
    					}
    				}
    			}
    		}
    	}
    	*/
    }
}


package spatial.nodes;

import spatial.exceptions.UnimplementedMethodException;
import spatial.kdpoint.KDPoint;
import spatial.knnutils.BoundedPriorityQueue;
import spatial.knnutils.NNData;
import spatial.trees.CentroidAccuracyException;
import spatial.trees.PRQuadTree;


import java.util.Collection;
import java.util.LinkedList;


/** <p>A {@link PRQuadBlackNode} is a &quot;black&quot; {@link PRQuadNode}. It maintains the following
 * invariants: </p>
 * <ul>
 *  <li>It does <b>not</b> have children.</li>
 *  <li><b>Once created</b>, it will contain at least one {@link KDPoint}. </li>
 * </ul>
 *
 * <p><b>YOU ***** MUST ***** IMPLEMENT THIS CLASS!</b></p>
 *
 * @author --- YOUR NAME HERE! ---
 */
public class PRQuadBlackNode extends PRQuadNode {


    /**
     * The default bucket size for all of our black nodes will be 1, and this is something
     * that the interface also communicates to consumers.
     */
    public static final int DEFAULT_BUCKETSIZE = 1;

    /* ******************************************************************** */
    /* *************  PLACE ANY  PRIVATE FIELDS AND METHODS HERE: ************ */
    /* ********************************************************************** */
    //we will use array to hold points
    KDPoint points[];
    int point_counts;
    /* *********************************************************************** */
    /* ***************  IMPLEMENT THE FOLLOWING PUBLIC METHODS:  ************ */
    /* *********************************************************************** */


    /**
     * Creates a {@link PRQuadBlackNode} with the provided parameters.
     * @param centroid The {@link KDPoint} which will act as the centroid of the quadrant spanned by the current {@link PRQuadBlackNode}.
     * @param k An integer to which 2 is raised to define the side length of the quadrant spanned by the current {@link PRQuadBlackNode}.
     *          See {@link PRQuadTree#PRQuadTree(int, int)} for a full explanation of how k works.
     * @param bucketingParam The bucketing parameter provided to us {@link PRQuadTree}.
     * @see PRQuadTree#PRQuadTree(int, int)
     * @see #PRQuadBlackNode(KDPoint, int, int, KDPoint)
     */
    public PRQuadBlackNode(KDPoint centroid, int k, int bucketingParam){
        super(centroid, k, bucketingParam); // Call to the super class' protected constructor to properly initialize the object is necessary, even for a constructor that just throws!
        
        //we have 0 points in our array so far
        point_counts = 0;
        //we initialize our array to the size of the bucketing parameter
        
        points = new KDPoint[bucketingParam];
    }

    /**
     * Creates a {@link PRQuadBlackNode} with the provided parameters.
     * @param centroid The centroid of the quadrant spanned by the current {@link PRQuadBlackNode}.
     * @param k The exponent to which 2 is raised in order to define the side of the current quadrant. Refer to {@link PRQuadTree#PRQuadTree(int, int)} for
     *          a thorough explanation of this parameter.
     * @param bucketingParam The bucketing parameter of the {@link PRQuadBlackNode}, passed to us by the {@link PRQuadTree} or {@link PRQuadGrayNode} during
     *                       object construction.
     * @param p The {@link KDPoint} with which we want to initialize this.
     * @see #DEFAULT_BUCKETSIZE
     * @see PRQuadTree#PRQuadTree(int, int)
     * @see #PRQuadBlackNode(KDPoint, int, int)
     */
    public PRQuadBlackNode(KDPoint centroid, int k, int bucketingParam, KDPoint p){
        this(centroid, k, bucketingParam); // Call to the current class' other constructor, which takes care of the base class' initialization itself.
        //here we are provided with an extra parameter of a kd point p, so we have to initialize our point array
        //and then we have to set point_count to 1 and insert point into array
        points = new KDPoint[bucketingParam];
        point_counts = 0;
        points[point_counts] = p;
        point_counts ++;
    }


    /**
     * <p>Inserting a {@link KDPoint} into a {@link PRQuadBlackNode} can have one of two outcomes:</p>
     *
     * <ol>
     *     <li>If, after the insertion, the node's capacity is still <b>SMALLER THAN OR EQUAL TO </b> the bucketing parameter,
     *     we should simply store the {@link KDPoint} internally.</li>
     *
     *     <li>If, after the insertion, the node's capacity <b>SURPASSES</b> the bucketing parameter, we will have to
     *     <b>SPLIT</b> the current {@link PRQuadBlackNode} into a {@link PRQuadGrayNode} which will recursively insert
     *     all the available{@link KDPoint}s. This pprocess will continue until we reach a {@link PRQuadGrayNode}
     *     which successfully separates all the {@link KDPoint}s of the quadrant it represents. Programmatically speaking,
     *     this means that the method will polymorphically call itself, splitting black nodes into gray nodes as long as
     *     is required for there to be a set of 4 quadrants that separate the points between them. This is one of the major
     *     bottlenecks in PR-QuadTrees; the presence of a pair of {@link KDPoint}s with a very small {@link
     *     KDPoint#euclideanDistance(KDPoint) euclideanDistance} between them can negatively impact search in certain subplanes, because
     *     the subtrees through which those subplanes will be modeled will be &quot;unnecessarily&quot; tall.</li>
     * </ol>
     *
     * @param p A {@link KDPoint} to insert into the subtree rooted at the current node.
     * @param k The side length of the quadrant spanned by the <b>current</b> {@link PRQuadGrayNode}. It will need to be updated
     *           per recursive call to help guide the input {@link KDPoint} to the appropriate subtree.
     * @return The subtree rooted at the current node, potentially adjusted after insertion.
     */
    @Override
    public PRQuadNode insert(KDPoint p, int k) {
    	//black nodes can have k == 0, gray nodes cannot
    	//if the number of points we have is greater than or equal to the bucketing parameter,
    	//make a grey node and insert appropriately
        if (point_counts >= bucketingParam) {
        	if (k <= 0) {
        		throw new CentroidAccuracyException("black node too small");
        	}
        	PRQuadGrayNode x = new PRQuadGrayNode(centroid, k - 1, bucketingParam);
        	//ok so now we have gray node x, so we have to assign the points to the proper regions
        	//4 regions to worry about
        	for (int i = 0; i < point_counts; i++) {
        		//add each point to gray node
        		x.insert(points[i], k - 1);
        	}
        	//problem with resplitting here...
        	x.insert(p, k);
        	return x;
        } else {
        	//insert and increment point counts
        	points[point_counts] = p;
            point_counts ++;
        	//return this current node
        	return this;
        }
    }


    /**
     * <p><b>Successfully</b> deleting a {@link KDPoint} from a {@link PRQuadBlackNode} always decrements its capacity by 1. If, after
     * deletion, the capacity is at least 1, then no further changes need to be made to the node. Otherwise, it can
     * be scrapped and turned into a white node.</p>
     *
     * <p>If the provided {@link KDPoint} is <b>not</b> contained by this, no changes should be made to the internal
     * structure of this, which should be returned as is.</p>
     * @param p The {@link KDPoint} to delete from this.
     * @return Either this or null, depending on whether the node underflows.
     */
    @Override
    public PRQuadNode delete(KDPoint p) {
        //so we have to iterate through our array to see if we have to delete a point
    	int index = -1;
    	for (int i = 0; i < point_counts; i++) {
    		if (points[i].equals(p)) {
        		points[i] = null;
        		index = i;
        	}
    	}
    	if (index != -1) {
    		//if our index is not -1, then that means we must have found some point
    		//so what we will do here is say that our deleted point will be equal to 
    		//whatever our last point is
    		points[index] = points[point_counts - 1];
    		//now we set the last point equal to null
    		points[point_counts - 1] = null;
    		//now decrement point count
    		point_counts --;
    	}
    	if (point_counts == 0) {
    		return null;
    	}
    	return this;
    }

    @Override
    public boolean search(KDPoint p){
        for (int i = 0; i < point_counts; i++) {
        	if (points[i].equals(p)) {
        		return true;
        	}
        }
    	return false;
    }

    @Override
    public int height(){
        return 0;
    }

    @Override
    public int count()  {
        return point_counts;
    }

    /** Returns all the {@link KDPoint}s contained by the {@link PRQuadBlackNode}. <b>INVARIANT</b>: the returned
     * {@link Collection}'s size can only be between 1 and bucket-size inclusive.
     *
     * @return A {@link Collection} that contains all the {@link KDPoint}s that are contained by the node. It is
     * guaranteed, by the invariants, that the {@link Collection} will not be empty, and it will also <b>not</b> be
     * a null reference.
     */
    public Collection<KDPoint> getPoints()  {
    	LinkedList<KDPoint> pts = new LinkedList<>();
    	for (int i = 0; i < point_counts; i++) {
    		pts.add(points[i]);
    	}
        return pts;
    }

    @Override
    public void range(KDPoint anchor, Collection<KDPoint> results,
                      double range) {
        //ok so when we are in a black node, the idea is this
    	//we just iterate through our array and see if the point is within range and add it to collection
    	for (int i = 0; i < point_counts; i++) {
    		if (anchor.euclideanDistance(points[i]) <= range) {
    			results.add(points[i]);
    		}
    	}
    }

    @Override
    public NNData<KDPoint> nearestNeighbor(KDPoint anchor, NNData<KDPoint> n) {
    	//ok so we are on a black node, what we will want to do is this
    	//first if we have infinity passed in for our n, our best guess is -1
    	//so we'll have to update that range with whatever the first point we visit is
    	for (int i = 0; i < point_counts; i++) {    		
    		if (n.getBestDist() == -1 && anchor.euclideanDistance(points[i]) != 0) {
    			//we do this if we have not visited anything yet
    			double n_best_dist = anchor.euclideanDistance(points[i]);
    			n.update(points[i], n_best_dist);
    		}
    		//calculate the distance from current point to anchor
    		double new_dist = anchor.euclideanDistance(points[i]);
    		//compare new distance to what our best distance is so far
    		if (new_dist < n.getBestDist() && new_dist != 0) {
    			//if our new distance is less than our current best, update point
    			n.update(points[i], new_dist);
    		}
    	}
    	//just return our point n
        return n;
    }

    @Override
    public void kNearestNeighbors(int k, KDPoint anchor, BoundedPriorityQueue<KDPoint> queue){
        //ok so idea here is this
    	//we are in a black node
    	//and we have a bounded priority queue
    	//k is the number of nearest neighbors, so if we want 4 nearest neighbors, 3, etc
    	//we got anchor point
    	//and we got our queue
    	for (int i = 0; i < point_counts; i++) {
    		if (points[i].euclideanDistance(anchor) != 0) {
    			queue.enqueue(points[i], anchor.euclideanDistance(points[i]));
    		}
    	}
    }
}

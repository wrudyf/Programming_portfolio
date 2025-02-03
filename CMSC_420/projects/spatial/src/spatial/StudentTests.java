package spatial;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import spatial.kdpoint.InvalidDimensionalityException;
import spatial.kdpoint.KDPoint;
import spatial.knnutils.BoundedPriorityQueue;
import spatial.trees.KDTree;
import spatial.trees.PRQuadTree;
import visualization.CompactVizTree;

import java.util.*;

import static org.junit.Assert.*;
import static spatial.kdpoint.KDPoint.*;

/**
 * <p>A testing framework for {@link spatial.trees.KDTree} and {@link spatial.trees.PRQuadTree}</p>.
 * You should extend it with your own tests.
 *
 * @author  --- YOUR NAME HERE! ---
 *
 * @see KDTree
 * @see PRQuadTree
 * @see spatial.trees.SpatialDictionary
 * @see spatial.trees.SpatialQuerySolver
 */

public class StudentTests {


    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */
    /* *********************************** PRIVATE FIELDS  AND METHODS **************************************** */
    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */

    private PRQuadTree prQuadTree;
    private KDTree kdTree;
    private static final long SEED=47;
    private Random r;
    private static final int MAX_ITER = 200;
    private static final int BOUND=100; // An upper bound for your sampled int coordinates.
    private int getRandomSign(){
        return 2 * r.nextInt(2) - 1;
    }

    private int[] getRandomIntCoords(int dim){
        int[] coords = new int[dim];
        for(int i = 0; i < dim; i++)
            coords[i] = getRandomSign() * r.nextInt(BOUND);
        return coords;
    }

    private KDPoint getRandomPoint(int dim){
        return new KDPoint(getRandomIntCoords(dim)); // This will trigger KDPoint(double[]...) constructor
    }

    private boolean checkRangeQuery(KDTree tree, KDPoint origin, double range, KDPoint... candidates){
        Collection<KDPoint> rangeQueryResults = tree.range(origin, range);
        List<KDPoint> candidateList = Arrays.asList(candidates);
        return rangeQueryResults.containsAll(candidateList); // Order not important in range queries: only containment.
    }

    /* Setup and teardown methods; those are run before and after every jUnit test. */


    @Before
    public void setUp(){
        r = new Random(SEED);
        prQuadTree = new PRQuadTree(r.nextInt(BOUND), r.nextInt(BOUND));
    }

    @After
    public void tearDown(){
        r = null;
        kdTree = null;
        prQuadTree = null;
        System.gc();
    }

    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */
    /* ***************************************** BPQ Tests ************************************************* */
    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */

    @Test(expected=IllegalArgumentException.class)
    public void testBPQZeroCapacityProvided(){
        new BoundedPriorityQueue<>(0);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testBPQNegativeCapacityProvided(){
        new BoundedPriorityQueue<>(-1);
    }

    @Test
    public void testBPQBasicEnqueueDequeueFirstAndLast(){
        BoundedPriorityQueue<KDPoint> myQueue = new BoundedPriorityQueue<>(1);
        myQueue.enqueue(ZERO, 2.3);
        assertEquals("After enqueueing a single KDPoint in a BPQ instance with a capacity of 1, a call to first() did not return " +
                "the point itself.", ZERO, myQueue.first());
        assertEquals("After enqueueing a single KDPoint in a BPQ instance with a capacity of 1, a call to last() did not return " +
                "the point itself.", ZERO, myQueue.last());
        assertEquals("After enqueueing a single KDPoint in a BPQ instance with a capacity of 1, a call to dequeue() did not return " +
                "the point itself.", ZERO, myQueue.dequeue());
    }

    @Test
    public void testBPQComplexEnqueueDequeueFirstAndLast() {
        BoundedPriorityQueue<KDPoint> myQueue = new BoundedPriorityQueue<>(3);
        myQueue.enqueue(ZERO, 2.3);
        myQueue.enqueue(ONEONE, 1.1);
        assertEquals("After enqueueing two KDPoints in a BPQ instance with a capacity of 3, a call to first() did not return " +
                "the expected point.", ONEONE, myQueue.first());
        assertEquals("After enqueueing two KDPoints in a BPQ instance with a capacity of 3, a call to last() did not return " +
                "the expected point.", ZERO, myQueue.last());
        assertEquals("After enqueueing two KDPoints in a BPQ instance with a capacity of 3, a call to dequeu() did not return " +
                "the expected point.", ONEONE, myQueue.dequeue());
        myQueue.enqueue(MINUSONEMINUSONE, 4);
        System.out.println("testing");
        assertEquals("After enqueueing two KDPoints in a BPQ instance with a capacity of 3, dequeuing one and enqueuing another, " +
                "a call to last() did not return the expected point.", MINUSONEMINUSONE, myQueue.last());

    }

    @Test
    public void testBPQEnqueuePastCapacity(){
        BoundedPriorityQueue<KDPoint> myQueue = new BoundedPriorityQueue<>(5);
        myQueue.enqueue(ZERO, 1);
        myQueue.enqueue(ONEONE, 2);
        myQueue.enqueue(ONEMINUSONE, 3);
        myQueue.enqueue(MINUSONEONE, 4);
        myQueue.enqueue(ZEROMINUSONE,5);
        myQueue.enqueue(ONEZERO, 5);   // FIFO should keep this one away
        assertEquals("After enqueuing six elements in a BPQ with initial capacity 5, a call to last() did not return " +
                "the expected element.", ZEROMINUSONE, myQueue.last());
        myQueue.enqueue(ONEZERO, 0.5);   // The BPQ's sorting should put this first.
        myQueue.enqueue(ZEROONE, 1.5);  // And this third.
        assertEquals("After enqueuing eight elements in a BPQ with initial capacity 5, we would expect its size to still be" +
                "5.", 5, myQueue.size());
        assertEquals("After enqueuing eight elements in a BPQ with initial capacity 5, a call to dequeue() did not return " +
                "the expected element.", ONEZERO, myQueue.dequeue()); // Two previous last ones must have been thrown out.
        assertEquals("After enqueuing eight elements in a BPQ with initial capacity 5 and one dequeueing, our second call to dequeue() did not return " +
                "the expected element.", ZERO, myQueue.dequeue());
        assertEquals("After enqueuing eight elements in a BPQ with initial capacity 5 and two dequeueings, our third call to dequeue() did not return " +
                "the expected element.", ZEROONE, myQueue.dequeue());
    }
    @Test
    public void testBPQEnqueueSimpleStuff(){
        BoundedPriorityQueue<KDPoint> myQueue = new BoundedPriorityQueue<>(3);
        myQueue.enqueue(ZERO, 0);
        myQueue.enqueue(ONEONE, 0);
        myQueue.enqueue(ONEMINUSONE, 0);
        myQueue.enqueue(ZERO, 0);
        myQueue.enqueue(MINUSONEMINUSONE, 2.5);
        myQueue.enqueue(MINUSONEMINUSONE, 4);
        System.out.println("checking");
        System.out.println(myQueue.size());
        System.out.println(myQueue.dequeue());
        System.out.println(myQueue.dequeue());
        System.out.println(myQueue.dequeue());
        System.out.println(myQueue.dequeue());
        System.out.println(myQueue.first());
        System.out.println(myQueue.last());
        System.out.println("checking again");
    }
    @Test
    public void testBPQEnqueueSimpleStuff2(){
        BoundedPriorityQueue<KDPoint> myQueue = new BoundedPriorityQueue<>(3);
        myQueue.enqueue(ZERO, 0);
        myQueue.enqueue(ONEONE, 0);
        myQueue.enqueue(ONEMINUSONE, 0);
        myQueue.enqueue(MINUSONEMINUSONE, 2.5);
        myQueue.enqueue(MINUSONEMINUSONE, 4);
        System.out.println("checking");
        myQueue.dequeue();
        System.out.println("checking again");
    }

    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */
    /* ***************************************** KD-TREE TESTS ************************************************* */
    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */
    @Test
    public void testKDTreeIsEmpty(){
        kdTree = new KDTree(10);
        assertTrue("A freshly created KD-Tree should be empty!", kdTree.isEmpty());
    }

    @Test
    public void testKDTreeFewInsertions(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 30));
        kdTree.insert(new KDPoint(12, 18));
        kdTree.insert(new KDPoint(-20, 300));

        assertEquals("The first point inserted should be our root.", new KDPoint(10, 30), kdTree.getRoot());
        assertEquals("The height of this KD-Tree should be 1.", 1, kdTree.height());
        assertEquals("The number of nodes in this tree should be 3.", 3, kdTree.count());
    }

    @Test
    public void testKDTreeSimpleRange() throws InvalidDimensionalityException {
        int MAX_DIM = 10;
        for(int dim = 1; dim <= MAX_DIM; dim++){ // For MAX_DIM-many trees...
            KDTree tree = new KDTree(dim);
            for(int i = 0; i < MAX_ITER; i++){ // For MAX_ITER-many points...
                KDPoint originInDim = KDPoint.getOriginInDim(dim);
                KDPoint p = getRandomPoint(dim);
                tree.insert(p);
                assertTrue("Failed a range query for a " + dim + "-D tree which only contained " +
                        p + ", KDPoint #" + i + ".", checkRangeQuery(tree, originInDim, p.euclideanDistance(originInDim)));
            }
        }
    }


    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */
    /* ***************************************** PR-QUADTREE TESTS ******************************************** */
    /* ******************************************************************************************************** */
    /* ******************************************************************************************************** */
    @Test
    public void testPRQEmptyPRQuadTree(){
        assertNotNull("Tree reference should be non-null by setUp() method.", prQuadTree);
        assertTrue("A freshly created PR-QuadTree should be empty!", prQuadTree.isEmpty());
    }


    @Test
    public void testPRQSimpleQuadTree(){
        prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
        prQuadTree.insert(new KDPoint(1, 1));
        prQuadTree.insert(new KDPoint(4, 2)); // Should fit
        assertEquals("After two insertions into a PR-QuadTree with b = 2, the result should be a quadtree consisting of a single black node.",
            0, prQuadTree.height());
        assertEquals("After two insertions into a PR-QuadTree, the count should be 2.", 2, prQuadTree.count());

        // The following deletion should work just fine...
        try {
            prQuadTree.delete(new KDPoint(1, 1));
        } catch(Throwable t){
            fail("Caught a " + t.getClass().getSimpleName() + " with message: " + t.getMessage() + " when attempting to delete a KDPoint that *should*" +
                    " be in the PR-QuadTree.");
        }
        assertFalse("After deleting a point from a PR-QuadTree, we should no longer be finding it in the tree.",
                prQuadTree.search(new KDPoint(1, 1)));

        // The following two insertions should split the root node into a gray node with 2 black node children and 2 white node children.
        prQuadTree.insert(new KDPoint(-5, -6));
        prQuadTree.insert(new KDPoint(0, 0)); // (0, 0) should go to the NE quadrant after splitting.
        assertEquals("After inserting three points into a PR-QuadTree with b = 2, the tree should split into a gray node with 4 children.",
                prQuadTree.height(), 1);
        for(KDPoint p: new KDPoint[]{new KDPoint(0, 0), new KDPoint(4, 2), new KDPoint(-5, -6)})
            assertTrue("After inserting a point into a PR-QuadTree without subsequently deleting it, we should be able to find it.", prQuadTree.search(p));

    }




    @Test
    public void testKNNPRQuadTree(){

        int k = 4;
        int kNN = 3;
        int bucketingParam = 2;
        prQuadTree = new PRQuadTree(k, bucketingParam); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
        KDPoint[] points = {new KDPoint(1,1),new KDPoint(2,2),new KDPoint(3,3),
                new KDPoint(-2,2),new KDPoint(-1,2),new KDPoint(-2,6),
        };
        for(int i=1;i<points.length;i++)
            prQuadTree.insert(points[i]);




        KDPoint queryPt = new KDPoint(-1,4);


        BoundedPriorityQueue<KDPoint> expectedKnnPoints = new BoundedPriorityQueue<>(kNN+1);
        expectedKnnPoints.enqueue(new KDPoint(-1, 2),2);
        expectedKnnPoints.enqueue(new KDPoint(-2, 2), 2.236068);
        expectedKnnPoints.enqueue(new KDPoint(-2, 6),2.236068 );
//        expectedKnnPoints.enqueue(new KDPoint(-2.0, 7.0),2.236068);



        BoundedPriorityQueue<KDPoint> knnPoints = prQuadTree.kNearestNeighbors(kNN,queryPt);
        assertEquals("Expected KNN result to have "+expectedKnnPoints.size()+" elements but it actually have "+knnPoints.size()+ " elements"
                ,expectedKnnPoints.size(),knnPoints.size());
        KDPoint actualPoint;
        for (int i=0;i<kNN;i++)
        {
            actualPoint = knnPoints.dequeue();
            assertTrue("KNN result contains "+actualPoint+", which was not expected" ,expectedKnnPoints.contains(actualPoint));
        }

    }

    @Test
    public void testNNPRQuadTree(){
        prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
        KDPoint[] points = {new KDPoint(1,1)};

        prQuadTree.insert(points[0]);
        KDPoint nn;

        nn = prQuadTree.nearestNeighbor(points[0]);
        assertNull("nearestNeighbor check; Expected null but actual value is not null. Make sure the code does not include query point in the result",nn);


        nn = prQuadTree.nearestNeighbor(new KDPoint(points[0].coords[0],points[0].coords[1] + 1));
        assertEquals("nearestNeighbor check failed. ",nn,points[0]);
    }

    @Test
    public void testPRQRange() {
        prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
        KDPoint point = ONEONE;
        prQuadTree.insert(point);
        ArrayList<KDPoint> ptsWithinRange = new ArrayList<>(prQuadTree.range(ZERO,0.5));
        assertEquals("PR-QuadTree contains (1, 1) and a range query from (0, 0) with a range of 0.5 " +
                        "should not be sufficient to include (1, 1)",
                0,ptsWithinRange.size());
        ptsWithinRange = new ArrayList<>(prQuadTree.range(ZERO, euclideanDistance(ZERO, point)));
        assertTrue("PR-QuadTree contains (1, 1) and a range query from (0, 0) with a range of sqrt(2) + EPS " +
                        "should be sufficient to include (1, 1)",
                ptsWithinRange.size() == 1 && ptsWithinRange.get(0).equals(point));

        // Inserting (0, 0) should *not* change anything, because we never report the anchor point.
        prQuadTree.insert(point);
        ptsWithinRange = new ArrayList<>(prQuadTree.range(ZERO, euclideanDistance(ZERO, point)));
        assertTrue("PR-QuadTree contains (1, 1) and (0, 0). A range query from (0, 0) with a range " +
                        "of sqrt(2) + EPS should be sufficient to include (1, 1) but *not* report (0, 0).",
                ptsWithinRange.size() == 1 && ptsWithinRange.get(0).equals(point));
    }


    /**
     * This &quot;test&quot; just gives an example for how to generate a KD-tree visualization using {@link CompactVizTree}.
     * If successful, an image named <tt>compact_kdtree.png</tt> should be saved inside your project directory
     * Please make sure to delete these image files before submission. We give it to you as a {@code jUnit} test
     * in order for it to run automatically whenever you run the full suite.
     */
    @Test
    public void testKDTreeViz(){
        kdTree = new KDTree(2);
        KDPoint[] points = {new KDPoint(10, 30),
                new KDPoint(12, 18),
                new KDPoint(-20, 300),
                new KDPoint(16, 100),
                new KDPoint(10, 500),
                new KDPoint(18, 500),
        };

        for(int i=1;i<points.length;i++)
            kdTree.insert(points[i]);

        ArrayList<String> kdDescription = kdTree.treeDescription(false);
        CompactVizTree visualizer = new CompactVizTree(120,40,10);
        visualizer.drawBinaryTreeToFile(kdDescription,"compact_kdtree");

    }

    /**
     * This "test" just gives an example for how to generate a Quadtree visualization using {@link CompactVizTree}.
     * If successful, an image named {@code compact_quadtree.png} should be created.
     * Please make sure to delete these image files before submission. We give it to you as a {@code jUnit} test
     * in order for it to run automatically whenever you run the full suite.
     */
    @Test
    public void testPRTreeViz(){
        prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.

        KDPoint[] points = {new KDPoint(1, 1),
                new KDPoint(4, 2),
                new KDPoint(7, 2),
                new KDPoint(7, 7),
                new KDPoint(2, 7),
                new KDPoint(-1, -7),
                new KDPoint(-1, 7),
                new KDPoint(7, -7),
                new KDPoint(5, 2),
                new KDPoint(1, 2),
                new KDPoint(-2, 2),
                new KDPoint(-2, 1)
        };

        for(int i=1;i<points.length;i++)
            prQuadTree.insert(points[i]);


        ArrayList<String> kdDescription = prQuadTree.treeDescription(false);
//        VizTree visualizer = new VizTree();
        CompactVizTree visualizer = new CompactVizTree(120,120,10);
        visualizer.drawBTreeToFile(kdDescription,4,"compact_quadtree");
    }
    
    //MY STUDENT TESTS BELOW HERE
    
    //SIMPLE INSERTIONS INTO KD TREE TESTS
    @Test
    public void testKDTreeFewInsertions1(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(2, 3));
        kdTree.insert(new KDPoint(5, 1));
        kdTree.insert(new KDPoint(1, 10));
        kdTree.insert(new KDPoint(4, 0));
        System.out.println("testing");
        System.out.println("testing is 1,3 in kd tree?" + kdTree.search(new KDPoint (1,3)));
        System.out.println("testing is 2,3 in kd tree?" + kdTree.search(new KDPoint (2,3)));
        System.out.println("testing is 4,0 in kd tree?" + kdTree.search(new KDPoint (4,0)));
    }
    
    //SIMPLE INSERT AND DELETE INTO KD TREE TESTS <---------------------------------------------------------
    @Test
    public void testKDTreeOneInsertOneDelete(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 20));
        System.out.println("testing");
        kdTree.delete(new KDPoint(10, 20));
        assertEquals(null, kdTree.getRoot());
        //ok 
    }
    @Test
    public void testKDTreeOneInsertOneDeletetest(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 20));
        kdTree.insert(new KDPoint(9, 5));
        System.out.println("testing");
        kdTree.delete(new KDPoint(9, 5));
        kdTree.delete(new KDPoint(10,20));
        System.out.println(kdTree.height());
        System.out.println(kdTree.isEmpty());
        System.out.println("testing point 2");
        //ok with deleting left and right
    }
    @Test
    public void testKDTreeOneInsertOneDeletetestLecvideo(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 20));
        kdTree.insert(new KDPoint(5, 10));
        kdTree.insert(new KDPoint(5, 8));
        kdTree.insert(new KDPoint(11, 5));
        kdTree.insert(new KDPoint(15,2));
        kdTree.insert(new KDPoint(20, 1));
        System.out.println("testing");
        kdTree.delete(new KDPoint(10, 20));
        kdTree.delete(new KDPoint(5, 10));
        kdTree.delete(new KDPoint(5, 8));
        kdTree.delete(new KDPoint(11, 5));
        kdTree.delete(new KDPoint(15,2));
        kdTree.delete(new KDPoint(20, 1));
        System.out.println(kdTree.isEmpty());
        System.out.println("testing point 2");
        //ok 
    }
    
    @Test
    public void testKDTreeheight(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 20));
        kdTree.insert(new KDPoint(5, 10));
        kdTree.insert(new KDPoint(5, 8));
        kdTree.insert(new KDPoint(11, 5));
        kdTree.insert(new KDPoint(15,2));
        kdTree.insert(new KDPoint(20, 1));
        System.out.println("height " + kdTree.height());
        System.out.println("testing point 2");
        assertEquals(3, kdTree.height());
        //keep getting height wrong...maybe issue is with how i insert values into tree
    }
    @Test
    public void testKDTreeTwoInsertOneDelete(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 20));
        kdTree.insert(new KDPoint(22, 30));
        System.out.println("testing");
        kdTree.delete(new KDPoint(10, 20));
        System.out.println("testing point 2");
    }
    
    @Test
    public void testKDTreeThreeInsertOneInnerDelete(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(10, 20));
        kdTree.insert(new KDPoint(9, 10));
        kdTree.insert(new KDPoint(2, 5));
        kdTree.insert(new KDPoint(4, 15));        
        System.out.println("testing");
        kdTree.delete(new KDPoint(22, 30));
        System.out.println("testing point 2");
    }
    @Test
    public void testKDTreeDeleteLeftNonNullRightNull(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(5, 10));
        kdTree.insert(new KDPoint(3, 11));
        kdTree.insert(new KDPoint(2, 10));        
        System.out.println("testing");
        kdTree.delete(new KDPoint(3, 11));
        System.out.println("testing point 2");
    }
    
    @Test
    public void testKDTreeNearestNeighbor22(){
        kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(5, 10));
        kdTree.insert(new KDPoint(3, 11));
        kdTree.insert(new KDPoint(2, 10));        
        kdTree.insert(new KDPoint(7, 11));
        kdTree.insert(new KDPoint(10, 20));
        System.out.println(kdTree.nearestNeighbor(new KDPoint(1, 9)));
    }
    
    @Test public void testKDTreeRangeSimple() {
    	kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(5, 10));
        
        kdTree.insert(new KDPoint(1, 10));
        kdTree.insert(new KDPoint(7, 9));        
        kdTree.insert(new KDPoint(6, 8));
        kdTree.insert(new KDPoint(11, 10));
        
        //anchor  = 5,9, range = 3, should only get points
        //(5,10), (7,9), (6,8)
        //should not see nodes 1,10 or 11,10
        System.out.println(kdTree.range(new KDPoint(5,9), 3));
        System.out.println("testing");
    }
    
    @Test public void testKDTreeRangeSimple3() {
    	kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(0, -2));
        
        kdTree.insert(new KDPoint(-2, 1));
        kdTree.insert(new KDPoint(7, 0));        
        kdTree.insert(new KDPoint(7, 5));
        kdTree.insert(new KDPoint(3, -5));
        
        //anchor  = 5,9, range = 3, should only get points
        //(5,10), (7,9), (6,8)
        //should not see nodes 1,10 or 11,10
        System.out.println(kdTree.range(new KDPoint(7,3), 4.5));
        System.out.println("testing");
    }
    
    @Test public void testKDTreeNearestNeighbor2() {
    	kdTree = new KDTree(2);
        kdTree.insert(new KDPoint(0, -2));
        
        kdTree.insert(new KDPoint(7,3));
        kdTree.insert(new KDPoint(-2, 1));
        kdTree.insert(new KDPoint(7, 0));        
        kdTree.insert(new KDPoint(7, 5));
        kdTree.insert(new KDPoint(3, -5));
        
        //anchor  = 5,9, range = 3, should only get points
        //(5,10), (7,9), (6,8)
        //should not see nodes 1,10 or 11,10
        System.out.println(kdTree.nearestNeighbor(new KDPoint(7,3)) );
        System.out.println("testing");
    }
    
    @Test public void testKDTreeNearestNeighborSimple1() {
    	kdTree = new KDTree(2);
    	kdTree.insert(new KDPoint(4,11));
    	kdTree.insert(new KDPoint(2,-5));
    	kdTree.insert(new KDPoint(7,5));
        //anchor  = 5,9, range = 3, should only get points
        //(5,10), (7,9), (6,8)
        //should not see nodes 1,10 or 11,10
        System.out.println(kdTree.nearestNeighbor(new KDPoint(7,3)) );
        System.out.println("testing");
    }
    @Test public void testKDTreeNearestNeighborSimple1D() {
    	kdTree = new KDTree(1);
    	kdTree.insert(new KDPoint(4));
    	kdTree.insert(new KDPoint(6));
    	//kdTree.insert(new KDPoint(7,5));
        //anchor  = 5,9, range = 3, should only get points
        //(5,10), (7,9), (6,8)
        //should not see nodes 1,10 or 11,10
        System.out.println(kdTree.nearestNeighbor(new KDPoint(4) ));
        System.out.println("testing");
    }
    @Test public void testKDTreeKNearestNeighborTestSimple() {
    	kdTree = new KDTree(1);
    	kdTree.insert(new KDPoint(4));
    	kdTree.insert(new KDPoint(6));
    	//kdTree.insert(new KDPoint(7,5));
        //anchor  = 5,9, range = 3, should only get points
        //(5,10), (7,9), (6,8)
        //should not see nodes 1,10 or 11,10
        System.out.println(kdTree.nearestNeighbor(new KDPoint(4) ));
        System.out.println("testing");
    }
    @Test public void testKDTree4NearestNeighborTestSimple() {
    	kdTree = new KDTree(1);
    	kdTree.insert(new KDPoint(5, 1));
    	kdTree.insert(new KDPoint(6, 3));
    	kdTree.insert(new KDPoint(5, 3));
    	kdTree.insert(new KDPoint(2, 0));
    	BoundedPriorityQueue<KDPoint> myQueue = kdTree.kNearestNeighbors(2, (new KDPoint (5,2)));
        Iterator it = myQueue.iterator();
        while (it.hasNext()) {
        	System.out.println(it.next());
        }
        System.out.println("testing");
    }
    @Test public void testKDTreeDeleteOne() {
    	kdTree = new KDTree(1);
    	kdTree.insert(new KDPoint(5, 1));
    	kdTree.delete(new KDPoint(5, 1));
    	//after inserting one point and deleting it, root should be null
    	assertEquals(null, kdTree.getRoot());
    }
    @Test public void testKDTreeDeleteOneFromTwo() {
    	kdTree = new KDTree(2);
    	//INSERTIONS
    	kdTree.insert(new KDPoint(10, 20));
    	//kdTree.insert(new KDPoint(5, 10));
    	//kdTree.insert(new KDPoint(5, 8));
    	kdTree.insert(new KDPoint(11, 5));
    	kdTree.insert(new KDPoint(15, 2));
    	kdTree.insert(new KDPoint(20, 1));
    	//DELETIONS
    	kdTree.delete(new KDPoint(11, 5));
    	kdTree.delete(new KDPoint(15, 2));
    	kdTree.delete(new KDPoint(20, 1));
    	kdTree.delete(new KDPoint(10, 20));
    	
    	System.out.println("KD tree height is " + kdTree.height());
    	//after inserting one point and deleting it, root should be null
    	assertEquals(true, kdTree.isEmpty());
    }
    @Test public void testKDTreeDeleteOneFromThree() {
    	kdTree = new KDTree(3);
    	kdTree.insert(new KDPoint(5, 1, 2));
    	kdTree.insert(new KDPoint(6, 8, 12));
    	kdTree.insert(new KDPoint(6, 7, 2));
    	kdTree.insert(new KDPoint(7, 0, 0));
    	kdTree.insert(new KDPoint(7, 0, 3));
    	//kdTree.insert(new KDPoint(6, 5, 5));
    	//kdTree.insert(new KDPoint(4, 10, 3));
    	//kdTree.delete(new KDPoint(4, 10, 3));
    	//kdTree.delete(new KDPoint(6, 8, 12));
    	kdTree.delete(new KDPoint(7,0,0));
    	//kdTree.delete(new KDPoint(7,0,3));
    	//kdTree.delete(new KDPoint(5, 1, 2));
    	//kdTree.delete(new KDPoint(4, 10, 3));
    	//kdTree.delete(new KDPoint(6, 5, 5));
    	//after inserting one point and deleting it, root should be null
    	assertEquals(new KDPoint(5, 1, 2), kdTree.getRoot());
    }
    @Test public void testKDTreeHeightQuickTest() {
    	kdTree = new KDTree(3);
    	kdTree.insert(new KDPoint(5, 10, 1));
    	kdTree.insert(new KDPoint(10, 11, 2));
    	kdTree.insert(new KDPoint(7, 9, 4));
    	kdTree.insert(new KDPoint(6, 13, 2));
    	kdTree.insert(new KDPoint(100, 5, 3));
    	kdTree.insert(new KDPoint(6, 10, 5));
    	kdTree.delete(new KDPoint(5, 1, 2));
    	assertEquals(4, kdTree.height());
    }
    
    // NOV 6 CHANGES?
    @Test public void testKDTreeHeightQuickDelete() {
    	kdTree = new KDTree(3);
    	kdTree.insert(new KDPoint(5, 10, 1));
    	kdTree.insert(new KDPoint(10, 11, 2));
    	kdTree.insert(new KDPoint(3, 5, 11));
    	kdTree.insert(new KDPoint(7, 9, 11));
    	kdTree.insert(new KDPoint(7, 12, 11));
    	kdTree.delete(new KDPoint(5, 10, 1));
    	assertEquals(4, kdTree.height());
    }
    @Test public void testKDTreeHeightQuickDelete2d() {
    	kdTree = new KDTree(2);
    	kdTree.insert(new KDPoint(5, 10));
    	kdTree.insert(new KDPoint(6,8));
    	kdTree.insert(new KDPoint(6,7));
    	kdTree.insert(new KDPoint(6,10));
    	kdTree.delete(new KDPoint(5, 10));
    	assertEquals(2, kdTree.height());
    }
    @Test public void testKDTreeDuplicateInsert() {
    	kdTree = new KDTree(2);
    	kdTree.insert(new KDPoint(5,10));
    	kdTree.insert(new KDPoint(5,10));
    	kdTree.insert(new KDPoint(5,10));
    	//kdTree.delete(new KDPoint(5, 10));
    	assertEquals(2, kdTree.height());
    }
    
    @Test public void testKDTreeKNNTests() {
    	kdTree = new KDTree(2);
    	kdTree.insert(new KDPoint(5,10));
    	kdTree.insert(new KDPoint(10,10));
    	kdTree.insert(new KDPoint(2,5));
    	kdTree.insert(new KDPoint(5, 7));
    	kdTree.insert(new KDPoint(15, 22));
    	
    	BoundedPriorityQueue<KDPoint> myQueue = kdTree.kNearestNeighbors(3, (new KDPoint (15,30)));
        Iterator it = myQueue.iterator();
        while (it.hasNext()) {
        	System.out.println(it.next());
        }
    }
    
    //PR QUAD TREE TESTS BELOW HERE <--------------------------------------------------------------
    @Test public void testPRQuadTreeBasicInsertANdSplit() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(3, 2));
    	prQuadTree.insert(new KDPoint(-3, 0));
    	System.out.println("t");
    	prQuadTree.insert(new KDPoint(3, 6));
    	System.out.println("t");
    	System.out.println("height is " + prQuadTree.height());
    	//prQuadTree.insert(new KDPoint(-3, -3));
    	//have to implement search
    }
    
    
    @Test public void testPRQuadTreeBasicInsertANdDelete() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(3, 2));
    	prQuadTree.insert(new KDPoint(1, 2));
    	prQuadTree.delete(new KDPoint(2, 4));
    	System.out.println("testing");
    }
    
    @Test public void testPRQuadTreeBasicInsertANdDeleteReturnOne() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(3, 2));
    	prQuadTree.insert(new KDPoint(1, 2));
    	prQuadTree.delete(new KDPoint(2, 4));
    	System.out.println("testing");
    }
    @Test public void testPRQuadTreeBasicInsertANdDeleteMergeIntoONe() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(-3, 2));
    	prQuadTree.insert(new KDPoint(1, -2));
    	prQuadTree.delete(new KDPoint(2, 4));
    	System.out.println("testing");
    }
    @Test public void testPRQuadTreeBasicInsertAndGetChildren() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(-3, 2));
    	prQuadTree.insert(new KDPoint(1, -2));
    	prQuadTree.delete(new KDPoint(2, 4));
    	//how do i test getchidlren???
    }
    @Test public void testPRQuadTreeBasicRangeTest() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(-3, 2));
    	prQuadTree.insert(new KDPoint(1, -2));
    	ArrayList<KDPoint> ptsWithinRange = new ArrayList<>(prQuadTree.range(ZERO, 5));
    	
    	System.out.println(ptsWithinRange);
    	//how do i test getchidlren???
    }
    @Test public void testPRQuadTreeBasicNearestNeighbor() {
    	prQuadTree = new PRQuadTree(4, 2); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(-3, 2));
    	prQuadTree.insert(new KDPoint(1, -2));
    	KDPoint nn;
        nn = prQuadTree.nearestNeighbor(new KDPoint(2,4));
    	System.out.println(nn);
    }
    @Test public void testPRQuadTreeBasicKNearestNeighborBlackNode() {
    	prQuadTree = new PRQuadTree(4, 3); // Space from (-8, -8) to (8, 8), bucketing parameter = 2.
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(-3, 2));
    	prQuadTree.insert(new KDPoint(1, -2));
    	BoundedPriorityQueue<KDPoint> knnPoints = prQuadTree.kNearestNeighbors(2, new KDPoint(1, 1));
    	Iterator KNearestPoints = knnPoints.iterator();
    	while (KNearestPoints.hasNext()) {
    		System.out.println(KNearestPoints.next());
    	}
    }
    @Test public void testPRQuadTreeNoSplitting() {
    	prQuadTree = new PRQuadTree(1, 3); // k = 1, bucket = 3
    	prQuadTree.insert(new KDPoint(2, 4));
    	prQuadTree.insert(new KDPoint(-3, 2));
    	prQuadTree.insert(new KDPoint(1, -2));
    	prQuadTree.insert(new KDPoint(1, -3));
    	
    }
    @Test public void testPRQuadTreeAddAndRange() {
    	prQuadTree = new PRQuadTree(3, 2); // k = 1, bucket = 3
    	prQuadTree.insert(new KDPoint(-2, 4));
    	prQuadTree.insert(new KDPoint(0,0));
    	prQuadTree.insert(new KDPoint(1, -2));
    	
    	assertEquals(3, prQuadTree.count());
    }
    
    
    
}
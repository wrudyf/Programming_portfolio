package pqueue;

import org.junit.Test;
import pqueue.exceptions.InvalidCapacityException;
import pqueue.exceptions.InvalidPriorityException;
import pqueue.heaps.ArrayMinHeap;
import pqueue.heaps.EmptyHeapException;
import pqueue.heaps.LinkedMinHeap;
import pqueue.heaps.MinHeap;
import pqueue.priorityqueues.EmptyPriorityQueueException;
import pqueue.priorityqueues.LinearPriorityQueue;
import pqueue.priorityqueues.MinHeapPriorityQueue;
import pqueue.priorityqueues.PriorityQueue;

import static org.junit.Assert.*;

import java.util.Iterator;

/**
 * {@link StudentTests} is a {@code jUnit} testing library which you should extend with your own tests.
 *
 * @author  <a href="https://github.com/JasonFil">Jason Filippou</a> and --- YOUR NAME HERE! ----
 */
public class StudentTests {

    private static String throwableInfo(Throwable thrown){
        return "Caught a " + thrown.getClass().getSimpleName() +
                " with message: " + thrown.getMessage();
    }

    private MinHeap<String> myHeap;
    private PriorityQueue<String> myQueue;

    @Test
    public void initAndAddOneElement() throws InvalidPriorityException {
        try {
            myHeap = new ArrayMinHeap<>();
            myQueue = new MinHeapPriorityQueue<>();
        } catch(Throwable t){
            fail(throwableInfo(t));
        }
        assertTrue("After initialization, all MinHeap and PriorityQueue implementations should report that they are empty.",
                myHeap.isEmpty() && myQueue.isEmpty());
        assertTrue("After initialization, all MinHeap and PriorityQueue implementations should report a size of 0.",
                (myHeap.size() == 0) && (myQueue.size() == 0));
        myHeap.insert("Mary");
        assertEquals("After inserting an element, ArrayMinHeap instances should report a size of 1.", 1, myHeap.size());

        // MinHeap::enqueue() declares that it checks InvalidPriorityException if priority <= 0 (from the docs of MinHeap).
        // In this case, we know for sure that InvalidPriorityException should *not* be thrown, since priority = 2 >= 0.
        // To avoid cluttering a code with "dummy" try-catch blocks, we declare InvalidPriorityException as checked from
        // this test as well. This is why we have the throws declaration after the name of the test.
        myQueue.enqueue("Jason", 2);
        assertEquals("After inserting an element, MinHeapPriorityQueue instances should report a size of 1.", 1, myQueue.size());
    }

    // Here is one simple way to write tests that expect an Exception to be thrown. Another, more powerful method is to
    // use the class org.junit.rules.ExpectedException: https://junit.org/junit4/javadoc/4.12/org/junit/rules/ExpectedException.html
    @Test(expected = InvalidCapacityException.class)
    public void ensureInvalidCapacityExceptionThrown() throws InvalidCapacityException{
         myQueue = new LinearPriorityQueue<>(-2);
    }

    @Test(expected = InvalidPriorityException.class)
    public void ensureInvalidPriorityExceptionThrown() throws InvalidPriorityException, InvalidCapacityException{
        myQueue = new LinearPriorityQueue<>(4);
        myQueue.enqueue("Billy", -1);
    }

    @Test
    public void testEnqueingOrder() throws InvalidPriorityException, EmptyPriorityQueueException {
        myQueue = new MinHeapPriorityQueue<>();
        myQueue.enqueue("Ashish", 8);
        myQueue.enqueue("Diana", 2);        // Lower priority, so should be up front.
        myQueue.enqueue("Adam", 2);        // Same priority, but should be second because of FIFO.
        assertEquals("We were expecting Diana up front.", "Diana", myQueue.getFirst());
    }

    @Test
    public void testDequeuingOrder() throws InvalidPriorityException, EmptyPriorityQueueException {
        testEnqueingOrder();    // To populate myQueue with the same elements.
        myQueue.dequeue();      // Now Adam should be up front.
        assertEquals("We were expecting Adam up front.", "Adam", myQueue.getFirst());
    }

    /* ******************************************************************************************************** */
    /* ********************** YOU SHOULD ADD TO THESE UNIT TESTS BELOW. *************************************** */
    /* ******************************************************************************************************** */
    
    //ARRAY MINHEAP TESTS <---------------------------------------------------------------------------------
    @Test
    //check to see that array is empty by checking that we make an array with size 0
    public void testArrayMinHeapSize0() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	assertEquals(0, myHeap2.size());
    }
    
    @Test
    //check to see that isempty works
    public void testArrayEmpty() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	assertEquals(true, myHeap2.isEmpty());
    }
    
    @Test
    //check to see that size is 1 after we insert 1 element
    public void testArrayMinHeapSize1() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(5);
    	assertEquals(1, myHeap2.size());
    }
    
    @Test(expected = EmptyHeapException.class) 
    //check to see that our minimum is at our root (index 0)
    public void testArrayMinHeapMin() throws EmptyHeapException {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(5);
    	boolean check;
    	if (5 == myHeap2.getMin()) {
    		check = true;
    	}
    	else {
    		check = false;
    	}
    	
    }
    @Test 
    //check to see that our minimum is at our root (index 0)
    public void testArrayMinHeapMinEmpty() throws EmptyHeapException {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	//myHeap2.insert(5);
    	boolean check;
    	if (5 == myHeap2.getMin()) {
    		check = true;
    	}
    	else {
    		check = false;
    	}
    	assertEquals(true, check);
    }
    
    @Test
    //check to test isEmpty
    public void testArrayMinHeapMinAdding() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(10);
    	myHeap2.insert(5);
    	myHeap2.insert(11);
    	myHeap2.insert(1);
    	try {
    		System.out.println(myHeap2.getMin());
    	}
    	catch (Exception e){
    		System.out.println("error, empty minheap");
    	}
    }
    
    @Test
    //check to see that deletemin works
    public void testArrayMinHeapMinAddingGetMinKey() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(8);
    	myHeap2.insert(10);
    	myHeap2.insert(7);
    	myHeap2.insert(9);
    	myHeap2.insert(3);
    	try {
    		//System.out.println("test");
    		//System.out.println( myHeap2.deleteMin());
    		System.out.println("Min is " + myHeap2.getMin());
    		System.out.println("new min is " + myHeap2.deleteMin());
    		System.out.println("Min is " + myHeap2.getMin());
    		System.out.println("new size is " + myHeap2.size());
    	}
    	catch (Exception e){
    		System.out.println("error, empty minheap");
    	}
    }
    @Test
    public void testArrayMinHeapMinComparingTwoHeaps() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(8);
    	myHeap2.insert(10);
    	myHeap2.insert(7);
    	
    	MinHeap<Integer> myHeap3;
    	myHeap3 = new ArrayMinHeap<>();
    	myHeap3.insert(8);
    	myHeap3.insert(10);
    	myHeap3.insert(7);
    	assertEquals(true, myHeap2.equals(myHeap3));
    }
    @Test
    public void testArrayMinHeapMinIterator() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(8);
    	myHeap2.insert(10);
    	myHeap2.insert(7);
    	
    	Iterator it3 = myHeap2.iterator();
    	while (it3.hasNext()) {
    		System.out.println(it3.next());
    	}
    }
    @Test
    public void testArrayMinHeapMinCopy() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(8);
    	myHeap2.insert(10);
    	myHeap2.insert(7);
    	
    	MinHeap<Integer> myHeap3;
    	myHeap3 = new ArrayMinHeap<>(myHeap2);
    	Iterator it3 = myHeap3.iterator();
    	while (it3.hasNext()) {
    		System.out.println(it3.next());
    	}
    }
    //have to make sure to follow heap property for array min heap iterator
    @Test
    public void testArrayMinHeapIterator() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(0);
    	myHeap2.insert(1);
    	myHeap2.insert(2);
    	myHeap2.insert(3);
    	myHeap2.insert(4);
    	myHeap2.insert(5);
    	myHeap2.insert(6);
    	Iterator it3 = myHeap2.iterator();
    	while (it3.hasNext()) {
    		System.out.println(it3.next());
    	}
    }
    
    //concurrent modification error ok
    @Test
    public void testArrayMinHeapIteratorConcurrentModError() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(0);
    	myHeap2.insert(1);
    	myHeap2.insert(2);
    	myHeap2.insert(3);
    	myHeap2.insert(4);
    	myHeap2.insert(5);
    	myHeap2.insert(6);
    	Iterator it3 = myHeap2.iterator();
    	while (it3.hasNext()) {
    		int x = (int) it3.next();
    		if (x == 3) {
    			try {
					myHeap2.deleteMin();
				} catch (EmptyHeapException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
    		}
    		System.out.println(x);
    	}
    }
    
    @Test
    public void testArrayMinHeapIteratorOrderTest() {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new ArrayMinHeap<>();
    	myHeap2.insert(0);
    	myHeap2.insert(1);
    	myHeap2.insert(2);
    	myHeap2.insert(3);
    	myHeap2.insert(6);
    	myHeap2.insert(5);
    	myHeap2.insert(4);
    	Iterator it3 = myHeap2.iterator();
    	while (it3.hasNext()) {
    		System.out.println(it3.next());
    	}
    }
    //NOTE: FOR MINHEAP REMEMBER FAIL FAST ITERATOR...SHOULD WE THROW ERROR 
    //IF WHEN ITERATING SOMEONE CHANGES SOMETHING IN THE DATA STRUCT???
    //<------------------------------------------------------------------end of array minheap
    
    //LINKED MINHEAP TESTS <-------------------------------------------------------------------------------
    @Test
    public void testLinkedMinHeapConstructor() {
    	MinHeap<Integer> myHeap;
    	myHeap = new LinkedMinHeap<>();
    	//linked minheap should be empty
    	assertEquals(true, myHeap.isEmpty());
    }
    @Test
    public void testLinkedMinHeapConstructorNotEmpty() {
    	MinHeap<Integer> myHeap;
    	myHeap = new LinkedMinHeap<>(5);
    	//linked minheap should NOT be empty
    	assertEquals(false, myHeap.isEmpty());
    }
    @Test
    public void testLinkedMinHeapInsert() {
    	MinHeap<Integer> myHeap;
    	myHeap = new LinkedMinHeap<>();
    	myHeap.insert(4);
    	//linked minheap should NOT be empty
    	assertEquals(false, myHeap.isEmpty());
    }
    @Test
    public void testLinkedMinHeapInsertCompleteProperty() {
    	MinHeap<Integer> myHeap;
    	myHeap = new LinkedMinHeap<>();
    }
    
    @Test
    //check to see insert for minheap linked
    public void testLinkedMinHeapAdd() throws EmptyHeapException {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new LinkedMinHeap<>();
    	myHeap2.insert(9);
    	myHeap2.insert(8);
    	myHeap2.insert(5);
    	//myHeap2.insert(3);
    	//myHeap2.insert(15);
    	Iterator it1 = myHeap2.iterator();
    	while (it1.hasNext()) {
    		System.out.println(it1.next());
    	}
    }
    @Test
    //check to see insert for minheap linked
    //NEED TO DEBUG DELETEMIN...
    public void testLinkedMinHeapAddDelete() throws EmptyHeapException {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new LinkedMinHeap<>();
    	myHeap2.insert(9);
    	myHeap2.insert(10);
    	myHeap2.insert(11);
    	myHeap2.insert(5);
    	
    	//myHeap2.insert(3);
    	//myHeap2.insert(15);
    	System.out.println("testing");
    	System.out.println(myHeap2.deleteMin());
    	System.out.println(myHeap2.deleteMin());
    	System.out.println(myHeap2.deleteMin());
    	System.out.println(myHeap2.deleteMin());
    	    	
    	System.out.println("testing again");
    	//ok i finally debugged delete min I think	
    }
    @Test
    
    public void testLinkedMinHeapAddOrder() throws EmptyHeapException {
    	MinHeap<Integer> myHeap2;
    	myHeap2 = new LinkedMinHeap<>();
    	myHeap2.insert(1);
    	myHeap2.insert(2);
    	myHeap2.insert(3);
    	myHeap2.insert(4);
    	
    	Iterator itx = myHeap2.iterator();
    	while (itx.hasNext()) {
    		System.out.println(itx.next());
    	}
    }
   //END OF LINKED MINHEAP TESTS <------------------------------------------------------------------------ 
    
  //LINEAR PRIORITY QUEUE TESTS <---------------------------------------------------------------------------- 
    @Test
    public void testLinearPriorityQueueempty() {
    	//private PriorityQueue<String> myQueue;
    	PriorityQueue<String> myQueue;
    	myQueue = new LinearPriorityQueue<>();
    	assertEquals(true, myQueue.isEmpty());
    }
    
    @Test
    public void testLinearPriorityQueuesize0() {
    	//private PriorityQueue<String> myQueue;
    	PriorityQueue<String> myQueue;
    	myQueue = new LinearPriorityQueue<>();
    	assertEquals(0, myQueue.size());
    }
    
    @Test
    public void testLinearPriorityQueuesizeAdding() {
    	//private PriorityQueue<String> myQueue;
    	PriorityQueue<String> myQueue;
    	myQueue = new LinearPriorityQueue<>();
    	assertEquals(0, myQueue.size());
    }
    
    @Test
    public void testLinearPriorityQueueAddingToQueue() throws InvalidPriorityException, InvalidCapacityException, EmptyPriorityQueueException {
    	myQueue = new LinearPriorityQueue<>();
        myQueue.enqueue("Ashish", -8);           
    }
    
    @Test
    public void testLinearPriorityQueueAddingToQueueTest() throws InvalidPriorityException, InvalidCapacityException, EmptyPriorityQueueException {
    	myQueue = new LinearPriorityQueue<>();
        myQueue.enqueue("Ashish", 4);       
        myQueue.enqueue("Timmy", 3);
        myQueue.enqueue("Tony", 5);
        myQueue.enqueue("Tommyy", 5);
        System.out.println("testing");
    }
    @Test
    public void testLinearPriorityQueueIterator() throws InvalidPriorityException  {
    	myQueue = new LinearPriorityQueue<>();
        myQueue.enqueue("Ashish", 4);
        myQueue.enqueue("Timmy", 3);
        myQueue.enqueue("Tony", 5);
        myQueue.enqueue("Tommyy", 5);
        Iterator it = myQueue.iterator();
        while (it.hasNext()) {
        	System.out.println("item: " + it.next());
        }
    }
    
    //MINHEAP PRIORITY QUEUE TESTS BELOW
    @Test
    public void testMinHeapPQ() throws InvalidPriorityException {
    	myQueue = new MinHeapPriorityQueue<>();
    	myQueue.enqueue("Ashish", 4);
        myQueue.enqueue("Timmy", 3);
        myQueue.enqueue("Tony", 5);
        myQueue.enqueue("Tommyy", 5);
        Iterator it = myQueue.iterator();
        //ok so issue here might be the way we make the iterator in our minheap
        //basically we sort it...but that might not be right
        //so i have to go over the iterator from linkedminheap again to make sure
        //that we add the elements in the right order with our iterator for the pq minheap implementation
        
        while (it.hasNext()) {
        	System.out.println("item: " + it.next());
        }
    }
    
    
}
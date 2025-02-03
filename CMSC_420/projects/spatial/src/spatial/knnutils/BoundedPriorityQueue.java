package spatial.knnutils;

import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Iterator;

import spatial.exceptions.UnimplementedMethodException;


/**
 * <p>{@link BoundedPriorityQueue} is a priority queue whose number of elements
 * is bounded. Insertions are such that if the queue's provided capacity is surpassed,
 * its length is not expanded, but rather the maximum priority element is ejected
 * (which could be the element just attempted to be enqueued).</p>
 *
 * <p><b>YOU ***** MUST ***** IMPLEMENT THIS CLASS!</b></p>
 *
 * @author  <a href = "https://github.com/jasonfillipou/">Jason Filippou</a>
 *
 * @see PriorityQueue
 * @see PriorityQueueNode
 */
public class BoundedPriorityQueue<T> implements PriorityQueue<T>{

	/* *********************************************************************** */
	/* *************  PLACE YOUR PRIVATE FIELDS AND METHODS HERE: ************ */
	/* *********************************************************************** */
	int size;
	int elements;
	PriorityQueueNode[] items;

	/* *********************************************************************** */
	/* ***************  IMPLEMENT THE FOLLOWING PUBLIC METHODS:  ************ */
	/* *********************************************************************** */

	/**
	 * Constructor that specifies the size of our queue.
	 * @param size The static size of the {@link BoundedPriorityQueue}. Has to be a positive integer.
	 * @throws IllegalArgumentException if size is not a strictly positive integer.
	 */
	public BoundedPriorityQueue(int size) throws IllegalArgumentException{
		if (size <= 0) {
			throw new IllegalArgumentException();
		}
		this.size = size;
		elements = 0;
		items = new PriorityQueueNode[size];
	}

	/**
	 * <p>Enqueueing elements for BoundedPriorityQueues works a little bit differently from general case
	 * PriorityQueues. If the queue is not at capacity, the element is inserted at its
	 * appropriate location in the sequence. On the other hand, if the object is at capacity, the element is
	 * inserted in its appropriate spot in the sequence (if such a spot exists, based on its priority) and
	 * the maximum priority element is ejected from the structure.</p>
	 * 
	 * @param element The element to insert in the queue.
	 * @param priority The priority of the element to insert in the queue.
	 */
	@Override
	public void enqueue(T element, double priority) {
		//so, we need to insert in proper order like in priority queue
		//except if queue is full, then eject last element
		//also have to take into account insertion order 
		//smaller priorities go at the front of the queue, higher priorities go to the back
		elements ++;
		//if we go over our size, have to reset it 
		if (elements > size) {
			elements = size;
		}
		int orderInserted = 0;
		int i = 0;
		while (i < elements) {
			if (items[i] != null) {
				if (items[i].getPriority() == priority) {
					orderInserted ++;
				}
			}
			i++;
		}
		
		PriorityQueueNode x = new PriorityQueueNode(element, priority, orderInserted);
		i = 0;
		while (i < elements) {
			//if we are at null space, just insert
			if (items[i] == null) {
				items[i] = x;
				break;
			}
			//if we are NOT at some null space, check priority
			else {
				//if current item is greater
				if (items[i].compareTo(x) > 0) {
					//if our non null priority queue node returns something greater than 0
					//that means our PRIORITY IS greater than what we want to insert
					//so we have to insert node here and then shift everything over to the right
					
					//first shift everything over to the right from this point on
					int j = elements - 1;
					while (j > i) {
						items[j] = items [j - 1];
						j --;
					}
					//now that everything is shifted to the right, we have to insert our element here
					items[i] = x;
					break;
				}
				//else we just continue with the loop
			}
			i++;
		}
	}

	@Override
	public T dequeue() {
		if (elements == 0) {
			return null;
		}
		//get first element 
		T first = (T) items[0].getData();
		//shift elements to the left
		for (int i = 0; i < elements - 1; i ++) {
			items[i] = items[i + 1];
		}
		items[elements - 1] = null;
		elements --;
		//give back first element
		return first;
	}

	@Override
	public T first() {
		if (elements == 0) {
			return null;
		}
		return (T) items[0].getData();
	}
	
	/**
	 * Returns the last element in the queue. Useful for cases where we want to 
	 * compare the priorities of a given quantity with the maximum priority of 
	 * our stored quantities. In a minheap-based implementation of any {@link PriorityQueue},
	 * this operation would scan O(n) nodes and O(nlogn) links. In an array-based implementation,
	 * it takes constant time.
	 * @return The maximum priority element in our queue, or null if the queue is empty.
	 */
	public T last() {
		if (elements == 0) {
			return null;
		}
		return (T) items[elements - 1].getData();
	}

	/**
	 * Inspects whether a given element is in the queue. O(N) complexity.
	 * @param element The element to throw new UnimplementedMethodException(); // ERASE THIS LINE AFTER YOU IMPLEMENT THIS METHOD!search for.
	 * @return {@code true} iff {@code element} is in {@code this}, {@code false} otherwise.
	 */
	public boolean contains(T element)
	{
		int i = 0;
		while (i < elements) {
			if (items[i].getData().equals(element)) {
				return true;
			}
			i ++;
		}
		return false;
	}
	
	public double getLastPriority() {
		if (elements == 0) {
			return -1;
		}
		return items[elements - 1].getPriority();
	}

	@Override
	public int size() {
		return elements;
	}

	@Override
	public boolean isEmpty() {
		if (elements == 0) {
			return true;
		}
		else {
			return false;
		}
	}

	@Override
	public Iterator<T> iterator() {
		return new bpq_iterator<T>();
	}
	private class bpq_iterator<T> implements Iterator<T>{
		ArrayList items_it = new ArrayList<T>();
		int index, elems;
		public bpq_iterator() {
			index = -1;
			elems = elements;
			for (int i = 0; i < elems; i ++) {
				items_it.add(items[i].getData());
			}
		}
		
		@Override
		public boolean hasNext() {
    		if (index < elems - 1) {
    			return true;
    		}
    		else {
    			return false;
    		}
    	}
    	@Override 
    	public T next() {
    		if (elems != elements) {
    			throw new ConcurrentModificationException();
    		}
    		index = index + 1;
    		return (T) items_it.get(index);
    	}
	}
}

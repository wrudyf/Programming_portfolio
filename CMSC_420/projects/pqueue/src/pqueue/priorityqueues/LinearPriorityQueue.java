package pqueue.priorityqueues; // ******* <---  DO NOT ERASE THIS LINE!!!! *******

/* *****************************************************************************************
 * THE FOLLOWING IMPORTS ARE HERE ONLY TO MAKE THE JAVADOC AND iterator() METHOD SIGNATURE
 * "SEE" THE RELEVANT CLASSES. SOME OF THOSE IMPORTS MIGHT *NOT* BE NEEDED BY YOUR OWN
 * IMPLEMENTATION, AND IT IS COMPLETELY FINE TO ERASE THEM. THE CHOICE IS YOURS.
 * ********************************************************************************** */

import demos.GenericArrays;
import pqueue.exceptions.*;
import pqueue.fifoqueues.FIFOQueue;
import pqueue.heaps.ArrayMinHeap;

import java.util.*;
/**
 * <p>{@link LinearPriorityQueue} is a {@link PriorityQueue} implemented as a linear {@link java.util.Collection}
 * of common {@link FIFOQueue}s, where the {@link FIFOQueue}s themselves hold objects
 * with the same priority (in the order they were inserted).</p>
 *
 * <p>You  <b>must</b> implement the methods in this file! To receive <b>any credit</b> for the unit tests related to
 * this class, your implementation <b>must</b>  use <b>whichever</b> linear {@link Collection} you want (e.g
 * {@link ArrayList}, {@link LinkedList}, {@link java.util.Queue}), or even the various {@link List} and {@link FIFOQueue}
 * implementations that we provide for you. You can also use <b>raw</b> arrays, but take a look at {@link GenericArrays}
 * if you intend to do so. Note that, unlike {@link ArrayMinHeap}, we do not insist that you use a contiguous storage
 * {@link Collection}, but any one available (including {@link LinkedList}) </p>
 *
 * @param <T> The type held by the container.
 *
 * @author  ---- YOUR NAME HERE ----
 *
 * @see MinHeapPriorityQueue
 * @see PriorityQueue
 * @see GenericArrays
 */
public class LinearPriorityQueue<T> implements PriorityQueue<T> {

	/* ***********************************************************************************
	 * Write any private data elements or private methods for LinearPriorityQueue here...*
	 * ***********************************************************************************/
	private class LinkedNode {
		private T element;
		private int priority;
		private LinkedNode next;
	}
	
	private LinkedNode head;
	//private int capacity;

	/* *********************************************************************************************************
	 * Implement the following public methods. You should erase the throwings of UnimplementedMethodExceptions.*
	 ***********************************************************************************************************/

	/**
	 * Default constructor initializes the element structure with
	 * a default capacity. This default capacity will be the default capacity of the
	 * underlying element structure that you will choose to use to implement this class.
	 */
	public LinearPriorityQueue(){
		
		//capacity = 0;
		head = null;
	}

	/**
	 * Non-default constructor initializes the element structure with
	 * the provided capacity. This provided capacity will need to be passed to the default capacity
	 * of the underlying element structure that you will choose to use to implement this class.
	 * @see #LinearPriorityQueue()
	 * @param capacity The initial capacity to endow your inner implementation with.
	 * @throws InvalidCapacityException if the capacity provided is less than 1.
	 */
	public LinearPriorityQueue(int capacity) throws InvalidCapacityException{	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		//how to throw error
		/*
		if (capacity < 1) {
			Throw new InvalidCapacityException();
		}
		*/
		
		//this.capacity = capacity;
		head = null;
	}

	@Override
	public void enqueue(T element, int priority) throws InvalidPriorityException{	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		if (priority < 1) {
			//throw exception here if priority is less than 1, one error here
			throw new InvalidPriorityException("invalid priority, please insert positive integer");
		}
		if (head == null) {
			LinkedNode x = new LinkedNode();
			x.element = element;
			x.priority = priority;
			x.next = null;
			head = x;
			return;
		}
		else {
			enqueue_aux(element, priority, head);
		}
	}
	public void enqueue_aux(T element, int priority, LinkedNode curr) {
		//so we have two options
		//either our priority is strictly less than current node's priority
		if (priority < curr.priority) {
			//if it's less than, we make a new node
			LinkedNode xn = new LinkedNode();
			xn.element = curr.element;
			xn.priority = curr.priority;
			xn.next = curr.next;
			curr.element = element;
			curr.priority = priority;
			curr.next = xn;
			return;
		} 
		else {
			//if it's greater than, we have two cases to worry about here
			if (curr.next == null) {
				//if we are at the end, then just make a new node
				LinkedNode xn = new LinkedNode();
				xn.element = element;
				xn.priority = priority;
				curr.next = xn;
				return;
			}
			enqueue_aux(element, priority, curr.next);
		}
	}

	@Override
	public T dequeue() throws EmptyPriorityQueueException { 	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		T tmp = head.element;
		head = head.next;
		return tmp;
	}

	@Override
	public T getFirst() throws EmptyPriorityQueueException {	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		return head.element;
	}

	@Override
	public int size() {
		if (head == null) {
			return 0;
		}
		else {
			return 1 + aux_size(head.next);
		}
	}
	
	private int aux_size(LinkedNode x) {
		if (x == null) {
			return 0;
		}
		else {
			return 1 + aux_size(x.next);
		}
	}

	@Override
	public boolean isEmpty() {
		if (head == null) {
			return true;
		}
		else {
			return false;
		}
	}

	@Override
	public Iterator<T> iterator() {
		LinearPQIterator x = new LinearPQIterator();
		return x;
	}
	private class LinearPQIterator implements Iterator<T> {
		int index = -1;
		private Object[] it_data;
		int it_size;
		public LinearPQIterator() {
			it_data = new Object[size()];
			int ind = 0;
			LinkedNode x = head;
			while (x != null) {
				it_data[ind] = x.element;
				ind ++;
				x = x.next;
			}
			it_size = size();
		}
		@Override
		public boolean hasNext() {
			if (index < size() - 1) {
				return true;
			} else {
				return false;
			}
		}
		@Override 
		public T next() {
			if (size() != it_size) {
				throw new ConcurrentModificationException("Modified original structure");
			}
			index = index + 1;
			return (T) it_data[index];
		}
	}
}
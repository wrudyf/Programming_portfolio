package pqueue.priorityqueues; // ******* <---  DO NOT ERASE THIS LINE!!!! *******


/* *****************************************************************************************
 * THE FOLLOWING IMPORTS WILL BE NEEDED BY YOUR CODE, BECAUSE WE REQUIRE THAT YOU USE
 * ANY ONE OF YOUR EXISTING MINHEAP IMPLEMENTATIONS TO IMPLEMENT THIS CLASS. TO ACCESS
 * YOUR MINHEAP'S METHODS YOU NEED THEIR SIGNATURES, WHICH ARE DECLARED IN THE MINHEAP
 * INTERFACE. ALSO, SINCE THE PRIORITYQUEUE INTERFACE THAT YOU EXTEND IS ITERABLE, THE IMPORT OF ITERATOR
 * IS NEEDED IN ORDER TO MAKE YOUR CODE COMPILABLE. THE IMPLEMENTATIONS OF CHECKED EXCEPTIONS
 * ARE ALSO MADE VISIBLE BY VIRTUE OF THESE IMPORTS.
 ** ********************************************************************************* */

import pqueue.exceptions.*;
import pqueue.heaps.ArrayMinHeap;
import pqueue.heaps.EmptyHeapException;
import pqueue.heaps.MinHeap;
import pqueue.heaps.LinkedMinHeap;
import pqueue.priorityqueues.MinHeapPriorityQueue;

import java.util.ConcurrentModificationException;
import java.util.Iterator;
/**
 * <p>{@link MinHeapPriorityQueue} is a {@link PriorityQueue} implemented using a {@link MinHeap}.</p>
 *
 * <p>You  <b>must</b> implement the methods of this class! To receive <b>any credit</b> for the unit tests
 * related to this class, your implementation <b>must</b> use <b>whichever</b> {@link MinHeap} implementation
 * among the two that you should have implemented you choose!</p>
 *
 * @author  ---- YOUR NAME HERE ----
 *
 * @param <T> The Type held by the container.
 *
 * @see LinearPriorityQueue
 * @see MinHeap
 * @see PriorityQueue
 */
public class MinHeapPriorityQueue<T> implements PriorityQueue<T>{

	/* ***********************************************************************************
	 * Write any private data elements or private methods for MinHeapPriorityQueue here...*
	 * ***********************************************************************************/
	//MinHeap mostly gets integers...and we want to order things by priorities
	MinHeap myHeap;
	//so what we will have to do is encapsulate our element and priority in an object of some sort
	private class Node implements Comparable<Node>{
		private T element;
		private int priority;
		
		public Node (T element, int priority) {
			this.element = element;
			this.priority = priority;
		}
		private int getPriority() {
			return priority;
		}
		private T getData() {
			return element;
		}
		
		//3, "a"
		//3, "b"
		//let it compare strings
		@Override
		public int compareTo(Node o) {
			// TODO Auto-generated method stub
			if (this.priority < o.getPriority()) {
				return -1;
			}
			else if (this.priority > o.getPriority()) {
				return 1;
			}
			else {
				return 0;
			}
		}
	}
	


	/* *********************************************************************************************************
	 * Implement the following public methods. You should erase the throwings of UnimplementedMethodExceptions.*
	 ***********************************************************************************************************/
		/**
	 * Simple default constructor.
	 */
	public MinHeapPriorityQueue(){
		myHeap = new LinkedMinHeap<>();
		
	}

	@Override
	public void enqueue(T element, int priority) throws InvalidPriorityException {	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		if (priority < 1) {
			throw new InvalidPriorityException("invalid priority, please insert positive integer");
		}
		Node x = new Node(element, priority);
		myHeap.insert(x);
	}

	@Override
	public T dequeue() throws EmptyPriorityQueueException {		// DO *NOT* ERASE THE "THROWS" DECLARATION!
		Node x = null;
		try {
			x = (Node) myHeap.deleteMin();
		} catch (EmptyHeapException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return x.getData();
	}

	@Override
	public T getFirst() throws EmptyPriorityQueueException {	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		Node elem = null;
		try {
			elem = (Node) myHeap.getMin();
		} catch (EmptyHeapException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return elem.getData();
	}

	@Override
	public int size() {
		return myHeap.size();
	}

	@Override
	public boolean isEmpty() {
		return myHeap.isEmpty();
	}


	@Override
	public Iterator<T> iterator() {
		MinHeapPQIterator x = new MinHeapPQIterator();
		return x;
	}
	private class MinHeapPQIterator implements Iterator<T> {
		int index = -1;
		private Object[] it_data;
		int it_size;
		public MinHeapPQIterator(){
			this.it_size = size();
			//so idea here will be this
			it_data = new Object[it_size];
			Iterator itx = myHeap.iterator();
			int cindex = 0;
			while (itx.hasNext()) {
				it_data[cindex] = itx.next();
				cindex ++;
			}
		}
		@Override
		public boolean hasNext() {
			if (index < it_size - 1) {
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
			Node x = (Node) it_data[index];
			return x.getData();
		}
	}
}
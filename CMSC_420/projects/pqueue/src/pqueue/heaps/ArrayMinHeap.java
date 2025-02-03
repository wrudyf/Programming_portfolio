package pqueue.heaps; // ******* <---  DO NOT ERASE THIS LINE!!!! *******

/* *****************************************************************************************
 * THE FOLLOWING IMPORT IS NECESSARY FOR THE ITERATOR() METHOD'S SIGNATURE. FOR THIS
 * REASON, YOU SHOULD NOT ERASE IT! YOUR CODE WILL BE UNCOMPILABLE IF YOU DO!
 * ********************************************************************************** */

import pqueue.exceptions.UnimplementedMethodException;

import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Iterator;


/**
 * <p>{@link ArrayMinHeap} is a {@link MinHeap} implemented using an internal array. Since heaps are <b>complete</b>
 * binary trees, using contiguous storage to store them is an excellent idea, since with such storage we avoid
 * wasting bytes per {@code null} pointer in a linked implementation.</p>
 *
 * <p>You <b>must</b> edit this class! To receive <b>any</b> credit for the unit tests related to this class,
 * your implementation <b>must</b> be a <b>contiguous storage</b> implementation based on a linear {@link java.util.Collection}
 * like an {@link java.util.ArrayList} or a {@link java.util.Vector} (but *not* a {@link java.util.LinkedList} because it's *not*
 * contiguous storage!). or a raw Java array. We provide an array for you to start with, but if you prefer, you can switch it to a
 * {@link java.util.Collection} as mentioned above. </p>
 *
 * @author -- YOUR NAME HERE ---
 *Rudy F
 *
 * @see MinHeap
 * @see LinkedMinHeap
 * @see demos.GenericArrays
 */

public class ArrayMinHeap<T extends Comparable<T>> implements MinHeap<T> {

	/* *****************************************************************************************************************
	 * This array will store your data. You may replace it with a linear Collection if you wish, but
	 * consult this class' 	 * JavaDocs before you do so. We allow you this option because if you aren't
	 * careful, you can end up having ClassCastExceptions thrown at you if you work with a raw array of Objects.
	 * See, the type T that this class contains needs to be Comparable with other types T, but Objects are at the top
	 * of the class hierarchy; they can't be Comparable, Iterable, Clonable, Serializable, etc. See GenericArrays.java
	 * under the package demos* for more information.
	 * *****************************************************************************************************************/
	private Object[] data;
	
	/* *********************************************************************************** *
	 * Write any further private data elements or private methods for LinkedMinHeap here...*
	 * *************************************************************************************/
	private int size = 0;
	
	/* *********************************************************************************************************
	 * Implement the following public methods. You should erase the throwings of UnimplementedMethodExceptions.*
	 ***********************************************************************************************************/

	/**
	 * Default constructor initializes the data structure with some default
	 * capacity you can choose.
	 */
	public ArrayMinHeap(){
		//this is ok
		data = new Object[0];
		size = 0;
	}

	/**
	 *  Second, non-default constructor which provides the element with which to initialize the heap's root.
	 *  @param rootElement the element to create the root with.
	 */
	public ArrayMinHeap(T rootElement){
		//this is ok
		data = new Object[1];
		data[0] = rootElement;
		size = 1;
	}

	/**
	 * Copy constructor initializes {@code this} as a carbon copy of the {@link MinHeap} parameter.
	 *
	 * @param other The MinHeap object to base construction of the current object on.
	 */
	public ArrayMinHeap(MinHeap<T> other){
		//so idea here is this
		//make an iterator of the other
		Iterator oth = other.iterator();
		while (oth.hasNext()) {
			this.insert((T) oth.next());
		}
	}

	/**
	 * Standard {@code equals()} method. We provide it for you: DO NOT ERASE! Consider its implementation when implementing
	 * {@link #ArrayMinHeap(MinHeap)}.
	 * @return {@code true} if the current object and the parameter object
	 * are equal, with the code providing the equality contract.
	 * @see #ArrayMinHeap(MinHeap)
	 */
	@Override
	public boolean equals(Object other){
		if(other == null || !(other instanceof MinHeap))
			return false;
		Iterator itThis = iterator();
		Iterator itOther = ((MinHeap) other).iterator();
		while(itThis.hasNext())
			if(!itThis.next().equals(itOther.next()))
				return false;
		return !itOther.hasNext();
	}


	@Override
	public void insert(T element) {
		//make new array
		Object[] data2 = new Object[size + 1];
		//copy elements from our original array to new array
		for (int i = 0; i < size ; i++) {
			data2[i] = data[i];
		}
		//insert new element into last spot of our new copied array
		data2[size] = element;
		//increment size
		size += 1;
		//reassign our old array to be equal to new array
		data = data2;
		
		//curr is node we are at right now
		T curr = (T) data[size - 1];
		//parent is curr's parent node
		T parent = (T) data[(size - 2) / 2];
		//if the comparison between current and parent is less than 0, then that means parent is greater
		if (curr.compareTo(parent) <  0) {
			percolate_up(size - 1);
		}
	}

	private void percolate_up(int index) {
		
		//hold temporary value
		//this holds the last value inserted <---value to be percolated up
		T tmp = (T) data[index];
		//set current to the value of the parent
		data[index] = data[(index - 1) / 2];
		//set parent to the value of the current
		data[(index - 1) / 2] = tmp;
		int new_i = (index - 1) / 2;
		//check to see if we have to carry the swap further up
		T curr = (T) data[new_i];
		T parent = (T) data[(new_i - 1) / 2];
		if (curr.compareTo(parent) < 0) {
			percolate_up(new_i);
		}
	}
	
	@Override
	public T deleteMin() throws EmptyHeapException { // DO *NOT* ERASE THE "THROWS" DECLARATION!
		if (size == 0) {
			throw new EmptyHeapException("cannot call deleteMin, empty heap");
		}
		T min = (T) data[0];
		//delete min and copy right-most leaf to it
		data[0] = data[size - 1];
		size = size - 1;
		
		percolate_down(0);
		return min;
	}

	
	private void percolate_down(int index) {
		//current node
		T tmp = (T) data[index];
		int left_index = (index * 2) + 1;		//left index
		int right_index = (index * 2) + 2;	//right index
		boolean left_exists = false;
		boolean right_exists = false;
		
		//check to see if index is within bounds
		if (left_index <= (size - 1)) {
			left_exists = true;
		}
		if (right_index <= (size - 1)) {
			right_exists = true;
		}
		
		//if both exist, compare to each other and see which is smaller
		if (left_exists == true && right_exists == true) {
			
			T left = (T) data[left_index];
			T right = (T) data[right_index];
			//if left is smaller than the right, compare to left child since it's smaller
			if (left.compareTo(right) < 0) {
				//compare to left child
				if (left.compareTo(tmp) < 0) {
					//if left child is smaller, do swap and make another function call
					data[index] = data[left_index];
					data[left_index] = tmp;
					percolate_down(left_index);
				}
			}
			//right child is smaller here or equal, so compare it
			else {
				//compare to right child
				if (right.compareTo(tmp) < 0) {
					//if right child is smaller, do swap and make another function call
					data[index] = data[right_index];
					data[right_index] = tmp;
					percolate_down(right_index);
				}
			}
		}
		//if both don't exist, one exists
		else {
			
			//if left exists, then right doesn't exist
			if (left_exists == true) {
				T left = (T) data[left_index];
				//compare to left child
				if (left.compareTo(tmp) < 0) {
				//if smaller, then we swap
					data[index] = data[left_index];
					data[left_index] = tmp;
					percolate_down(left_index);
				}
			}
			//right exists here
			else if(right_exists == true){
				T right = (T) data[right_index];
				if (right.compareTo(tmp) < 0) {
					data[index] = data[right_index];
					data[left_index] = tmp;
					percolate_down(right_index);
				}
			}
		}
	}
		@Override
	public T getMin() throws EmptyHeapException {	// DO *NOT* ERASE THE "THROWS" DECLARATION!
		//this is ok
		if (size == 0) {
			throw new EmptyHeapException("cannot call getMin, empty heap");
		}
		return (T) data[0];
	}

	@Override
	public int size() {
		return size;
	}

	@Override
	public boolean isEmpty() {
		if (size == 0) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 * Standard equals() method.
	 * @return {@code true} if the current object and the parameter object
	 * are equal, with the code providing the equality contract.
	 */


	@Override
	public Iterator<T> iterator() {
		ArrayMinHeapIterator x = new ArrayMinHeapIterator();
		return x;
	}
	
	private class ArrayMinHeapIterator implements Iterator<T>{
		private Object[] it_data;
		private int size_it;
		int index = -1;
		public ArrayMinHeapIterator() {
			ArrayList indices = new ArrayList<>();
			it_data = new Object[size];
			this.size_it = size;
			for (int i = 0; i < size_it; i ++) {
				it_data[i] = data[i];
			}
			//just sort it
			for (int i = 0; i < size_it - 1; i ++) {
				T curr = (T) it_data[i];
				T next = (T) it_data[i + 1];
				if (curr.compareTo(next) > 0) {
					//swap
					T tmp = curr;
					it_data[i] = it_data[i + 1];
					it_data[i + 1] = curr;
					i = 0;
				}
			}
		}
		
		@Override
		public boolean hasNext() {
			if (this.index < this.it_data.length - 1 ) {
				return true;
			}
			else {
				return false;
			}
		}
		@Override 
		public T next() {
			if (size() != size_it) {
				throw new ConcurrentModificationException("Modified original structure");
			}
			index = index + 1;
			return (T) it_data[index];
		}
	}
}
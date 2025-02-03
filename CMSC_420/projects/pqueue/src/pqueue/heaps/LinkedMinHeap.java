package pqueue.heaps; // ******* <---  DO NOT ERASE THIS LINE!!!! *******

import java.util.ConcurrentModificationException;

/* *****************************************************************************************
 * THE FOLLOWING IMPORT IS NECESSARY FOR THE ITERATOR() METHOD'S SIGNATURE. FOR THIS
 * REASON, YOU SHOULD NOT ERASE IT! YOUR CODE WILL BE UNCOMPILABLE IF YOU DO!
 * ********************************************************************************** */

import java.util.Iterator;

import pqueue.exceptions.UnimplementedMethodException;

/**
 * <p>A {@link LinkedMinHeap} is a tree (specifically, a <b>complete</b> binary tree) where every node is
 * smaller than or equal to its descendants (as defined by the {@link Comparable#compareTo(Object)} overridings of the type T).
 * Percolation is employed when the root is deleted, and insertions guarantee maintenance of the heap property in logarithmic time. </p>
 *
 * <p>You <b>must</b> edit this class! To receive <b>any</b> credit for the unit tests related to this class,
 * your implementation <b>must</b> be a &quot;linked&quot;, <b>non-contiguous storage</b> implementation based on a
 * binary tree of nodes and references. Use the skeleton code we have provided to your advantage, but always remember
 * that the only functionality our tests can test is {@code public} functionality.</p>
 * 
 * @author --- Rudy F. ---
 *
 * @param <T> The {@link Comparable} type of object held by {@code this}.
 *
 * @see MinHeap
 * @see ArrayMinHeap
 */
public class LinkedMinHeap<T extends Comparable<T>> implements MinHeap<T> {

	/* ***********************************************************************
	 * An inner class representing a minheap's node. YOU *SHOULD* BUILD YOUR *
	 * IMPLEMENTATION ON TOP OF THIS CLASS!                                  *
 	 * ********************************************************************* */
	private class MinHeapNode {
		private T data;
		private MinHeapNode lChild, rChild;
        /* *******************************************************************
         * Write any further data elements or methods for MinHeapNode here...*
         ********************************************************************* */
		private MinHeapNode parent;
	}
	/* *********************************
	  * Root of your tree: DO NOT ERASE!
	  * *********************************
	 */
	private MinHeapNode root;

    /* *********************************************************************************** *
     * Write any further private data elements or private methods for LinkedMinHeap here...*
     * *************************************************************************************/
	private int size = 0;
	
    /* *********************************************************************************************************
     * Implement the following public methods. You should erase the throwings of UnimplementedMethodExceptions.*
     ***********************************************************************************************************/

	/**
	 * Default constructor.
	 */
	public LinkedMinHeap() {
		root = null;
	}

	/**
	 * Second constructor initializes {@code this} with the provided element.
	 *
	 * @param rootElement the data to create the root with.
	 */
	public LinkedMinHeap(T rootElement) {
		MinHeapNode x = new MinHeapNode();
		x.data = rootElement;
		x.lChild = null;
		x.rChild = null;
		x.parent = null;
		root = x;
		size ++;
	}

	/**
	 * Copy constructor initializes {@code this} as a carbon
	 * copy of the parameter, which is of the general type {@link MinHeap}!
	 * Since {@link MinHeap} is an {@link Iterable} type, we can access all
	 * of its elements in proper order and insert them into {@code this}.
	 *
	 * @param other The {@link MinHeap} to copy the elements from.
	 */
	public LinkedMinHeap(MinHeap<T> other) {
		throw new UnimplementedMethodException();
	}


    /**
     * Standard {@code equals} method. We provide this for you. DO NOT EDIT!
     * You should notice how the existence of an {@link Iterator} for {@link MinHeap}
     * allows us to access the elements of the argument reference. This should give you ideas
     * for {@link #LinkedMinHeap(MinHeap)}.
     * @return {@code true} If the parameter {@code Object} and the current MinHeap
     * are identical Objects.
     *
     * @see Object#equals(Object)
     * @see #LinkedMinHeap(MinHeap)
     */
	/**
	 * Standard equals() method.
	 *
	 * @return {@code true} If the parameter Object and the current MinHeap
	 * are identical Objects.
	 */
	@Override
	public boolean equals(Object other) {
		if (!(other instanceof MinHeap))
			return false;
		Iterator itThis = iterator();
		Iterator itOther = ((MinHeap) other).iterator();
		while (itThis.hasNext())
			if (!itThis.next().equals(itOther.next()))
				return false;
		return !itOther.hasNext();
	}

	@Override
	public boolean isEmpty() {
		if (root == null) {
			return true;
		}
		else {
			return false;
		}
	}
	public int size () {
		return size;
	}
	@Override
	public void insert(T element) {
		if (root == null) {
			MinHeapNode x = new MinHeapNode();
			x.data = element;
			x.lChild = null;
			x.rChild = null;
			x.parent = null;
			root = x;
			size ++;
		}
		else {
			//not the best way to insert but it's the best thing i could think of
			insert_aux2(element, root, 0, size);	
		}
	}
	private void insert_aux2(T element, MinHeapNode x, int currIndex, int targetIndex) {
		//ok so idea here is this
		//we have our element (data to be inserted), we have our current node we're at
		//we also have our currentIndex (currIndex) and the targetIndex
		//based off of our current index and target index, we will decide to traverse either the left or 
		//right tree to get to desired index
		//base case is if we reach null
		if (x != null) {
			//calculate left and right indices
			int leftIndex = currIndex * 2 + 1;
			int rightIndex = currIndex * 2 + 2;
			if (leftIndex == targetIndex) {
				//if our left index is target index, make new node
				MinHeapNode leaf = new MinHeapNode();
				leaf.data = element;
				leaf.lChild = null;
				leaf.rChild = null;
				leaf.parent = x;
				x.lChild = leaf;
				//now we call percolate_up method to see if we have to percolate up
				percolate_up(x.lChild);
				size ++;
				return;
			}
			if (rightIndex == targetIndex) {
				//if our right index is target index, make new node
				MinHeapNode leaf = new MinHeapNode();
				leaf.data = element;
				leaf.lChild = null;
				leaf.rChild = null;
				leaf.parent = x;
				x.rChild = leaf;
				//now we call percolate_up method to see if we have to percolate up
				percolate_up(x.rChild);
				size ++;
				return;
			}
			//if we didn't enter either of the previous branches, then we have to find our insertion spot
			//go left
			insert_aux2(element, x.lChild, leftIndex, targetIndex);
			//go right
			insert_aux2(element, x.rChild, rightIndex, targetIndex);
		}
	}
	private void percolate_up(MinHeapNode curr) {
		//so each node has a link to its parent node...so idea here will be this
		//we look at current node and compare it to parent node
		if (curr != null && curr.parent != null) {
			if ( curr.data.compareTo(curr.parent.data) < 0 ) {
				//if parent is STRICTLY LESS THAN child, then we swap
				//we swap their values
				T tmp = curr.data;
				curr.data = curr.parent.data;
				curr.parent.data = tmp;
				percolate_up(curr.parent);
			}
		}
	}
	
	@Override
	public T getMin() throws EmptyHeapException {		// DO *NOT* ERASE THE "THROWS" DECLARATION!
		if (root == null) {
			throw new EmptyHeapException("cannot call getMin linked, empty heap");
		}
		return root.data;
	}

	@Override
	public T deleteMin() throws EmptyHeapException {    // DO *NOT* ERASE THE "THROWS" DECLARATION!
		if (root == null) {
			throw new EmptyHeapException("cannot call getMin from linked, empty heap");
		}
		//ok so we want to get the min
		T tmp = root.data;
		if (root.lChild == null && root.rChild == null) {
			root = null;
			size --;
			return tmp;
		}
		getRightMost(root, 0, size - 1, root);
		deleteRightMost(root, 0, size - 1);
		//maybe i have an error with percolate down
		percolate_down(root);
		return tmp;
	}
	private void deleteRightMost(MinHeapNode x, int index, int targetIndex) {
		if (x != null) {
			int leftIndex = index * 2 + 1;
			int rightIndex = index * 2 + 2;
			if (leftIndex == targetIndex) {
				x.lChild = null;
				size --;
				return;
			}
			if (rightIndex == targetIndex) {
				x.rChild = null;
				size --;
				return;
			}
			deleteRightMost(x.lChild, leftIndex, targetIndex);
			deleteRightMost(x.rChild, rightIndex, targetIndex);
		}
	}
	private void getRightMost(MinHeapNode x, int index, int targetIndex, MinHeapNode y) {
		if (x != null) {
			int leftIndex = index * 2 + 1;
			int rightIndex = index * 2 + 2;
			if (leftIndex == targetIndex) {
				y.data = x.lChild.data ;
				return;
			} 
			else if (rightIndex == targetIndex) {
				y.data = x.rChild.data;
				return;
			} else {
				getRightMost(x.lChild, leftIndex, targetIndex, y);
				getRightMost(x.rChild, rightIndex, targetIndex, y);	
			}		
		}
	}
	private void percolate_down(MinHeapNode x) {
		//ok so idea here is this
		//we have to see which of our children is smaller and swap with our smaller child 
		//if our current node is bigger than child
		//we have a few cases to worry about here
		if (x != null) {
			//base case is null node
			if (x.lChild != null && x.rChild != null) {
				//if both left and right child are not null, then we check which one is smaller
				if (x.lChild.data.compareTo(x.rChild.data) < 0) {
					//if left child IS STRICTLY SMALLER, then check to see if left child is smaller than curr
					if (x.lChild.data.compareTo(x.data) < 0) {
						//IF IT IS LESS THAN, we swap
						T tmp = x.data;
						x.data = x.lChild.data;
						x.lChild.data = tmp;
						percolate_down(x.lChild);
						return;
					} 
				}
				else {
					//check right child since it's smaller than left child
					if (x.rChild.data.compareTo(x.data) < 0) {
						//if r child is smaller than current, swap
						T tmp = x.data;
						x.data = x.rChild.data;
						x.rChild.data = tmp;
						percolate_down(x.rChild);
						return;
					}
				}
			} 
			if (x.lChild != null && x.rChild == null) {
				//if left is not null but right is null, then check current to left child
				if (x.lChild.data.compareTo(x.data) < 0) {
					//if left child is strictly less than parent, swap
					T tmp = x.data;
					x.data  = x.lChild.data;
					x.lChild.data = tmp;
					percolate_down(x.lChild);
					return;
				}
			}
			if (x.rChild != null && x.lChild == null) {
				//if right is not null, but left is null
				if (x.rChild.data.compareTo(x.data) < 0) {
					T tmp = x.data;
					x.data = x.rChild.data;
					x.rChild.data = tmp;
					percolate_down(x.rChild);
					return;
				}
			}
		}
	}
	@Override
	public Iterator<T> iterator() {
		LinkedMinHeapIterator x = new LinkedMinHeapIterator();
		return x;
	}
	private class LinkedMinHeapIterator implements Iterator<T>{
		//i think i have an off by one error somewhere here
		private Object[] it_data;
		private int it_size;
		int index = -1;
		public LinkedMinHeapIterator() {
			it_data = new Object[size];
			init_aux(root, 0);
			this.it_size = size;
			//now just sort it_data
			for (int i = 0; i < it_size - 1; i ++) {
				T curr = (T) it_data[i];
				T next = (T) it_data[i + 1];
				if (curr.compareTo(next) > 0) {
					T tmp = (T) it_data[i];
					it_data[i] = it_data[i + 1];
					it_data[i + 1] = tmp;
					i = 0;
				}
			}
		}
		private void init_aux(MinHeapNode x, int position) {
			if (x != null ) {
				it_data[position] = x.data;
				init_aux(x.lChild, (position * 2 + 1));
				init_aux(x.rChild, (position * 2 + 2));	
			}
		}
		@Override
		public boolean hasNext() {
			//had a bug here i had to fix real quick, took some time to fix though...shame
			if (this.index < it_data.length - 1) {
				return true;
			}
			else {
				return false;
			}
		}
		@Override
		public T next() {
			if (it_size != size) {
				throw new ConcurrentModificationException("Modified original structure");
			}
			index = index + 1;
			return (T) it_data[index];
		}
	}
}
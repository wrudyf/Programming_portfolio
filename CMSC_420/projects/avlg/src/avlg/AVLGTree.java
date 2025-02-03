package avlg;

import avlg.exceptions.UnimplementedMethodException;
import avlg.exceptions.EmptyTreeException;
import avlg.exceptions.InvalidBalanceException;
import java.lang.Math;
/** <p>{@link AVLGTree}  is a class representing an <a href="https://en.wikipedia.org/wiki/AVL_tree">AVL Tree</a> with
 * a relaxed balance condition. Its constructor receives a strictly  positive parameter which controls the <b>maximum</b>
 * imbalance allowed on any subtree of the tree which it creates. So, for example:</p>
 *  <ul>
 *      <li>An AVL-1 tree is a classic AVL tree, which only allows for perfectly balanced binary
 *      subtrees (imbalance of 0 everywhere), or subtrees with a maximum imbalance of 1 (somewhere). </li>
 *      <li>An AVL-2 tree relaxes the criteria of AVL-1 trees, by also allowing for subtrees
 *      that have an imbalance of 2.</li>
 *      <li>AVL-3 trees allow an imbalance of 3.</li>
 *      <li>...</li>
 *  </ul>
 *
 *  <p>The idea behind AVL-G trees is that rotations cost time, so maybe we would be willing to
 *  accept bad search performance now and then if it would mean less rotations. On the other hand, increasing
 *  the balance parameter also means that we will be making <b>insertions</b> faster.</p>
 *
 * @author YOUR NAME HERE!
 *
 * @see EmptyTreeException
 * @see InvalidBalanceException
 * @see StudentTests
 */
public class AVLGTree<T extends Comparable<T>> {

    /* ********************************************************* *
     * Write any private data elements or private methods here...*
     * ********************************************************* */
	private class Node{
		private T key;
		private Node left, right;
	}
	private Node root;
	private int balance;

    /* ******************************************************** *
     * ************************ PUBLIC METHODS **************** *
     * ******************************************************** */

    /**
     * The class constructor provides the tree with the maximum imbalance allowed.
     * @param maxImbalance The maximum imbalance allowed by the AVL-G Tree.
     * @throws InvalidBalanceException if maxImbalance is a value smaller than 1.
     */
    public AVLGTree(int maxImbalance) throws InvalidBalanceException {
    	if (maxImbalance <= 0) {
    		throw new InvalidBalanceException("max imbalance can't be 0 or less");
    	}
        this.balance = maxImbalance;
        root = null;
    }

    /**
     * Insert key in the tree. You will <b>not</b> be tested on
     * duplicates! This means that in a deletion test, any key that has been
     * inserted and subsequently deleted should <b>not</b> be found in the tree!
     * s
     * @param key The key to insert in the tree.
     */
    public void insert(T key) {
        if (root == null) {
        	Node x = new Node();
        	x.key = key;
        	x.left = null;
        	x.right = null;
        	root = x;
        }
        else {
        	root = insert_aux(key, root);
        	//check to see if tree is NOT balanced
        	/*
        	if (!isAVLGBalanced()) {
        		//if not balanced, then do some processing
        		//DON'T DO TOP DOWN, FIX AT INSERTION, TOP DOWN GIVES DIFF RESULT
        		//at every insertion, do balance check to where you insert at leaf and work upwards
        		root = fix(root);
        	}*/
        }
    }
    private Node fix(Node x) {
    	//if left tree has more weight, then rotate right
    	if (height(x.left) - height(x.right) > 0 && (Math.abs(height(x.left) - height(x.right))) > balance) {
    		if (height(x.left.left) - height(x.left.right) < 0) {
    			//rotate subtree left first
    			Node y2 = x.left.right;
    			Node b2 = x.left.left;
    			y2.left = x.left;
    			x.left.right = b2;
    			x.left = null;
    			y2.right = x;
    			return y2;
    		}
    		else {
    			Node y = x.left;
    			Node b = x.left.right;
    			y.right = x;
    			x.left = b;
    			return y;
    		}
    	}
    	//if right tree has more weight, then rotate left
    	else if (height(x.left) - height(x.right) < 0 && (Math.abs(height(x.left) - height(x.right))) > balance) {
    		//if sign at current node differs from sign at right subtree, then do double rotation
    		if (height(x.right.left) - height(x.right.right) > 0) {
    			//rotate subtree right first
    			Node y2 = x.right.left;
    			Node b2 = x.right.left.right;
    			y2.right = x.right;
    			x.right.left = b2;
    			x.right = null;
    			y2.left = x;
    			return y2;
    			//then rotate left at current tree
    			/*
    			Node x2 = y2.right;
    			Node b3 = y2.right.left;
    			x2.left = y2;
    			x2.right = b3;
    			return x2;
    			*/
    		} 
    		else {
    			Node x2 = x.right;
    			Node b = x.right.left;
    			x2.left = x;
    			x.right = b;
    			return x2;
    		}
    	}
    	//if we don't have  a balance issue, then process left and right
    	else {
    		x.left = fix(x.left);
    		x.right = fix(x.right);
    	}
    	return x;
    }
    private Node rotateRight(Node x) {
    	Node tmp = x.left;
    	x.left = tmp.right;
    	tmp.right = x;
    	return tmp;
    }
    private Node rotateLeft(Node x) {
    	Node tmp = x.right;
    	x.right = tmp.left;
    	tmp.left = x;
    	return tmp;
    }
    private Node rotateLeftRight(Node x) {
    	x.left = rotateLeft(x.left);
    	x = rotateRight(x);
    	return x;
    }
    //check this one
    private Node rotateRightLeft(Node x) {
    	x.right = rotateRight(x.right);
    	x = rotateLeft(x);
    	return x;
    }
    
    private Node insert_aux(T key, Node x) {
    	if (x == null) {    		
    		Node y = new Node();
    		y.key = key;
    		y.left = null;
    		y.right = null;
    		return y;
    	}
    	else {
    		//if current node key is less than given key, then insert on right 
    		if (x.key.compareTo(key) <= 0) {
    			//check height here
    			x.right = insert_aux(key, x.right);
    			if ((Math.abs(height(x.left) - height(x.right))) > balance) {
    				if (key.compareTo(x.right.key) >= 0) {
    					x = rotateLeft(x);
    				}
    				else {
    					x = rotateRightLeft(x);
    				}
    			}
    		}
    		//else insert on left
    		else {    			
    			
    			x.left = insert_aux(key, x.left);
    			//check height here
    			if ((Math.abs(height(x.left) - height(x.right))) > balance) {
    				//compare key to node's left key
    				if (key.compareTo(x.left.key) < 0) {
    					//if less than, rotateRight
    					x = rotateRight(x);
    				}
    				else {
    					x = rotateLeftRight(x);
    				}
    			}    		
    		}
    	}
    	return x;
    }
    
    private int height(Node x) {
    	if (x == null) {
    		return -1;
    	}
    	else {
    		return 1 + max((height(x.left)), (height(x.right)));
    	}
    }

    private int max(int x, int y) {
		if (x > y) {
			return x;
		}
		else {
			return y;
		}
	}

	/**
     * Delete the key from the data structure and return it to the caller.
     * @param key The key to delete from the structure.
     * @return The key that was removed, or {@code null} if the key was not found.
     * @throws EmptyTreeException if the tree is empty.
     */
    public T delete(T key) throws EmptyTreeException {
        if (root == null) {
        	throw new EmptyTreeException("empty tree");
        }
        else {
        	if (search(key) == null) {
        		return null;
        	}
        	else {
        		if (root.left == null && root.right == null) {
        			T tmp2 = root.key;
        			root = null;
        			return tmp2;
        		}
        		T tmp = search(key);        		        		
        		root = delete_aux (key, root);
        		return tmp;
        	}
        }
    }

    private Node delete_aux(T key, Node x) {
    	//if base case is reached, return null
    	if (x == null) {
    		return null;
    	}
    	//if current node key is less than given key, then delete on right
    	if (x.key.compareTo(key) < 0) {
    		x.right = delete_aux(key, x.right);
    		//check height here
    		if ((Math.abs(height(x.left) - height(x.right))) > balance) {
    			//deleting on right, so then left tree must have more weight
    			//so that means if left has more weight, our current balance is gonna be positive
    			if ((height(x.left.left) - height(x.left.right)) < 0) {
    				x = rotateLeftRight(x);
    			}
    			else {
    				x = rotateRight(x);
    			}
    		}
    	}
    	//if current node key is greater than key, then delete on left
    	else if (x.key.compareTo(key) > 0) {
    		x.left = delete_aux(key, x.left);
    		//check height here
    		if ((Math.abs(height(x.left) - height(x.right))) > balance) {
    			//deleting on left, so then right tree must have more weight
    			//so that means if right has more weight, our current balance is gonna be negative
    			if ( (height(x.right.left)) - (height(x.right.right))  > 0) {
    				//if the balance of the right sub tree is greater than 0, do right left
    				x = rotateRightLeft(x);
    			}
    			else {
    				x = rotateLeft(x);
    			}
    		}
    	}
    	//it must be equal, so delete
    	else {
    		if (x.left == null) {
    			return x.right;
    		}
    		else if (x.right == null) {
    			return x.left;
    		}
    		Node ios = inorder_successor(x.right, null); 
    		x.key = ios.key;
    		//System.out.println("key to replace with: " + x.key);
    		x.right = delete_aux(x.key, x.right);
    		
    		//check height here...?
    		if ((Math.abs(height(x.left) - height(x.right))) > balance) {
    			
    		}
    	}
    	return x;
    }
    private Node inorder_successor(Node x, Node r) {
    	if (x == null) {
    		return r;
    	}
    	else {
    		return inorder_successor(x.left, x);
    	}
    }
    /**
     * <p>Search for key in the tree. Return a reference to it if it's in there,
     * or {@code null} otherwise.</p>
     * @param key The key to search for.
     * @return key if key is in the tree, or {@code null} otherwise.
     * @throws EmptyTreeException if the tree is empty.
     */
    public T search(T key) throws EmptyTreeException {
        if (root == null) {
        	throw new EmptyTreeException("empty tree");
        }
        else {
        	//if equal, return here
        	if (root.key.compareTo(key) == 0) {
        		return root.key;
        	}
        	//else if current key is less than given key, search on right
        	else if (root.key.compareTo(key) < 0) {
        		return aux_search(key, root.right);
        	}
        	//look on left
        	else {
        		return aux_search(key, root.left);
        	}
        }
    }
    public T aux_search(T key, Node x) {
    	if (x == null) {
    		return null;
    	}
    	else {
    		//if equal, just return key
    		if (x.key.compareTo(key) == 0) {
    			return x.key;
    		}
    		//if current node key is less than 
    		else if (x.key.compareTo(key) < 0) {
    			return aux_search(key, x.right);
    		}
    		else {
    			return aux_search(key, x.left);
    		}
    	}
    }

    /**
     * Retrieves the maximum imbalance parameter.
     * @return The maximum imbalance parameter provided as a constructor parameter.
     */
    public int getMaxImbalance(){
        return balance;
    }


    /**
     * <p>Return the height of the tree. The height of the tree is defined as the length of the
     * longest path between the root and the leaf level. By definition of path length, a
     * stub tree has a height of 0, and we define an empty tree to have a height of -1.</p>
     * @return The height of the tree. If the tree is empty, returns -1.
     */
    
    //want to get longest path from root to leaf, so what do we do?
    //look at height of left tree and look at height of right tree
    //if height of left tree is greater, then we do height of left tree plus height of node we are at
    //else we do height of 
    public int getHeight(){
    	if (root == null) {
    		//throw new EmptyTreeException("empty tree");
    		return -1;
    	}
        if (height(root.left) > height(root.right)){
        	return 1 + height(root.left);
        }
        else {
        	return 1 + height(root.right);
        }
    }

    /**
     * Query the tree for emptiness. A tree is empty iff it has zero keys stored.
     * @return {@code true} if the tree is empty, {@code false} otherwise.
     */
    public boolean isEmpty() {
        if (root == null) {
        	return true;
        }
        else {
        	return false;
        }
    }

    /**
     * Return the key at the tree's root node.
     * @return The key at the tree's root node.
     * @throws  EmptyTreeException if the tree is empty.
     */
    public T getRoot() throws EmptyTreeException{
    	if (root == null) {
    		throw new EmptyTreeException("empty tree");
    	}
        return root.key;
    }


    /**
     * <p>Establishes whether the AVL-G tree <em>globally</em> satisfies the BST condition. This method is
     * <b>terrifically useful for testing!</b></p>
     * @return {@code true} if the tree satisfies the Binary Search Tree property,
     * {@code false} otherwise.
     */
    //what does it mean to be a bst? it means that elements to the right of the root are greater
    //and elements to the left of the root are less
    //so we have to check this property
    public boolean isBST() {
        if (root == null) {
        	return true;
        }
        else {
        	return bst_check(root);
        }
    }
    
    //have to compare current node to left child and right child
    private boolean bst_check(Node x){
    	//base case is if we reach null, so return true at null
    	if (x == null) {
    		return true;
    	}
    	//if x is not null, then do some processing here
    	else {
    		//check to see if left is null
    		if (x.left != null) {
    			//branch to take if left is not null
    			//also check to see if right is null
    			if (x.right != null) {
    				//branch to take if right and left are not null
    				//if both are not null, then compare to both
    				if (x.key.compareTo(x.left.key) >= 0) {
    					//branch to take if current key is greater than left child
    					//then compare to right 
    					if (x.key.compareTo(x.right.key) <= 0) {
    						//branch to take if current is less than right child
    						return true && (bst_check(x.left)) && (bst_check(x.right));
    					}
    					else {
    						//not less, then return false
    						return false;
    					}
    				}
    				else {
    					//if not greater than left child, false
    					return false;
    				}
    			}
    			else {
    				//branch to take if left is not null and right is null
    				//compare current to left
    				if (x.key.compareTo(x.left.key) >= 0) {
    					//branch to take if current key is greater than left child key
    					return true && (bst_check(x.left));
    				}
    				else {
    					//branch to take if current is not greater than left child
    					return false;
    				}
    			}
    		}
    		else {
    			//branch to take if left is null
    			//check to see if right is null
    			if (x.right != null) {
    				//branch to take if right is not null
    				//compare current to right key
    				if (x.key.compareTo(x.right.key) <= 0) {
    					//if current key is less than right key, return true and do call on right child
    					return true && (bst_check(x.right));
    				}
    				else {
    					//if greater, just return false and this makes everything else false
    					return false;
    				}
    			}
    			else {
    				//if we reach this case where left is null and right is null, just return true
    				return true;
    			}    			
    		}
    	}
    }

    /**
     * <p>Establishes whether the AVL-G tree <em>globally</em> satisfies the AVL-G condition. This method is
     * <b>terrifically useful for testing!</b></p>
     * @return {@code true} if the tree satisfies the balance requirements of an AVLG tree, {@code false}
     * otherwise.
     */
    public boolean isAVLGBalanced() {
        if (root == null) {        	
        	return true;
        }
        else {
        	if ( Math.abs(height(root.left) - height(root.right)) <= balance) {
        		return true && isAVLGBalanced_Aux(root.left) && isAVLGBalanced_Aux(root.right); 
        	}
        	else {
        		return false;
        	}
        }
    }
    private boolean isAVLGBalanced_Aux(Node x) {
    	if (x == null) {
    		return true;
    	}
    	else {
    		if ( (height(root.left) - height(root.right)) <= balance) {
    			return true && isAVLGBalanced_Aux(x.left) && isAVLGBalanced_Aux(x.right); 
    		} else {
    			return false;
    		}
    	}
    }

    /**
     * <p>Empties the AVL-G Tree of all its elements. After a call to this method, the
     * tree should have <b>0</b> elements.</p>
     */
    public void clear(){
        root = null;
    }

    /**
     * <p>Return the number of elements in the tree.</p>
     * @return  The number of elements in the tree.
     */
    public int getCount(){
        if (root == null) {
        	return 0;
        }
        else {
        	
        	return 1 + aux_getCount(root.left) + aux_getCount(root.right);
        }
    }
    
    private int aux_getCount(Node x) {
    	if (x == null) {
    		return 0;
    	}
    	else {
    		
    		return 1 + aux_getCount(x.left) + aux_getCount(x.right);
    	}
    }
}

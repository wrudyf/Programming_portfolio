package bpt;

import bpt.UnimplementedMethodException;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * <p>{@code BinaryPatriciaTrie} is a Patricia Trie over the binary alphabet &#123;	 0, 1 &#125;. By restricting themselves
 * to this small but terrifically useful alphabet, Binary Patricia Tries combine all the positive
 * aspects of Patricia Tries while shedding the storage cost typically associated with tries that
 * deal with huge alphabets.</p>
 *
 * @author RUdy
 */
public class BinaryPatriciaTrie {

    /* We are giving you this class as an example of what your inner node might look like.
     * If you would prefer to use a size-2 array or hold other things in your nodes, please feel free
     * to do so. We can *guarantee* that a *correct* implementation exists with *exactly* this data
     * stored in the nodes.
     */
    private static class TrieNode {
        private TrieNode left, right;
        private String str;
        private boolean isKey;
        // Default constructor for your inner nodes.
        TrieNode() {
            this("", false);
        }

        // Non-default constructor.
        TrieNode(String str, boolean isKey) {
            left = right = null;
            this.str = str;
            this.isKey = isKey;
        }
    }

    private TrieNode root;
    
    /**
     * Simple constructor that will initialize the internals of {@code this}.
     */
    public BinaryPatriciaTrie() {
    	//ROOT NODE HAS TO BE EMPTY STRING SO WE CAN BRANCH OFF EITHER TO LEFT WITH 0 OR RIGHT WITH 1
    	//IS FALSE A KEY THOUGH? I SAY NO
    	//DOES ROOT BEING EMPTY COUNT AS 1 STRING??? NO
        root = new TrieNode("", false);
    }

    /**
     * Searches the trie for a given key.
     *
     * @param key The input {@link String} key.
     * @return {@code true} if and only if key is in the trie, {@code false} otherwise.
     */
    public boolean search(String key) {
        //SO, IDEA HERE IS THIS
    	//WE CONSUME CHARS AS WE GO ALONG, AND IF WE END UP AT A NODE
    	//WHERE THE ISKEY IS SET TO FALSE, THEN WE RETURN FALSE
    	//ALSO, IF WE END UP AT A NULL NODE AND HAVE REMAINING CHARS, THEN RETURN FALSE TOO
    	//branch off from here
    	String first = key.substring(0,1);
    	if (first.compareTo("0") == 0) {
    		return search_aux(key, root.left);
    	}
    	else {
    		return search_aux(key, root.right);
    	}
    }
    private boolean search_aux(String key, TrieNode x) {
    	//ok, so right now we SHOULD BE at a current node
    	//if at a current node that is null, then return false
    	if (x == null) {
    		return false;
    	}
    	//if we ARE AT SOME NODE, THEN CONSUME IN 3 CASES
    	else {
    		if (key.length() == x.str.length()) {
    			//if they are the same length, check to see if they are the same
    			if (key.compareTo(x.str) == 0) {
    				//return whatever the iskey value is tha twe landed at
    				return x.isKey;
    			}
    			//if they are the same length but not the exact same, just return false
    			else {
    				return false;
    			}    			
    		}
    		//if the key length is greater than the x.str length, go here
    		else if (key.length() > x.str.length()) {
				//get segment from the key
    			String segment = key.substring(0, x.str.length());
    			//compare segment to the str at node
    			if (segment.compareTo(x.str) == 0) {
    				//if they are the same, then consume and decide which way to go
    				String n_key = key.substring(x.str.length(), key.length());
    				//get first char to determine which way to go
    				String first = n_key.substring(0, 1);
    				if (first.compareTo("0") == 0) {
    					//go left
    					return search_aux(n_key, x.left);
    				}
    				else {
    					//go right
    					return search_aux(n_key, x.right);
    				}
    			}
    			else {
    				//if not the same, then return false
    				return false;
    			}
			}
			//else if our key length is less than the length of the x str, return false
			else {
				return false;
			}
    	}
    }
    /**
     * Inserts key into the trie.
     *
     * @param key The input {@link String}  key.
     * @return {@code true} if and only if the key was not already in the trie, {@code false} otherwise.
     */
    public boolean insert(String key) {
        //IDEA: WHEN WE INSERT STRING HERE, WE HAVE TO CONSUME KEY CHARS
    	//IF KEY IS NOT EMPTY STRING, THEN WE DO SOME PROCESSING
    	if (search(key)) {    		
    		return false;
    	}
    	else {
    		String first = key.substring(0,1);
    		if (first.compareTo("0") == 0) {
    			root.left = insert_aux2(key, root.left);
    		}
    		else {
    			root.right = insert_aux2(key, root.right);
    		}
    		//root = insert_aux2(key, root);
    		return true;
    	}    	
    }
    private TrieNode insert_aux2(String key, TrieNode x) {
    	//make base case be null node be reached
    	if (x == null) {
    		//if we went to null node, then just create a new node
    		TrieNode y = new TrieNode(key, true);
    		return y;
    	}
    	//WE ARE NOT AT NULL NODE, SO CONSUME CHARS AND DETERMINE WHICH WAY TO GO
    	else {
    		String first;
    		if (key.compareTo("") == 0) {
    			//if we got an empty key...then set whatever node's iskey to true
    			x.isKey = true;
    		}
    		//if not empty key, then we consume chars and see which way to go
    		else {
    			//check to see if both are the same length
    			if (key.length() == x.str.length()) {
    				//if they are the same length...then two cases to worry about here
    				//if they are the exact same, then just consume and set iskey to true
    				if (key.compareTo(x.str) == 0) {
    					x.isKey = true;
    				}
    				//IF NOT THE EXACT SAME, THEN NEED TO SPLIT
    				else {
    					//x.isKey = false;
    					//DO SPLITTING BELOW...SO MAKE NEW PARENT NODE
    					
    					int index = index(key, x.str);
    					String n_child = key.substring(index, key.length());
    					String x_node_str = x.str.substring(index, x.str.length());
    					TrieNode y = new TrieNode(x.str.substring(0, index), false);
    					x.isKey = true;
    					x.str = x_node_str;
    					String x_node_first = x_node_str.substring(0, 1);
    					
    					if (x_node_first.compareTo("0") == 0) {
    						//if first char of node is 0, then y's left child is gonna be x
    						y.left = x;
    						y.right = insert_aux2(n_child, y.right);
    					}
    					else {
    						//if first char of node is 1, then y's right child is gonna be x
    						y.left = insert_aux2(n_child, y.left);
    						y.right = x;
    					}
    					
    					return y;
    				}
    			}
    			//else if our key is longer than the node's string
    			else if (key.length() > x.str.length()) {
    				//get segment
    				String segment = key.substring(0, x.str.length());
    				if (segment.compareTo(x.str) == 0) {
    					//if they are the same, then consume normally and go further down
    					String n_key = key.substring(x.str.length(), key.length());
    					String n_first = n_key.substring(0, 1);
    					//decide which way to go
    					if (n_first.compareTo("0") == 0) {
    						//go left
    						x.left = insert_aux2(n_key, x.left);    						
    					} else {
    						//go right
    						x.right = insert_aux2(n_key, x.right);
    					}
    				}
    				else {
    					//if not the same...THEN DO A SPLIT
    					//OK SO WE DO UNEVEN SPLIT HERE...
    					//since our KEY is longer than the node's string...
    					//to get the index where they differ, we pass in the smaller string to index method 
    					int index = index(x.str, key);
    					//follow same idea as before, make new node and set it as parent
    					String x_node_str = x.str.substring(index, x.str.length());
    					String n_child = key.substring(index, key.length());
    					TrieNode y = new TrieNode(x.str.substring(0, index), false);
    					x.isKey = false;		//<------changed this one thing from true to false
    											//not sure if that's right thing to do though...
    					x.str = x_node_str;
    					String x_node_first = x_node_str.substring(0, 1);
    					
    					if (x_node_first.compareTo("0") == 0) {
    						//if first char of node is 0, then y's left child is gonna be x
    						y.left = x;
    						y.right = insert_aux2(n_child, y.right);
    					}
    					else {
    						//if first char of node is 1, then y's right child is gonna be x
    						y.left = insert_aux2(n_child, y.left);
    						y.right = x;
    					}
    					y.isKey = false;
    					return y;
    				}
    			}
    			//else if our key is smaller than our node's string, push down
    			else {
    				
    				//well, if we reach this case where our key is smaller than the 
    				//key at the node, we have to push the other node down <----i think this is the only place
    				//that we push down...creating a parent might be easier here
    				String n_key = x.str.substring(key.length(), x.str.length());
    				TrieNode y = new TrieNode(key, true);
    				//TrieNode y = new TrieNode(n_key, true);
    				//trim node's key
    				x.str = n_key;
    				String n_first = x.str.substring(0, 1);
    				//decide which way to go
    				if (n_first.compareTo("0") == 0) {
    					//go left
    					y.left = x;
    					return y;
    				} else {
    					//go right
    					y.right = x;
    					return y;
    				}
    			}
    		}
    	}
    	return x;
    }
    private int index(String key, String node) {
    	int i = 0;
    	int length = key.length();
    	char[] first = key.toCharArray();
    	char[] second = node.toCharArray();
    	while (i < length) {
    		if (first[i] != second[i]) {
    			break;
    		}
    		i ++;
    	}
    	return i;
    }
    /**
     * Deletes key from the trie.
     *
     * @param key The {@link String}  key to be deleted.
     * @return {@code true} if and only if key was contained by the trie before we attempted deletion, {@code false} otherwise.
     */
    public boolean delete(String key) {
        //IDEA FOR DELETE...
    	if (search(key)) {
    		//if it exists, delete and then return true
    		String first = key.substring(0, 1);
    		if (first.compareTo("0") == 0) {
    			//if first char is 0, go left
    			root.left = delete_aux(key, root.left);
    			//inefficient but gonna try to do a fix method where we see if a child node has to 
    			//merge with children
    			root.left = fix(root.left);
    		}
    		else {
    			//else first char must be 1, so go right
    			root.right = delete_aux(key, root.right);
    			root.right = fix(root.right);
    		}
    		return true;
    	}
    	//if value does not exist,...then just return false
    	else {
    		return false;
    	}
    }
    
    private TrieNode fix(TrieNode x) {
    	//fixing 3 cases here,
    	//if null, just return null
    	if (x == null) {
    		return null;
    	}
    	//if both are not null, then keep moving down
    	else if (x.left != null && x.right != null) {
    		x.left = fix(x.left);
    		x.right = fix(x.right);
    	}
    	//if one is null, then maybe have to merge
    	else {
    		//if our current node is not a key, but we have one child then merge
    		if (x.isKey == false) {
    			if (x.left == null) {
    				//if left is null, then merge with right child
    				String new_string = x.str.concat(x.right.str);
    				x.right.str = new_string;
    				return x.right;
    			}
    			else {
    				//if right is null, then merge with left child
    				String new_string = x.str.concat(x.left.str);
    				x.left.str = new_string;
    				return x.left;
    			}
    		}
    		else{
    			//determine which way to go
    			if (x.left == null) {
    				x.left = fix(x.left);
    			}
    			else {
    				x.right = fix(x.right);
    			}
    		}
    	}
    	return x;
    }
    
    private TrieNode delete_aux(String key, TrieNode x) {
    	//two cases to worry about here
    	//if leaf node, then just delete it
    	//if interior node, set isKey to false
    	if (x == null) {
    		return null;
    	}
    	else {
    		//IF WE ARE IN THIS BRANCH, THEN WE ARE AT SOME NODE
    		//REALLY WE JUST CONSUME CHARS HERE AT THIS POINT...AND IF WE DID SEARCH AND INSERT RIGHT
    		//ONLY HAVE TO WORRY ABOUT TWO CASES...
    		//CASE 1 WHERE BOTH STRINGS ARE EQUAL
    		//CASE 2 WHERE KEY IS LONGER THAN STR AT NODE
    		//KEY AND STR AT NODE ARE SAME
    		if (x.str.length() == key.length()){
    			//IF BOTH LEFT AND RIGHT ARE NULL, THEN WE ARE LEAF AND JUST RETURN NULL
    			if (x.left == null && x.right == null) {
    				return null;
    			}
    			//OTHERWISE, WE ARE AN INTERIOR NODE AND WE SHOULD JUST SET ISKEY TO FALSE
    			else {
    				x.isKey = false;
    			}
    		}
    		//KEY IS LONGER, SO CONSUME
    		else {
    			//N_KEY IS NEW STRING TO PASS AFTER CONSUMING CHARS
    			String n_key = key.substring(x.str.length(), key.length());
    			//GET FIRST CHAR TO DETERMINE WHICH WAY TO GO
    			String first_n = n_key.substring(0, 1);
    			//if first char is equal to 0
    			if (first_n.compareTo("0") == 0) {
    				//go left
    				x.left = delete_aux(n_key, x.left);
    			}
    			//if we didn't enter previous branch, then it must be 1
    			else {
    				//go right
    				x.right = delete_aux(n_key, x.right);
    			}
    		}
    	}
    	return x;
    }

    /**
     * Queries the trie for emptiness.
     *
     * @return {@code true} if and only if {@link #getSize()} == 0, {@code false} otherwise.
     */
    public boolean isEmpty() {    	
        if (getSize() == 0){
        	return true;
        }
        else {
        	return false;
        }
    }

    /**
     * Returns the number of keys in the tree.
     * KEY IS SYNONYM WITH WHOLE STRING
     * @return The number of keys in the tree.
     */
    public int getSize() {
        return aux_size(root.left) + aux_size(root.right);
    }
    private int aux_size(TrieNode x) {
    	if (x == null) {
    		return 0;
    	}
    	else {
    		if (x.isKey == true) {
    			return 1 + aux_size(x.left) + aux_size(x.right);
    		}
    		else {
    			return 0 + aux_size(x.left) + aux_size(x.right);
    		}
    	}
    }

    /**
     * <p>Performs an <i>inorder (symmetric) traversal</i> of the Binary Patricia Trie. Remember from lecture that inorder
     * traversal in tries is NOT sorted traversal, unless all the stored keys have the same length. This
     * is of course not required by your implementation, so you should make sure that in your tests you
     * are not expecting this method to return keys in lexicographic order. We put this method in the
     * interface because it helps us test your submission thoroughly and it helps you debug your code! </p>
     *
     * <p>We <b>neither require nor test </b> whether the {@link Iterator} returned by this method is fail-safe or fail-fast.
     * This means that you  do <b>not</b> need to test for thrown {@link java.util.ConcurrentModificationException}s and we do
     * <b>not</b> test your code for the possible occurrence of concurrent modifications.</p>
     *
     * <p>We also assume that the {@link Iterator} is <em>immutable</em>, i,e we do <b>not</b> test for the behavior
     * of {@link Iterator#remove()}. You can handle it any way you want for your own application, yet <b>we</b> will
     * <b>not</b> test for it.</p>
     *
     * @return An {@link Iterator} over the {@link String} keys stored in the trie, exposing the elements in <i>symmetric
     * order</i>.
     */
    //iterates over the string keys stored...we do iterate over nodes but should only 
    //return iterator of all the STRINGS STORED IN BPT...
    public Iterator<String> inorderTraversal() {
        return new NodeIterator<String>();
    }
    
    private class NodeIterator<String> implements Iterator<String>{
    	ArrayList items = new ArrayList<String>();
    	int index = -1;
    	public NodeIterator() {
    		init_aux(root, root.str);
    	}
    	private void init_aux(TrieNode x, java.lang.String str) {
    		if (x != null) {
    			if (x.isKey == true) {
    				init_aux(x.left, str.concat(x.str));
    				items.add(str.concat(x.str));
    				init_aux(x.right, str.concat(x.str));
    			}
    			else {
    				init_aux(x.left, str.concat(x.str));
    				init_aux(x.right, str.concat(x.str));
    			}
    		}
    	}
    	
    	@Override
		public boolean hasNext() {
    		if (index < items.size() - 1) {
    			return true;
    		}
    		else {
    			return false;
    		}
    	}
    	@Override 
    	public String next() {
    		index = index + 1;
    		return (String) items.get(index).toString();
    	}
    }

    /**
     * Finds the longest {@link String} stored in the Binary Patricia Trie.
     * @return <p>The longest {@link String} stored in this. If the trie is empty, the empty string &quot;&quot; should be
     * returned. Careful: the empty string &quot;&quot;is <b>not</b> the same string as &quot; &quot;; the latter is a string
     * consisting of a single <b>space character</b>! It is also <b>not the same as the</b> null <b>reference</b>!</p>
     *
     * <p>Ties should be broken in terms of <b>value</b> of the bit string. For example, if our trie contained
     * only the binary strings 01 and 11, <b>11</b> would be the longest string. If our trie contained
     * only 001 and 010, <b>010</b> would be the longest string.</p>
     */
    //so, not sure if bug is related to this or how we insert
    //but we do break ties by choosing 1 (going to the right)
    //expected 11011 but got 10111
    //maybe we get error in the way we insert?
    public String getLongest() {
    	return getLongest_aux(root);
    }
    
    private String getLongest_aux(TrieNode x) {
    	//do not want null
    	if (x != null) {
    		//if left tree has more chars, then choose longest string it builds
	    	if (getChars(x.left) > getChars(x.right)) {
	    		return x.str + getLongest_aux(x.left);
	    	}
	    	//otherwise, right has more chars or is equal, so we go right
	    	else {
	    		return x.str + getLongest_aux(x.right);
	    	}
    	}
    	else {
    		return "";
    	}
    }

    private int getChars(TrieNode x) {
    	if (x == null) {
    		return 0;
    	}
    	else {
    		//get string length
    		return x.str.length() + max(getChars(x.left), getChars(x.right));
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
     * Makes sure that your trie doesn't have splitter nodes with a single child. In a Patricia trie, those nodes should
     * be pruned.
     * @return {@code true} iff all nodes in the trie either denote stored strings or split into two subtrees, {@code false} otherwise.
     */
    public boolean isJunkFree(){
        return isEmpty() || (isJunkFree(root.left) && isJunkFree(root.right));
    }

    private boolean isJunkFree(TrieNode n){
        if(n == null){   // Null subtrees trivially junk-free
            return true;
        }
        if(!n.isKey){   // Non-key nodes need to be strict splitter nodes
            return ( (n.left != null) && (n.right != null) && isJunkFree(n.left) && isJunkFree(n.right) );
        } else {
            return ( isJunkFree(n.left) && isJunkFree(n.right) ); // But key-containing nodes need not.
        }
    }
}

package phonebook.hashes;
import java.util.ArrayList;

import phonebook.exceptions.UnimplementedMethodException;
import phonebook.utils.KVPair;
import phonebook.utils.PrimeGenerator;

/**
 * <p>{@link QuadraticProbingHashTable} is an Openly Addressed {@link HashTable} which uses <b>Quadratic
 * Probing</b> as its collision resolution strategy. Quadratic Probing differs from <b>Linear</b> Probing
 * in that collisions are resolved by taking &quot; jumps &quot; on the hash table, the length of which
 * determined by an increasing polynomial factor. For example, during a key insertion which generates
 * several collisions, the first collision will be resolved by moving 1^2 + 1 = 2 positions over from
 * the originally hashed address (like Linear Probing), the second one will be resolved by moving
 * 2^2 + 2= 6 positions over from our hashed address, the third one by moving 3^2 + 3 = 12 positions over, etc.
 * </p>
 *
 * <p>By using this collision resolution technique, {@link QuadraticProbingHashTable} aims to get rid of the
 * &quot;key clustering &quot; problem that {@link LinearProbingHashTable} suffers from. Leaving more
 * space in between memory probes allows other keys to be inserted without many collisions. The tradeoff
 * is that, in doing so, {@link QuadraticProbingHashTable} sacrifices <em>cache locality</em>.</p>
 *
 * @author YOUR NAME HERE!
 *
 * @see HashTable
 * @see SeparateChainingHashTable
 * @see OrderedLinearProbingHashTable
 * @see LinearProbingHashTable
 * @see CollisionResolver
 */
public class QuadraticProbingHashTable extends OpenAddressingHashTable {

    /* ********************************************************************/
    /* ** INSERT ANY PRIVATE METHODS OR FIELDS YOU WANT TO USE HERE: ******/
    /* ********************************************************************/
	Boolean resize = false;
	int stored_elements = 0;
    /* ******************************************/
    /*  IMPLEMENT THE FOLLOWING PUBLIC METHODS: */
    /* **************************************** */
    /**
     * Constructor with soft deletion option. Initializes the internal storage with a size equal to the starting value of  {@link PrimeGenerator}.
     * @param soft A boolean indicator of whether we want to use soft deletion or not. {@code true} if and only if
     *               we want soft deletion, {@code false} otherwise.
     */
    public QuadraticProbingHashTable(boolean soft) {
    	softFlag = soft;
        primeGenerator = new PrimeGenerator();
        table = new KVPair[primeGenerator.getCurrPrime()];
        count = 0;
    }
    @Override
    public String put(String key, String value) {
    	//do illegal args exception handling here
    	if (key == null || value == null) {
    		throw new IllegalArgumentException("given null key or value");
    	}	
    	//check to see if we land on a tombstone spot
    	if (table[hash(key)] != null && table[hash(key)].getKey().compareTo("") == 0) {
    		//if we land on a spot that is a tombstone, don't resize and just insert here
    		table[hash(key)] = new KVPair(key, value);
    		stored_elements ++;
    		return value;
    	}
    	else {
	    	//if resize flag is set to true, then resize first and then insert val
	    	if (resize == true) {
	    		//resize
	    		//build new arraylist first to hold all elemenets
	    		ArrayList<KVPair> xn = new ArrayList<KVPair>();
	    		for (int i = 0; i < table.length ; i++) {
	    			if (table[i] != null) {
	    				if (table[i].getKey().compareTo("") != 0) {
	    					xn.add(table[i]);
	    				}
	    			}
	    		}
	    		//now we have all our stuff in our arraylist, so let's say table is equal to new bigger table
	    		table = new KVPair[primeGenerator.getNextPrime()];
	    		//System.out.println("new table size is + " +  primeGenerator.getCurrPrime());
	    		//reset count to 0 
	    		count = 0;
	    		stored_elements = 0;
	    		//reset flag to false
	    		resize = false;
	    		//now let's copy over our stuff by reinserting using our put
	    		for (KVPair item : xn) {
	    			//System.out.println("adding");
	    			put(item.getKey(),item.getValue());
	    		}	
	    		put(key, value);
	    		return value;
	    	}
	    	//else, insert normally, QUADRATIC PROBING WORK DONE HERE
	    	//idea: if we do not reach collision (not null or not tombstone), then insert
	    	//otherwise, calculate a "step", and then increase our index by that step
	    	//if we hit collision, increase step again and repeat
	    	else {
	    		int index = (hash(key));
	    		//if index is null or tombstone, just insert and we're good
	    		if (table[index] == null || table[index].getKey().compareTo("") == 0) {
	    			table[index] = new KVPair(key, value);
	    			count ++;
	    			stored_elements ++;
	    			if (count >= (capacity() / 2) + 1) {
	    				resize = true;
	    			}
	    		}
	    		//if it is not null, we have a collision and have to find the next avail spot AND "sort"
	    		else {
	    			int i = 1;
	    			int new_index = index;
	    			while (table[new_index] != null) {
	    				//if it's a tombstone, then just get this index and use it to insert
	    				if (table[new_index].getKey().compareTo("") == 0) {
	    					break;
	    				}
	    				//do work here
	    				new_index = index + (i * i) + i;
	    				//reset to wrap around
	    				if (new_index >= table.length ) {    			
	    					new_index = new_index - (table.length );
	    					i++;
	    					//System.out.println("new index: " + new_index);
	    					continue;
	    				}
	    				i++;
	    			}
	    			table[new_index] = new KVPair(key, value);
	    			count ++;
	    			stored_elements ++;
	    			if (count >= (capacity() / 2) + 1) {
	    				resize = true;
	    			}
	    		}
	    		return value;
	    	}
    	}
    }

    @Override
    public String get(String key) {
    	//UPDATE SEARCH HERE TO FAIL FASTER TOO <-----------------------
    	//when we get a value, we hash the key to get an index
    	int index = (hash(key));
    	//if the key hashes to an index that is null in the table, then just return null
    	if (table[index] == null) {
    		return null;
    	}
    	else {
    	//if the key hashes to an index that is NOT Null in the table, then branch here
    		
	    	//if the key hashes to a spot where the key is the same, then we just return its value   
	    	if (table[index].getKey().compareTo(key) == 0) {
	    		return table[index].getValue();
	    	}
	    	//if key is not the same, then increase index until we get the matching key
	    	//<----------------POSSIBLE BUG
	    	else {
	    		int i = 1;
	    		int new_index = index;
    			while (table[new_index] != null) {
    				//if it's a tombstone, then just get this index and use it to insert
    				if (table[new_index].getKey().compareTo("") == 0) {
    					return null;
    				}
    				if (table[new_index].getKey().compareTo(key) == 0) {
    					return table[new_index].getValue();
    				}
    				//do work here
    				new_index = index + (i * i) + i;
    				//reset to wrap around
    				if (new_index >= table.length ) {    			
    					new_index = new_index - (table.length );
    					i++;
    					
    					continue;
    				}
    				i++;
    			}
	    		return null;
	    	}
    	}
    }

    @Override
    public String remove(String key) {
    	//does remove have to be modified??? i remember in the vids marsh said if we delete in ordered linear probe,
    	//we have to delete everything and reinsert
    	//if key is null, return null
    	if (key == null) {
        	return null;
        }
    	//get index
    	int index = hash(key);
    	//if softFlag is true, then we DO SOFT DELETION HERE <-----------------------------------
    	if (softFlag == true) {
    		if (table[index] == null) {
        		//if key hashes to an index that is null, just return null
        		return null;
        	}
        	else {
        		int i = 1;
        		int new_index = index;
        		//we are not null at index
        		while (table[new_index] != null) {
        			if (table[new_index].getKey().compareTo(key) == 0) {
        				String x = table[new_index].getValue();
        				//set to tombstone for soft deletion
        				table[new_index] = TOMBSTONE;
        				stored_elements --;
        				return x;
        				//correct here <----------
        			}
        			//if we did not enter previous branch, then possible collision 
        			//occurred and we have to find next spot
        			new_index = index + (i * i) + i;
        			if (new_index >= table.length) {
        				new_index = new_index - (table.length);
        				i++;
        				continue;
        			}
        			i++;
        		}
        		return null;
        	}
    	}
    	//SOFT DELETE IS OKAY HERE
    	//otherwise, we do HARD DELETION HERE <--------------------------------------------------
    	else {
    		if (table[index] == null) {
        		//if key hashes to an index that is null, just return null
        		return null;
        	}
        	else {
        		String x = null;
        		int i = 1;
        		int new_index = index;
        		//ok, so idea here will be this
        		//we will check where we hash to since it's not null
        		//if it's equal to value we want to remove, just remove it
        		//otherwise, we have to look further into the collision cluster to find it and remove it
        		//we are not null at index
        		while (table[new_index] != null) {        		
        			if (table[new_index].getKey().compareTo(key) == 0) {
        				x = table[new_index].getValue();
        				table[new_index] = null;
        				count --;        
        				stored_elements --;
        				break;
        			}
        			//if we did not enter previous branch, then possible collision 
        			//occurred and we have to find next spot
        			new_index = index + (i * i) + i;
        			if (new_index >= table.length) {
        				new_index = new_index - (table.length);
        				i++;
        				continue;
        			}
        			i++;
        		}
        		//now we have exited loop because we found our spot
        		//but we still have to increase our new_index to find next spot and check if it's null for cluster
        		new_index = index + (i * i) + i;
        		
        		if (new_index >= table.length ) {
        			new_index = new_index - table.length;
        		}
        		i++;
        		//now, we have to check to see if anything after is null, if not null add to new list
        		//new list to hold elements
        		ArrayList<KVPair> cluster = new ArrayList<KVPair>();
        		//adding clustered items to our list
        		while (table[new_index] != null) {
        			cluster.add(table[new_index]);
        			table[new_index] = null;
        			count --;
        			stored_elements --;
        			new_index = index + (i * i) + i;
        			if (new_index >= table.length) {
        				new_index = new_index - table.length;
        				continue;
        			}
        			i++;
        		}
        		//check to see if we have to remove the resize flag
        		if (count >= (capacity() / 2) + 1) {
        			resize = true;
        		}
        		else {
        			resize = false;
        		}
        		//now put the cluster back in        		
        		for (KVPair item : cluster) {
            		put(item.getKey(), item.getValue());
            	}	
        		return x;
        	}
    	}
    }


    @Override
    public boolean containsKey(String key) {
    	if (key == null) {
        	return false;
        }
        else {
        	//search for key
        	int index = hash(key);
        	while (table[index] != null) {
        		if (table[index].getKey().compareTo(key) == 0) {
        			return true;
        		}
        		index ++;
        	}
        	return false;
        }
    }

    @Override
    public boolean containsValue(String value) {
    	if (value == null) {
        	return false;
        }
        int i = 0;
        int length = table.length - 1;
        while (i < length ) {
        	if (table[i] != null) {
        		if (table[i].getValue().compareTo(value) == 0) {
        			return true;
        		}
        	}
        	i ++;
        }
        return false;
    }
    @Override
    public int size(){
        return stored_elements;
    }

    @Override
    public int capacity() {
        return table.length;
    }
}
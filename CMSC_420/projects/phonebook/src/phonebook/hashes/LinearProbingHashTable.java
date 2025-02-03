package phonebook.hashes;

import java.util.ArrayList;

import phonebook.exceptions.UnimplementedMethodException;
import phonebook.utils.KVPair;
import phonebook.utils.PrimeGenerator;


/**
 * <p>{@link LinearProbingHashTable} is an Openly Addressed {@link HashTable} implemented with <b>Linear Probing</b> as its
 * collision resolution strategy: every key collision is resolved by moving one address over. It is
 * the most famous collision resolution strategy, praised for its simplicity, theoretical properties
 * and cache locality. It <b>does</b>, however, suffer from the &quot; clustering &quot; problem:
 * collision resolutions tend to cluster collision chains locally, making it hard for new keys to be
 * inserted without collisions. {@link QuadraticProbingHashTable} is a {@link HashTable} that
 * tries to avoid this problem, albeit sacrificing cache locality.</p>
 *
 * @author Rudy
 *
 * @see HashTable
 * @see SeparateChainingHashTable
 * @see OrderedLinearProbingHashTable
 * @see QuadraticProbingHashTable
 * @see CollisionResolver
 */
public class LinearProbingHashTable extends OpenAddressingHashTable {

    /* ********************************************************************/
    /* ** INSERT ANY PRIVATE METHODS OR FIELDS YOU WANT TO USE HERE: ******/
    /* ********************************************************************/
	Boolean resize = false;
	int stored_elements = 0;
    /* ******************************************/
    /*  IMPLEMENT THE FOLLOWING PUBLIC METHODS: */
    /* **************************************** */
	// We mask the top bit of the default hashCode() to filter away negative values.
    // Have to copy over the implementation from OpenAddressingHashTable; no biggie.
    
    /**
     * Constructor with soft deletion option. Initializes the internal storage with a size equal to the starting value of  {@link PrimeGenerator}.
     *
     * @param soft A boolean indicator of whether we want to use soft deletion or not. {@code true} if and only if
     *             we want soft deletion, {@code false} otherwise.
     */
    public LinearProbingHashTable(boolean soft) {
        softFlag = soft;
        primeGenerator = new PrimeGenerator();
        table = new KVPair[primeGenerator.getCurrPrime()];
        count = 0;
    }

    /**
     * Inserts the pair &lt;key, value&gt; into this. The container should <b>not</b> allow for {@code null}
     * keys and values, and we <b>will</b> test if you are throwing a {@link IllegalArgumentException} from your code
     * if this method is given {@code null} arguments! It is important that we establish that no {@code null} entries
     * can exist in our database because the semantics of {@link #get(String)} and {@link #remove(String)} are that they
     * return {@code null} if, and only if, their key parameter is {@code null}. This method is expected to run in <em>amortized
     * constant time</em>.
     * <p>
     * Instances of {@link LinearProbingHashTable} will follow the writeup's guidelines about how to internally resize
     * the hash table when the capacity exceeds 50&#37;
     *
     * @param key   The record's key.
     * @param value The record's value.
     * @return The value added.
     * @throws IllegalArgumentException if either argument is {@code null}.
     */
    //so we put key into our table
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
	    	//else, insert normally
	    	else {
	    		int index = (hash(key));
	    		//if index is null, just insert
	    		if (table[index] == null || table[index].getKey().compareTo("") == 0) {
	    			table[index] = new KVPair(key, value);
	    			count ++;
	    			stored_elements ++;
	    			if (count >= (capacity() / 2) + 1) {
	    				resize = true;
	    			}
	    		}
	    		//if it is not null, we have a collision and have to find the next avail spot
	    		else {
	    			while (table[index] != null) {
	    				if (table[index].getKey().compareTo("") == 0) {
	    					break;
	    				}
	    				if (index >= table.length - 1) {
	    					index = 0;
	    					continue;
	    				}
	    				index ++;
	    			}
	    			//now we have index for null space in table, so insert
	    			table[index] = new KVPair(key, value);
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
	    		while (table[index] != null) {
	    			if (index >= table.length - 1) {
	    				index = 0;
	    				continue;
	    			}
	    			if (table[index].getKey().compareTo(key) == 0) {
	    				return table[index].getValue();
	    			}
	    			
	    			index ++;
	    		}
	    		return null;
	    	}
    	}
    	
    }


    /**
     * <b>Return</b> the value associated with key in the {@link HashTable}, and <b>remove</b> the {@link phonebook.utils.KVPair} from the table.
     * If key does not exist in the database
     * or if key = {@code null}, this method returns {@code null}. This method is expected to run in <em>amortized constant time</em>.
     *
     * @param key The key to search for.
     * @return The associated value. If the key is {@code null}, return {@code null};
     * if the key doesn't exist in the database, return {@code null}.
     */
    @Override
    public String remove(String key) {
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
        		//we are not null at index
        		while (table[index] != null) {
        			if (table[index].getKey().compareTo(key) == 0) {
        				String x = table[index].getValue();
        				//set to tombstone for soft deletion
        				table[index] = TOMBSTONE;
        				stored_elements --;
        				return x;
        			}
        			if (index >= table.length - 1) {
        				index = 0;
        				continue;
        			}
        			index ++;
        		}
        		return null;
        		
        	}
    	}
    	//SOFT DELETE IS OK
    	//otherwise, we do HARD DELETION HERE <--------------------------------------------------
    	else {
    		if (table[index] == null) {
        		//if key hashes to an index that is null, just return null
        		return null;
        	}
        	else {
        		String x = null;
        		//we are not null at index
        		
        		while (table[index] != null) {
        			if (table[index].getKey().compareTo(key) == 0) {
        				x = table[index].getValue();
        				table[index] = null;
        				count --;        	
        				stored_elements --;
        				break;
        			}
        			if (index >= table.length  - 1) {
        				index = 0;
        				continue;
        			}
        			index ++;
        		}
        		index ++;
        		if (index >= table.length - 1) {
        			index = 0;
        		}
        		//now, we have to check to see if anything after is null, if not null add to new list
        		//new list to hold elements
        		ArrayList<KVPair> cluster = new ArrayList<KVPair>();
        		//adding clustered items to our list
        		while (table[index] != null) {
        			cluster.add(table[index]);
        			table[index] = null;
        			count --;
        			stored_elements --;
        			if (index >= table.length - 1) {
        				index = 0;
        				continue;
        			}
        			index ++;
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
        		if (index >= table.length - 1) {
        			index = 0;
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
    public int size() {
        return stored_elements;
    }

    @Override
    public int capacity() {
        return table.length;
    }
}
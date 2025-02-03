package phonebook.hashes;

import java.util.ArrayList;
import java.util.Iterator;

import phonebook.exceptions.UnimplementedMethodException;
import phonebook.utils.KVPair;
import phonebook.utils.KVPairList;
import phonebook.utils.PrimeGenerator;

/**<p>{@link SeparateChainingHashTable} is a {@link HashTable} that implements <b>Separate Chaining</b>
 * as its collision resolution strategy, i.e the collision chains are implemented as actual
 * Linked Lists. These Linked Lists are <b>not assumed ordered</b>. It is the easiest and most &quot; natural &quot; way to
 * implement a hash table and is useful for estimating hash function quality. In practice, it would
 * <b>not</b> be the best way to implement a hash table, because of the wasted space for the heads of the lists.
 * Open Addressing methods, like those implemented in {@link LinearProbingHashTable} and {@link QuadraticProbingHashTable}
 * are more desirable in practice, since they use the original space of the table for the collision chains themselves.</p>
 *
 * @author Rudy F
 * @see HashTable
 * @see SeparateChainingHashTable
 * @see LinearProbingHashTable
 * @see OrderedLinearProbingHashTable
 * @see CollisionResolver
 */
public class SeparateChainingHashTable implements HashTable{

    /* ****************************************************************** */
    /* ***** PRIVATE FIELDS / METHODS PROVIDED TO YOU: DO NOT EDIT! ***** */
    /* ****************************************************************** */
	private boolean resize = false;
    private KVPairList[] table;
    private int table_indices;
    private PrimeGenerator primeGenerator;

    // We mask the top bit of the default hashCode() to filter away negative values.
    // Have to copy over the implementation from OpenAddressingHashTable; no biggie.
    public int hash(String key){
        return (key.hashCode() & 0x7fffffff) % table.length;
    }
    
    /* **************************************** */
    /*  IMPLEMENT THE FOLLOWING PUBLIC METHODS:  */
    /* **************************************** */
    /**
     *  Default constructor. Initializes the internal storage with a size equal to the default of {@link PrimeGenerator}.
     */
    public SeparateChainingHashTable(){
    	primeGenerator = new PrimeGenerator();
        table_indices = 0;
        table = new KVPairList[(primeGenerator.getCurrPrime())];
    }

    @Override
    public String put(String key, String value) {
    	int index = hash(key);
    	//hash the key to get a table index
    	//if space in table is empty (i.e. null), then insert a new kvpairlist val into table index
    	if (table[index] == null) {    		
    		table[hash(key)] = new KVPairList(key, value);
    		table_indices ++;
    		
    	}
    	//if key is already in, then just use the addBack method to append the key to 
    	//the back of the list
    	else {
    		table[index].addBack(key, value);
    		
    		
    		/*don't worry about handling duplicate values yet
    		//but we have to check if the key already exists, if it does exist then
    		//we should update the value for the key
    		if (table[(hash(key))].containsKey(key) ) {
    			table[(hash(key))].updateValue(key, value);
    		}
    		else {
    		//otherwise, we just add at the end
    		table[(hash(key))].addBack(key, value);
    		}
    		*/
    	}
    	
        return key;
    }

    @Override
    public String get(String key) {
        //if we hash the key, and then see that it is null in the table
    	//it does not exist so there is nothing to get
    	if (table[(hash(key))] == null) {
    		return null;
    	}
    	//if the key hashes to a table index that is not null
    	//then we will try to use the getValue(key) to return the value of the key
    	else {
    		return table[(hash(key))].getValue(key).getValue();
    	}
    }

    @Override
    public String remove(String key) {
        //if we hash the key and then see it is null in the table
    	//it does not exist, so there is nothing to get
    	if (table[(hash(key))] == null) {
    		return null;
    	} 
    	else {
    	//if we hash the key and it IS NOT NULL in the table
    	//then we will use the remove method of the table index
    		String s = table[hash(key)].getValue(key).getValue();
    		table[hash(key)].remove(key, s);
    		return s;
    	}
    }

    @Override
    public boolean containsKey(String key) {
        if (table[(hash(key))] == null) {
        	return false;
        }
        else {
        	//it is not null, so look for key
        	if (table[(hash(key))].containsKey(key)) {
        		return true;
        	}
        	else {
        		return false;
        	}
        }
    }

    @Override
    public boolean containsValue(String value) {
        //have to loop through whole list
    	int index = 0;
    	while (table[index] != null) {
    		if (table[index].containsValue(value)) {
    			return true;
    		}
    		index ++;
    	}
    	return false;
    }

    @Override
    public int size() {
    	int counter = 0;
    	for (int i = 0; i < table.length ; i++) {
    		if (table[i] != null) {
    			counter += table[i].size();
    		}
    	}
        return counter;
    }

    @Override
    public int capacity() {
        return table.length; // Or the value of the current prime.
    }

    public KVPairList get(int idx) throws IndexOutOfBoundsException {
    	return table[idx];
    }

    /**
     * Enlarges this hash table. At the very minimum, this method should increase the <b>capacity</b> of the hash table and ensure
     * that the new size is prime. The class {@link PrimeGenerator} implements the enlargement heuristic that
     * we have talked about in class and can be used as a black box if you wish.
     * @see PrimeGenerator#getNextPrime()
     */
    public void enlarge() {
    	//copy all elements first
    	ArrayList<KVPairList> x = new ArrayList<KVPairList>();
    	for (int i = 0; i < table.length; i++) {
    		if (table[i] != null) {
    			x.add(table[i]);
    		}
    	}
    	//now we have all of our elements copied, make table equal to a bigger table
    	table = new KVPairList[primeGenerator.getNextPrime()];
    	//now reinsert the lists
    	for (KVPairList item : x){
    		//each item is a list, so we need an iterator for it to go through 
    		//all the keys
    		Iterator<KVPair> y = item.iterator();
    		while (y.hasNext()) {
    			KVPair thing_to_add = y.next();
    			//add each thing again
    			put(thing_to_add.getKey(), thing_to_add.getValue());
    		}
    	}
    }

    /**
     * Shrinks this hash table. At the very minimum, this method should decrease the size of the hash table and ensure
     * that the new size is prime. The class {@link PrimeGenerator} implements the shrinking heuristic that
     * we have talked about in class and can be used as a black box if you wish.
     *
     * @see PrimeGenerator#getPreviousPrime()
     */
    public void shrink(){
        //copy all elements from bigger array first
    	ArrayList<KVPairList> x = new ArrayList<KVPairList>();
    	for (int i = 0; i < table.length; i ++) {
    		if (table[i] != null) {
    			x.add(table[i]);
    		}
    	}
    	//now we have all of our elements copied, make table equal to a bigger table
    	table = new KVPairList[primeGenerator.getPreviousPrime()];
    	for (KVPairList item : x) {
    		//each item is a list, so we need an iterator for it to go through
    		//all the keys
    		Iterator<KVPair> y = item.iterator();
    		while (y.hasNext()) {
    			KVPair thing_to_add = y.next();
    			//add each thing again
    			put (thing_to_add.getKey(), thing_to_add.getValue());
    		}
    	}
    }
    public void print() {
    	for (int i = 0; i < table.length; i++){
    		if (table[i] != null) {
    			System.out.print("INDEX: "+ i + " " + table[i].toString());
    		}
    		else {
    			System.out.println("INDEX: " + i + " NULL");
    		}
    	}
    }
}

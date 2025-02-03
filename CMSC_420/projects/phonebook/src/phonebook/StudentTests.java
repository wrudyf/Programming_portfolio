package phonebook;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import phonebook.hashes.*;
import phonebook.utils.KVPair;
import phonebook.utils.NoMorePrimesException;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import static org.junit.Assert.*;
import static phonebook.hashes.CollisionResolver.*;

//import sun.plugin.perf.PluginRollup;

/**
 * <p> {@link StudentTests} is a place for you to write your tests for {@link Phonebook} and all the various
 * {@link HashTable} instances.</p>
 *
 * @author YOUR NAME HERE!
 * @see Phonebook
 * @see HashTable
 * @see SeparateChainingHashTable
 * @see LinearProbingHashTable
 * @see QuadraticProbingHashTable
 */
public class StudentTests {

    private Phonebook pb;
    private CollisionResolver[] resolvers = {SEPARATE_CHAINING, LINEAR_PROBING, ORDERED_LINEAR_PROBING, QUADRATIC_PROBING};
    private HashMap<String, String> testingPhoneBook;
    private static final long SEED = 47;
    private static final Random RNG = new Random(SEED);
    private static final int NUMS = 1000;
    private static final int UPPER_BOUND = 100;

    private String format(String error, CollisionResolver namesToPhones, CollisionResolver phonesToNames) {
        return error + "Collision resolvers:" + namesToPhones + ", " + phonesToNames + ".";
    }


    private String errorData(Throwable t) {
        return "Received a " + t.getClass().getSimpleName() + " with message: " + t.getMessage() + ".";
    }

    @Before
    public void setUp() {
        testingPhoneBook = new HashMap<>();
        testingPhoneBook.put("Arnold", "894-59-0011");
        testingPhoneBook.put("Tiffany", "894-59-0011");
        testingPhoneBook.put("Jessie", "705-12-7500");
        testingPhoneBook.put("Mary", "888-1212-3340");
    }

    @After
    public void tearDown() {
        testingPhoneBook.clear();
    }


    // Make sure that all possible phonebooks we can create will report empty when beginning.
    @Test
    public void testBehaviorWhenEmpty() {
        for (CollisionResolver namesToPhones : resolvers) {
            for (CollisionResolver phonesToNames : resolvers) {
                pb = new Phonebook(namesToPhones, phonesToNames);
                assertTrue(format("Phonebook should be empty", namesToPhones, phonesToNames), pb.isEmpty());
            }
        }
    }

    // See if all of our hash tables cover the simple example from the writeup.
    @Test
    public void testOpenAddressingResizeWhenInsert() {
        SeparateChainingHashTable sc = new SeparateChainingHashTable();
        LinearProbingHashTable lp = new LinearProbingHashTable(false);
        QuadraticProbingHashTable qp = new QuadraticProbingHashTable(false);
        assertEquals("Separate Chaining hash should have a capacity of 7 at startup.", 7, sc.capacity());
        assertEquals("Linear Probing hash should have a capacity of 7 at startup.", 7, lp.capacity());
        assertEquals("Quadratic Probing hash should have a capacity of 7 at startup.", 7, qp.capacity());
        for (Map.Entry<String, String> entry : testingPhoneBook.entrySet()) { // https://docs.oracle.com/javase/10/docs/api/java/util/Map.Entry.html
            sc.put(entry.getKey(), entry.getValue());
            lp.put(entry.getKey(), entry.getValue());
            qp.put(entry.getKey(), entry.getValue());
        }
        assertEquals("Separate Chaining hash should have a capacity of 7 after inserting 4 elements.", 7, sc.capacity());
        assertEquals("Linear Probing hash should have a capacity of 7 after inserting 4 elements.", 7, lp.capacity());
        assertEquals("Quadratic Probing hash should have a capacity of 7 after inserting 4 elements.", 7, qp.capacity());

        sc.put("DeAndre", "888-1212-3340");
        assertEquals("Separate Chaining hash should still have a capacity of 7 after inserting 5 elements.", 7, sc.capacity());
        sc.enlarge();
        assertEquals("Separate Chaining hash should have a capacity of 13 after first call to enlarge().", 13, sc.capacity());
        sc.enlarge();
        assertEquals("Separate Chaining hash should have a capacity of 23 after second call to enlarge().", 23, sc.capacity());
        sc.shrink();
        assertEquals("Separate Chaining hash should have a capacity of 13 after two calls to enlarge() and one to shrink().",
                13, sc.capacity());
        sc.shrink();
        assertEquals("Separate Chaining hash should have a capacity of 7 after two calls to enlarge() and two to shrink().",
                7, sc.capacity());
        lp.put("DeAndre","888-1212-3340" );
        assertEquals("Linear Probing hash should have a capacity of 13 after inserting 5 elements.",
                13, lp.capacity());
        qp.put("DeAndre","888-1212-3340" );
        assertEquals("Quadratic Probing hash should have a capacity of 13 after inserting 5 elements.",
                13, qp.capacity());

        // The following two deletions should both fail and thus not affect capacity.

        lp.remove("Thomas");
        assertEquals("Linear Probing hash with starting capacity of 7 should have a capacity of 13 after " +
                "five insertions and a failed deletion.", 13, lp.capacity());
        qp.remove("Thomas" );
        assertEquals("Quadratic Probing hash with starting capacity of 7 should have a capacity of 13 after " +
                "five insertions and a failed deletion.", 13, qp.capacity());
    }

    // An example of a stress test to catch any insertion errors that you might get.
    @Test
    public void insertionStressTest() {
        HashTable sc = new SeparateChainingHashTable();
        HashTable lp = new LinearProbingHashTable(false);
        HashTable qp = new QuadraticProbingHashTable(false);
        for (int i = 0; i < NUMS; i++) {
            String randomNumber = Integer.toString(RNG.nextInt(UPPER_BOUND));
            String randomNumber2 = Integer.toString(RNG.nextInt(UPPER_BOUND));
            try {
                sc.put(randomNumber, randomNumber2);
            } catch (NoMorePrimesException ignored) {
                // To have this exception thrown is not a problem; we have a finite #primes to generate resizings for.
            } catch (Throwable t) {
                fail("Separate Chaining hash failed insertion #" + i + ". Error message: " + errorData(t));
            }

            try {
                lp.put(randomNumber, randomNumber2);
            } catch (NoMorePrimesException ignored) {
                // To have this exception thrown is not a problem; we have a finite #primes to generate resizings for.
            } catch (Throwable t) {
                fail("Linear Probing hash failed insertion #" + i + ". Error message: " + errorData(t));
            }


            try {
                qp.put(randomNumber, randomNumber2);
            } catch (NoMorePrimesException ignored) {
                // To have this exception thrown is not a problem; we have a finite #primes to generate resizings for.
            } catch (Throwable t) {
                fail("Quadratic Probing hash failed insertion #" + i + ". Error message: " + errorData(t));
            }
        }

    }

    @Test
    public void testSCProbes() {
        SeparateChainingHashTable sc = new SeparateChainingHashTable();

        sc.put("Arnold", "894-59-0011");
        sc.put("Tiffany", "894-59-0011");
        sc.put("Jessie", "705-12-7500");
        sc.put("Mary", "888-1212-3340");

        int idx = sc.hash("Arnold");
        assertEquals(1, sc.get(idx).getValue("Arnold").getProbes());
        assertEquals("894-59-0011", sc.get("Arnold"));
        idx = sc.hash("Tiffany");
        assertEquals(1, sc.get(idx).getValue("Tiffany").getProbes());
        idx = sc.hash("Jessie");
        assertEquals(2, sc.get(idx).getValue("Jessie").getProbes());
        idx = sc.hash("Mary");
        assertEquals(1, sc.get(idx).getValue("Mary").getProbes());

        // Search fail
        idx = sc.hash("Jerry");
        assertEquals(2, sc.get(idx).getKey("Jerry").getProbes());
        assertEquals(null, sc.remove("Jerry"));

        idx = sc.hash("Arnold");
        assertEquals(1, sc.get(idx).getValue("Arnold").getProbes());
        sc.remove("Arnold");

        idx = sc.hash("Tiffany");
        assertEquals(1, sc.get(idx).getValue("Tiffany").getProbes());
        sc.remove("Tiffany");

        idx = sc.hash("Jessie");
        assertEquals(1, sc.get(idx).getValue("Jessie").getProbes());
        sc.remove("Jessie");
        
        idx = sc.hash("Mary");
        assertEquals(1, sc.get(idx).getValue("Mary").getProbes());
        sc.remove("Mary");
    }


    @Test
    public void testLProbes() {

        LinearProbingHashTable lp = new LinearProbingHashTable(false);
        
        lp.put("Arnold", "894-59-0011");
        lp.put("Tiffany", "894-59-0011");
        lp.put("Jessie", "705-12-7500");
        lp.put("Mary", "888-1212-3340");

        assertEquals("Arnold", lp.get(lp.hash("Arnold")).getKey());
        assertEquals("894-59-0011", lp.get("Arnold"));
        assertEquals("Tiffany", lp.get(lp.hash("Tiffany")).getKey());
        assertEquals("Jessie", lp.get( (lp.hash("Jessie")+1)%lp.capacity() ).getKey());
        assertEquals("Mary", lp.get(lp.hash("Mary")).getKey());

        // Search fail
        assertEquals(null, lp.get("Jerry"));
        assertEquals(null, lp.remove("Jerry"));

        assertEquals("705-12-7500", lp.remove("Jessie"));
        assertEquals("894-59-0011", lp.remove("Arnold"));
        assertEquals("894-59-0011", lp.remove("Tiffany"));
        assertEquals("888-1212-3340", lp.remove("Mary"));



    }

    @Test
    public void testResizeSoftLProbes() {

        LinearProbingHashTable lp = new LinearProbingHashTable(true);
        String[] add1 = new String[]{"Tiffany", "Helen", "Alexander", "Paulette", "Jason", "Money", "Nakeesha", "Ray", "Jing", "Amg"};
        String[] remove1 = new String[]{"Helen", "Alexander", "Paulette", "Jason", "Money", "Nakeesha", "Ray", "Jing", "Amg"};
        String[] add2 = new String[]{"Christine", "Carl"};

        for(String s: add1) {
            lp.put(s, s);
        }

        for (String s: remove1) {
            lp.remove(s);
        }

        for(String s: add2) {
            lp.put(s, s);
        }

        assertEquals("After additions and deletions, and additions again, the capacity should be 23, but get " + lp.capacity() + ".", 23, lp.capacity());

        
        lp.put("Terry", "new");
        System.out.println(lp.toString());
        assertEquals("After additions and deletions, and additions again, resize should be triggered and the capacity should be 43, but get " + lp.capacity() + ".", 43, lp.capacity());

    }

    //separate chaining tests below
    @Test
    public void separateChainingTestAdd() {
    	SeparateChainingHashTable sc = new SeparateChainingHashTable();
    	sc.put("tester", "x1");
    	sc.put("tester", "x2");
    	System.out.println("testing " + sc.get("tester"));
    }
    @Test
    public void separateChainingTestAddTestingSize() {
    	SeparateChainingHashTable sc = new SeparateChainingHashTable();
    	sc.put("c", "x1");
    	sc.put("cf", "x2");
    	sc.put("b", "b value");
    	System.out.println(sc.size());
    	System.out.println(sc.remove("tester"));
    	sc.remove("cf");
    	sc.put("a", "a value");
    	sc.put("bc", "bc value");
    	sc.put("ab", "ab value");
    	sc.put("cd", "cd value");
    	
    	sc.enlarge();
    	sc.shrink();
    	System.out.println(sc.size());
    	
    	sc.print();
    }
    
    
    //linear probing tests below
    @Test
    public void linearprobingtestAdd() {
    	//look for collision patterns 
    	LinearProbingHashTable lp = new LinearProbingHashTable(true);
    	lp.put("jane", "jane is bad");
    	System.out.println(lp.capacity());
    	lp.put("doe", "i like pizza");
    	System.out.println(lp.capacity());
    	lp.put("john", "john is a generic name");
    	System.out.println(lp.capacity());
    	lp.put("jack", "i don't know what to say");
    	System.out.println(lp.capacity());
    	lp.put("jack2", "i don't know what to say 2");
    	System.out.println(lp.capacity());
    	System.out.println(lp.get("jane"));
    	System.out.println(lp.get("jack"));
    	System.out.println(lp.get("test"));
    	System.out.println(lp.containsKey("test"));
    	System.out.println(lp.containsKey("jane"));
    	System.out.println(lp.containsValue("test"));
    	System.out.println(lp.containsValue("jane is bad"));
    	System.out.println(lp.toString());
    	//lp.put("testing", null);
    }
    @Test
    public void linearprobingtestAddCheckingLocations() {
    	LinearProbingHashTable lp = new LinearProbingHashTable(true);
    	//lp.put("a", "test value 1");
    	//lp.put("ce", "testing");
    	//lp.put("cf", "another test");
    	//a hashes to 6
    	//ab hashes to 4
    	//lp.put("bc", "i like pizza");
    	//b hashes to 0
    	//bc hashes to 1
    	KVPair y = new KVPair("cc", "argg");
    	KVPair x = new KVPair("cd", "arg");
    	lp.put(x.getKey(), x.getValue());			//put cd
    	lp.put("g", "test");
    	lp.put("ab", "test 2");
    	//lp.put("cc", "test 2");
    	lp.put(y.getKey(), y.getValue());			//put cc
    	//should have 5 elements only
    	assertEquals(x.getKey(), lp.get(5).getKey());
    	assertEquals(x, lp.get(5));
    	assertEquals(y, lp.get(0));
    	lp.put("ca", "testing 3");
    	//c hashes to 1
    	//ca hashes to 2
    	//cf hashes to 0
    	//ce hashes to 6
    	//cc hashes to 4
    	//cd hashes to 5
    	//g hashes to 5
    	System.out.println(lp.toString());
    	//lp.put("c", "john is a generic name");
    	//lp.put("d", "i don't know what to say");
    	//System.out.println(lp.toString());
    	//lp.put("e", "i don't know what to say 2");
    	//lp.put("f", "G");
    	//System.out.println(lp.toString());
    }
    @Test
    public void linearprobingtestAddHardDelete() {
    	LinearProbingHashTable lp = new LinearProbingHashTable(false);
    	KVPair y = new KVPair("cc", "argg");
    	KVPair x = new KVPair("cd", "arg");
    	lp.put(x.getKey(), x.getValue());			//put cd
    	lp.put("g", "test g");
    	lp.put("ab", "test 2");
    	//lp.put("cc", "test 2");
    	lp.put(y.getKey(), y.getValue());			//put cc
    	//should have 5 elements only    	    	
    	System.out.println(lp.remove("cc"));
    	System.out.println(lp.size());
    	System.out.println(lp.toString());
    	}
    
    @Test
    public void linearprobingtestAddSoftDelete() {
    	LinearProbingHashTable lp = new LinearProbingHashTable(true);
    	KVPair y = new KVPair("cc", "argg");
    	KVPair x = new KVPair("cd", "arg");
    	lp.put(x.getKey(), x.getValue());			//put cd
    	lp.put("g", "test g");
    	lp.put("ab", "test 2");
    	//lp.put("cc", "test 2");
    	lp.put(y.getKey(), y.getValue());			//put cc
    	//should have 5 elements only    	    	
    	System.out.println(lp.remove("cc"));
    	System.out.println(lp.size());
    	lp.put("cf", "cee f");
    	System.out.println(lp.toString());
    }
    @Test
    public void linearprobingtestSoftDelete() {
    	//do we resize on tombstones?
    	LinearProbingHashTable lp = new LinearProbingHashTable(true);
    	lp.put("g", "test g");
    	lp.put("ab", "test 2");
    	lp.put("cc", "test 2");
    	lp.put("cf", "cee f");
    	lp.remove("cf");
    	//lp.put("b", "b value");
    	System.out.println(lp.toString());
    	System.out.println(lp.size());
    
    }
    
    //ordered linear probing tests below <-----------------------------------------------------------
    //NOTE: MIGHT HAVE TO FIX ORDERED LINEAR PROBING DELETION ON TOMBSTONES AND MAKE NEW VAR
    @Test
    public void orderedLinearProbingTestAdd() {
    	OrderedLinearProbingHashTable lp = new OrderedLinearProbingHashTable(false);
    	lp.put("feather", "test feather");
    	lp.put("fiscal", "test fiscal");
    	//lp.put("cc", "test 2");
    	//should have 5 elements only    	    	
    	lp.put("fang", "test fang");
    	System.out.println(lp.toString());
    }
    
    //quadratic probing tests below <----------------------------------------------------------------
    @Test
    public void QuadraticProbingTestAddCollision2i() {
    	QuadraticProbingHashTable lp = new QuadraticProbingHashTable(false);
    	lp.put("b", "test feather");
    	//lp.put("c", "test fiscal");
    	lp.put("cf", "bee test");
    	lp.put("ca", "not null");
    	lp.put("az", "x");
    	//lp.put("d", "also not null");
    	//lp.put("cc", "test 2");
    	//should have 5 elements only    	    	
    	System.out.println(lp.toString());
    }
    @Test
    public void QuadraticProbingTestAddCollisionWrapAround() {
    	QuadraticProbingHashTable lp = new QuadraticProbingHashTable(false);
    	lp.put("ce", "test feather");
    	lp.put("h", "test fiscal");
    	lp.put("ad", "value 2");
    	lp.put("abby", "value 3");
    	lp.put("henry", "testin again");
    	lp.put("joey", "55555");
    	//lp.put("d", "also not null");
    	//lp.put("cc", "test 2");
    	lp.remove("h");
    	System.out.println(lp.toString());
    	System.out.println("value is " + lp.get("joeyy"));
    }
    //failing quadratic probing
    //testing hard deletion and soft deletion
    //all just contains key
    //either something was not successfully added or deleted
}

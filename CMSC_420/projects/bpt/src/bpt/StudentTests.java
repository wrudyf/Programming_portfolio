package bpt;
import org.junit.Test;

import static org.junit.Assert.*;

import java.util.Iterator;

/**
 * A jUnit test suite for {@link BinaryPatriciaTrie}.
 *
 * @author --- YOUR NAME HERE! ----.
 */
public class StudentTests {


    @Test public void testEmptyTrie() {
        BinaryPatriciaTrie trie = new BinaryPatriciaTrie();

        assertTrue("Trie should be empty",trie.isEmpty());
        assertEquals("Trie size should be 0", 0, trie.getSize());

        assertFalse("No string inserted so search should fail", trie.search("0101"));

    }

    @Test public void testFewInsertionsWithSearch() {
        BinaryPatriciaTrie trie = new BinaryPatriciaTrie();

        assertTrue("String should be inserted successfully",trie.insert("00000"));
        assertTrue("String should be inserted successfully",trie.insert("00011"));
        assertFalse("Search should fail as string does not exist",trie.search("000"));

    }


    //testing isEmpty function
    @Test public void testFewInsertionsWithDeletion() {
        BinaryPatriciaTrie trie = new BinaryPatriciaTrie();

        trie.insert("000");
        trie.insert("001");
        trie.insert("011");
        trie.insert("1001");
        trie.insert("1");

        assertFalse("After inserting five strings, the trie should not be considered empty!", trie.isEmpty());
        assertEquals("After inserting five strings, the trie should report five strings stored.", 5, trie.getSize());

        trie.delete("0"); // Failed deletion; should affect exactly nothing.
        assertEquals("After inserting five strings and requesting the deletion of one not in the trie, the trie " +
                "should report five strings stored.", 5, trie.getSize());
        assertTrue("After inserting five strings and requesting the deletion of one not in the trie, the trie had some junk in it!",
                trie.isJunkFree());

        trie.delete("011"); // Successful deletion
        assertEquals("After inserting five strings and deleting one of them, the trie should report 4 strings.", 4, trie.getSize());
        assertTrue("After inserting five strings and deleting one of them, the trie had some junk in it!",
                trie.isJunkFree());
    }
    
    @Test
    public void testBasicInserSplit3() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("001");
        trie.insert("0010");    	trie.insert("0100");
        
        System.out.println("size is" + trie.getSize());
    }
    @Test
    public void testBasicInsertPushDown1() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("001");
        trie.insert("0010");
        trie.insert("0");
        System.out.println("size is" + trie.getSize());
    }
    
    @Test
    public void testBasicInsertConsume() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("00");
    	trie.insert("00000");
    	//trie.insert("10");
    	//trie.insert("100");
    	
    	System.out.println("size is " + trie.getSize());
    	System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 011 in the trie? " + trie.search("011") );
    	System.out.println("is 0 in the trie? " + trie.search("0"));
    }
    @Test
    public void testBasicInsertPushDown() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("000");
    	trie.insert("00");
    	//trie.insert("10");
    	//trie.insert("100");
    	
    	System.out.println("size is " + trie.getSize());
    	System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 011 in the trie? " + trie.search("011") );
    	System.out.println("is 0 in the trie? " + trie.search("0"));
    }
    
    @Test
    public void testBasicInsertSplit() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("000");
    	trie.insert("010");
    	trie.insert("0");
    	//trie.insert("10");
    	//trie.insert("100");
    	System.out.println("size is " + trie.getSize());
    	System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 011 in the trie? " + trie.search("011") );
    	System.out.println("is 0 in the trie? " + trie.search("0"));
    }
   
    
    @Test
    public void testBasicInsertMixed() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("00");
    	trie.insert("001");
    	trie.insert("000");
    	//trie.insert("001");
    	trie.insert("0");
    	//trie.insert("010");
    	//trie.insert("10");
    	//trie.insert("100");
    	//trie.insert("011");
    	System.out.println("size is " + trie.getSize());
    	System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	//System.out.println("is 011 in the trie? " + trie.search("011") );
    	//System.out.println("is 0 in the trie? " + trie.search("0"));
    }
    
    @Test
    public void testBasicInsertAndSearch() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("00");
    	//trie.insert("1");
    	trie.insert("0011");
    	trie.insert("000");
    	//trie.insert("011");
    	
    	System.out.println("size is " + trie.getSize());
    	//System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 0 in the trie? " + trie.search("0") );
    	System.out.println("stop");
    	System.out.println("is 00 in the trie? " + trie.search("00") );
    	System.out.println("is 0011 in the trie?" + trie.search("0011"));
    	System.out.println("is 1 in the trie? " + trie.search("1"));
    	System.out.println("is 011 in the trie? " + trie.search("011"));
    }
    
    @Test
    public void testBasicInsertAndSearc2h() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("0010");
    	trie.insert("1");
    	trie.insert("10");
    	trie.insert("11");
    	trie.insert("00100");
    	trie.insert("00101");
    	trie.insert("0011");
    	//trie.insert("011");
    	
    	System.out.println("size is " + trie.getSize());
    	//System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 0 in the trie? " + trie.search("0") );
    	System.out.println("stop");
    	System.out.println("is 00 in the trie? " + trie.search("00") );
    	System.out.println("is 0011 in the trie?" + trie.search("0011"));
    	System.out.println("is 1 in the trie? " + trie.search("1"));
    	System.out.println("is 011 in the trie? " + trie.search("011"));
    }
    @Test
    public void testBasicInsertAndSearc3() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("0011");
    	trie.insert("00110");
    	trie.insert("1");
    	trie.insert("00111");
    	
    	trie.insert("0010");
    	//trie.insert("011");
    	
    	System.out.println("size is " + trie.getSize());
    	//System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 0 in the trie? " + trie.search("0") );
    	System.out.println("stop");
    	System.out.println("is 00 in the trie? " + trie.search("00") );
    	System.out.println("is 0011 in the trie?" + trie.search("0011"));
    	System.out.println("is 1 in the trie? " + trie.search("1"));
    	System.out.println("is 011 in the trie? " + trie.search("011"));
    }
    @Test
    public void testBasicInsertAndSplit() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("0011");
    	trie.insert("001001");
    	//trie.insert("011");
    	
    	System.out.println("size is " + trie.getSize());
    	//System.out.println("is 0111 in the trie? " + trie.search("0111"));
    	System.out.println("is 0 in the trie? " + trie.search("0") );
    	System.out.println("stop");
    	System.out.println("is 00 in the trie? " + trie.search("00") );
    	System.out.println("is 0011 in the trie?" + trie.search("0011"));
    	System.out.println("is 1 in the trie? " + trie.search("1"));
    	System.out.println("is 011 in the trie? " + trie.search("011"));
    }
    @Test
    public void testBasicInsertAndDelete() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("0");
    	trie.insert("00");
    	trie.insert("1");
    	trie.delete("0");
    	System.out.println("size is " + trie.getSize());
    }
    @Test
    public void testDeleteJunk() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	//insertions
    	trie.insert("0");
    	trie.insert("00");
    	trie.insert("01");
    	//deletions
    	trie.delete("0");
    	trie.delete("00");
    	trie.delete("01");
    	System.out.println("size is " + trie.getSize());
    }
    @Test 
    public void getLongestSimple() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("000");
    	trie.insert("1");
    	trie.insert("11");
    	trie.insert("111");
    	trie.insert("0001");
    	//node, pick side with more one's in it as a quick fix
    	System.out.println("longest string should be " + trie.getLongest());
    }
    
    @Test 
    public void getLongestSimpleOne() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("10");
    	trie.insert("00");
    	//node, pick side with more one's in it as a quick fix
    	System.out.println("longest string should be " + trie.getLongest());
    }
    
    //ITERATOR TESTS BELOW HERE
    @Test
    public void iteratorTestSimple() {
    	BinaryPatriciaTrie trie = new BinaryPatriciaTrie();
    	trie.insert("0");
    	trie.insert("00");
    	trie.insert("01");
    	trie.insert("1");
    	Iterator <String> iterator = trie.inorderTraversal();
    	while (iterator.hasNext()) {
    		System.out.println("ITEM: " + iterator.next());
    	}
    }
    
}
use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/** 
    These traits are implemented for Nodes to make them comparable 
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/** 
    You must implement the above trait for the vector type 
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {

        self.push(ele);
        let mut index = 0;
        //min heapify here
                
        loop{
            if index == self.len(){break}  
            //check to see if 2 * idx + 1 is in bounds
            if (index * 2 + 1) < self.len(){
                //swap if it's greater than parent
                if self[index] > self[index * 2 + 1]{
                    
                    self.swap(index, index * 2 + 1);
                }
            }
            //check to see if 2 * idx + 2 is in bounds
            if (index * 2 + 2) < self.len(){
                //swap if it's greater than parent
                if self[index] > self[index * 2 + 1]{
                    
                    self.swap(index, index * 2 + 2);
                }
            }

            index = index + 1;
        }
    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
        if self.len() > 1{
            let last = self.len() - 1;
            //swap first and last element
            self.swap(0, last);
            //extract value from pop and delete last elem
            let x = self.pop();
            //min heapify the thing
            let mut index = 0;
            
            loop{
                if index == self.len(){break}  
                //check to see if 2 * idx + 1 is in bounds
                if (index * 2 + 1) < self.len(){
                    //swap if parent is greater
                    if self[index] > self[index * 2 + 1]{
                        self.swap(index, index * 2 + 1);
                    }
                }
                if (index * 2 + 2) < self.len(){
                    //swap if parent is greater
                    if self[index] > self[index * 2 + 2]{
                        self.swap(index, index * 2 + 2);
                    }
                }
                    index = index + 1;            

            }

            return x
        }

        else {
            if self.len() == 0{return None}
            let x = self.pop();
            return x
        }
    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.len() == 0{
            return None
        }
        else{
            return Some(&self[0])
        }
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let (p1x, p1y) = p1;
    let (p2x, p2y) = p2;
    let xd = p1x - p2x;
    let yd = p1y - p2y;
    return xd.abs() + yd.abs()
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut pairs = HashMap::new();
    let mut name = "";
    let mut smallest_dist = 9999;
    let mut current_dist = 9999;
    for (enemy, e_coors) in enemies.iter(){
        for (ally, a_coors) in allies.iter(){
                current_dist = distance(*e_coors, *a_coors);
                if current_dist < smallest_dist{
                    smallest_dist = current_dist;
                    name = ally;                    
                }                         
            
        }
        //insert name and current enemy onto hashmap
        pairs.insert(enemy, name);
        smallest_dist = 9999;
        current_dist = 9999;
    }
    
    //get enemy name
    for (enemy, ally) in pairs.iter(){
        if ally.to_string() == "Stark"{
            name = enemy;
        }
    }
    let mut a = 0;
    let mut b = 0;
    //get coors
    for (enemy, e_coors) in enemies.iter(){
        if name == enemy.to_string(){

            (a, b) = *e_coors;
        }
    }
    
//    println!("name is {}", name.to_string());
    return (name, a, b);  
}



/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/

pub fn gauss(n: i32) -> i32 {
    if n < 0 {-1}
    else{    
    let mut step = 0;
    let mut count = 0;
    loop {
    if step == n{break}
    else{
    step = step + 1;
    count = count + step;
    }
    }
    return count;
}
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut index = 0;
    let mut count = 0;
    loop{
    if index == ls.len() {break}
    else {
    	 if ls[index] <= e && ls[index] >= s{
	 count = count + 1;
	 index = index + 1;
	 }
	 else{
	 index = index + 1;
	 }	 
    }
    }
    return count;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    if target.len() == 0 {
        return true    
    }
    if target.len() > set.len()  {
        return false
    }
    let mut count = 0;
    let mut i = 0;
    let mut j = 0;
    loop{
        if i == set.len(){break}
        if j == target.len(){break}
        if set[i] == target[j]{

        i = 0;
        j = j + 1;
        count = count + 1;
        continue
        }

        i = i + 1;
    }
    if count == target.len(){return true}
    return false

}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
let mut step = 0;
let mut mean = 0.0;
if ls.len() == 0 {
   return None 
}
else{
	loop{
	if step == ls.len(){break}
	else{
	mean = mean + ls[step];
	step = step + 1;
	}	
	}
}
mean = mean / ls.len() as f64;
return Some(mean)

}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
let mut n:i32 = 0;
let mut index = 0;
    loop{
    if index == ls.len(){break}
    else {
    let  x = {
    	let mut x1 = 1;
	let mut i = 0;
	
	loop{
		if i == index {break}
		else{
		x1 = x1 * 2;
		
		i = i + 1;
		}
	}
	if index == 0
	{x1 = 1}
	if index == 1
	{x1 = 2}	
	x1
    };
//    println!("{} {}",x, lst[lst.len() - index - 1]);
    n = n + (ls[ls.len() - index - 1] * x);
    index = index + 1;
    }
    }
return n
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/

fn is_prime (x: u32) -> bool{
    if x <= 0 || x == 1{return false}
    if x == 2 {return true}

    let mut fact_count = 0;
    let mut num = 1;

    loop{
        if num == x {
            if x % num == 0 {fact_count = fact_count + 1}
            break
        }
        if x % num == 0 {fact_count = fact_count + 1}
        num = num + 1;
    }
    if fact_count > 2 {return false}            
    else{
        return true    
    }
}

pub fn factorize(n: u32) -> Vec<u32> {
    let mut facts = Vec::new();
    let mut current_biggest_prime = 2;
    let mut res = n;
    loop{
        let rem = res % current_biggest_prime;
        if rem == 0 {
        facts.push(current_biggest_prime);
        res = res / current_biggest_prime;
        if res == 1 {break}        
        }
        else{
            let mut next_biggest_prime = current_biggest_prime + 1;
            loop{
                if is_prime(next_biggest_prime) == true {break}
                next_biggest_prime = next_biggest_prime + 1; 
            }
            current_biggest_prime = next_biggest_prime;
        }
    }

    return facts
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    if lst.len() == 0{
                
        return Vec::new();
    }
    if lst.len() == 1{
        let mut rotated_i = Vec::new();
        rotated_i.push(lst[0]);
        return rotated_i;
    }
    else{
    let mut rotated = Vec::new();
    let mut index = 1;
    loop{
        if index == lst.len(){break}
        rotated.push(lst[index]);
        index = index + 1;
    }    
    rotated.push(lst[0]);
    return rotated;
    }
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if target.len() > s.len() {return false}
    if target.len() == 0 {return true}
    let mut index = 0;
    let mut t_index = 0;
    let mut m_count = 0;
    let arr:Vec<_> = s.chars().collect();
    let sub:Vec<_> = target.chars().collect();
    loop{
        if index == arr.len(){break}
        if arr[index] == sub[t_index]{
            m_count = m_count + 1;
            if m_count == sub.len(){
            return true;

            }
            t_index = t_index + 1;
        }
        else{
            t_index = 0;
        }
        index = index + 1;
    }
    return false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.len() == 0{return None}
    let arr:Vec<_> = s.chars().collect();
    let mut current_char = arr[0];
    let mut current_count = 0;
    let mut current_longest_count = 0;
    let mut current_longest_pos = 0;
    let mut index = 0;
    loop{
        if index == arr.len(){break}
            if current_char == arr[index] {
                current_count = current_count + 1;
                if current_count > current_longest_count{
                    current_longest_count = current_count;

                    current_longest_pos = index;
                }
            }
            else{
                current_char = arr[index];
                current_count = 1;
            }
        index = index + 1;
    }
//    println!("{} {}", current_longest_pos, current_longest_count);

    let s_slice: &str = &s[current_longest_pos -(current_longest_count - 1)..current_longest_pos + 1];
    
    Some(&s_slice)
}

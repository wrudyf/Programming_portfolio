def fib(n)
  if n == 0
    []
  elsif n == 1
    [0]
  elsif n == 2
    [0,1]
  else
    arr = [0,1,1]
    for i in 3..n-1 do
      arr.push(arr[i-1] + arr[i-2])
    end
    arr
  end
end

def isPalindrome(n)
  s = n.to_s
  for i in 0..(s.length/2) - 1
    if s[i] != s[s.length - 1 - i]
      return false
    end
  end
  true
end

def nthmax(n, a)
  mergesort(a)
  
  if n < a.length
    return a[a.length - n - 1]
  end
  return nil
end

def mergesort(array)
  if array.length > 1
    m = array.length / 2
    l = array[0..m-1]
    r = array[m..array.length - 1]
    l = mergesort(l)
    r = mergesort(r)

    i = 0
    j = 0
    k = 0
    while i < l.length && j < r.length
      if l[i] <= r[j]
        array[k] = l[i]
        i = i + 1
        k = k + 1
      else
        array[k] = r[j]
        j = j + 1
        k = k + 1
      end
    end
    while i < l.length
      array[k] = l[i]
      i = i + 1
      k = k + 1
    end
    while j < r.length
      array[k] = r[j]
      j = j + 1
      k = k + 1
    end
  end
  return array
end

def freq(s)
  if s == ""
    return ""
  end

  counts = Hash.new
  
  for i in 0..s.length - 1
    if !counts.has_key?(s[i])
      counts[s[i]] = 0
    end
    counts[s[i]] += 1
  end
  puts counts
  max_letter = ''
  max_num = 0
  counts.each {|key, value|
    if value > max_num
      max_letter = key
      max_num = value
    end
  }
  return max_letter
end

def zipHash(arr1, arr2)
  if arr1.length != arr2.length
    return nil
  end
  zippo = Hash.new
  for i in 0..arr1.size - 1
    zippo[arr1[i]] = arr2[i]
  end
  return zippo
end

def hashToArray(hash)
  array = []
  hash.each {|key, value|
    array.push([key, value])
  }
  return array
end

def maxProcChain(init, procs)
  val = init
  for i in 0..procs.size - 1
    val = procs[i].call(init)
    if val > init
      init = val
    end
  end
  return val
  
end

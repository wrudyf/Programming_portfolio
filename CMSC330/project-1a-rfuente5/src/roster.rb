class Person
  def initialize(name, age)
    @name = name
    @age = age
  end
  def getAge
    return @age
  end
  def setAge(x)
    @age = x
    return self
  end
  def getName
    return @name
  end
end

class Student < Person
  def initialize(name, age, grade)
    @name = name
    @age = age
    @grade = grade
  end
  def getGrade
    return @grade
  end
  def changeGrade(x)
    @grade = x
  end
end

class Staff < Person
  def initialize(name, age, position)
    @name = name
    @age = age
    @position = position
  end
  def getPosition
    return @position
  end
  def changePosition(newPosition)
    @position = newPosition
  end
end

class Roster  
  def initialize
    @people = {}
    @num_of_people = 0
  end
  
  def add(person)
    @people[person.getName] = person
    @num_of_people = @num_of_people + 1
  end

  def size
    return @num_of_people
  end

  def remove(person)
    if (people.delete(person.getName))
      @num_of_people = @num_of_people - 1
    end
  end

  def getPerson(name)
    if (@people.has_key?(name))
      return @people[name]
    end
    return nil
  end

  def print
    @people.each{|name, val|
      puts "what?  #{val.getName}"
    }
  end
  def map
    if block_given?
      @people.each{|name, val|
        yield val
      }
    end
  end
end
#don't make assumption that it won't be called on a person that may not have proper method

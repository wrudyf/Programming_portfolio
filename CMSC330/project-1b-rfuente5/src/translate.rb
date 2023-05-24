class Translation
  def initialize (language, translation)
    @language = language
    @translation = translation
  end
  def getLanguage
    return @language
  end
  def getTranslation
    return @translation
  end
  def changeTranslation(newTrans)
    @translation = newTrans
  end
end
class Language
  def initialize(name)
    @name = name
    #wordsP holds word names as keys, and the parts of speech as values in array
    @wordsP = {}
    #wordsT holds word names as keys, and the translation as values in size 2 array
    @wordsT = {}
    @grammar = []
  end
  
  #add word to hash that holds parts of speech as values
  def addWordP(word, part)
    #if we don't have the word key, add it
    if !@wordsP.has_key?(word)
      @wordsP[word] = [part]
    else
      #if we do have the word, add the part of speech by pushing to array
      #but only if we don't already have that part of speech
      if !@wordsP[word].include?(part)
        @wordsP[word].push(part)
      end
    end
  end
  
  #add word to hash that holds translations as values ex of trans: ["french", "le"]
  def addWordT(word, translation)
    #if we don't have the word, add it and the translation
    if !@wordsT.has_key?(word)
      @wordsT[word] = []
      @wordsT[word].push(translation)
    else
      #if we have the word, search the translations to see if we already have the translation
      #if we already have the translation, change it. otherwise, add new translation
      exists = false
      @wordsT[word].each{|tran|
        #if we find the exact same language, update translation
        if tran.getLanguage == translation.getLanguage
          exists = true
          tran.changeTranslation(translation.getTranslation)
        end
      }
      #if we didn't find an exact match, add new translation
      if exists == false
        @wordsT[word].push(translation)
      end
    end
  end

  def updateGrammar(gramm)
    @grammar = []
    gramm.each {|part|
      @grammar.push(part)
    }
  end

  def showWordsP
    puts "Showing words with parts of speech"
    @wordsP.each{|word, value|
      puts "====Word:  #{word} ---Part of speech: #{value}"
    }
  end

  def showWordsT
    puts "Showing words with corresponding translations"
    @wordsT.each{|word, translation_array|
      print "===Word: #{word} Trans->"
      translation_array.each{|translation|
        print " #{translation.getLanguage } -- #{translation.getTranslation}"
      }
      puts ""
    }
  end

  def showGrammar
    puts "Showing grammar below"
    @grammar.each{|part|
      print "#{part} "
    }
    puts "\n"
  end
  def getName
    return @name
  end
  def getWordsP
    return @wordsP
  end
  def getWordsT
    return @wordsT
  end
  def getGrammar
    return @grammar
  end
end

class Translator
  def initialize(words_file, grammar_file)
    @languages = []
    @languages.push(Language.new("English"))
    #===FIRST take in the words file data
    f = File.open(words_file)
    line = f.gets
    while line
      #process line to see if it meets requirements
      if line =~ /^([a-z-]+), ([A-Z]{3}), (([A-Z][a-z0-9]+:[a-z-]+)|([A-Z][a-z0-9]+:[a-z-]+, )+)+$/
        #add word to english lang first
        @languages[0].addWordP($1, $2)
        
        #get the languages that we have to add translations for
        index = $1.size + 7
        translations_line = line[index..]
        comp = translations_line.split(',')  
        langs = []
        words = []
        comp.each{|langword|
          info = langword.split(':')
          lang = info[0].strip
          word = info[1].strip
          langs.push(lang)
          words.push(word)
          if languageExists(lang) == -1
            @languages.push(Language.new(lang))
          end
          #add each word to its language
          @languages[languageExists(lang)].addWordP(word,$2)
        }
        #add all translations for word to english first
        for i in 0..langs.size - 1
          @languages[0].addWordT($1, Translation.new(langs[i], words[i]))
        end
        
        #now add all translations for each word to its own language
        
        #hash will hold language names as keys and words as values to easily get the word to add for translation
        words_to_add = {}
        for i in 0..langs.size - 1
          words_to_add[langs[i]] = words[i]
        end
        #now we double loop to add translations for all languages
        for i in 0..langs.size - 1
          langIndex = languageExists(langs[i])
          for j in 0..langs.size - 1
            if @languages[langIndex].getName != langs[j]
              @languages[langIndex].addWordT(words_to_add[langs[i]], Translation.new(langs[j], words[j]))
            end
          end
        end
        #now we go through all languages once and add the english translation for each language
        words_to_add.each{|key, value|
          langIndex = languageExists(key)
          @languages[langIndex].addWordT(value, Translation.new("English", $1))
        }
      end
      line = f.gets
    end
    f.close
    #showLangsInfo

    #===SECOND take in the grammar file data
    f = File.open(grammar_file)
    line = f.gets
    while line
      if line =~ /^([A-Z][a-z0-9]+): ((([A-Z]{3})|([A-Z]{3}, )|([A-Z]{3}{[0-9]+})|([A-Z]{3}{[0-9]+}, ))+)$/
        #puts line
        #$1 is language, $2 is the string with all parts of speech

        components = $2.split(',')
        comp = []
        components.each{|part|
          comp.push(part)
        }
        #remove white space from each part in array
        comp.each{|part| 
          part.strip!
        }

        #check each comp to see if they have the {num} to make final array
        
        final_comp = []

        comp.each{|part|
          if part.include?("{")
          #if we do have {num}, check for num and add num of parts
            sp = part.index("{")
            ep = part.index("}")
            count = part[(sp+1)..(ep-1)].to_i
            for i in 0..count - 1 do
              final_comp.push(part[0..(sp - 1)])
            end
          else
            #if we don't have {num}, just push one part
            final_comp.push(part)
          end
        }
        #if language does not exist, we add it
        langIndex = languageExists($1)
        if langIndex == -1
          @languages.push(Language.new($1))
        end
        #update langindex to have new val if it did not exist before
        langIndex = languageExists($1)

        #now add each part to language grammar
        @languages[langIndex].updateGrammar(final_comp)
      end
      line = f.gets
    end
    f.close
    #showLangsInfo
  end
  def showLangsInfo
    puts "======Information about languages in our system: \n"
    @languages.each{|lang|
      puts "Language name: #{lang.getName}"
      lang.showWordsP
      lang.showWordsT
      puts "\n"
      puts "Grammar for #{lang.getName}"
      lang.showGrammar
    }
  end
  def languageExists(language)
    for i in 0..@languages.length - 1
      if language == @languages[i].getName
        return i
      end
    end
    return -1
  end
  
  # part 1
  
  def updateLexicon(inputfile)
    f = File.open(inputfile)
    line = f.gets

    while line
      if line =~ /^([a-z-]+), ([A-Z]{3}), (([A-Z][a-z0-9]+:[a-z-]+)|([A-Z][a-z0-9]+:[a-z-]+, )+)+$/
        #add to parts of speech hash first
        
        @languages[0].addWordP($1, $2)

        index = $1.size + 7
        translations_line = line[index..]
        comp = translations_line.split(',')
        langs = []
        words = []
        comp.each{|langword|
          info = langword.split(':')
          lang = info[0].strip
          word = info[1].strip
          langs.push(lang)
          words.push(word)
          if languageExists(lang) == -1
            @languages.push(Language.new(lang))
          end
          @languages[languageExists(lang)].addWordP(word, $2)
        }
        #add to translations hash second
        words_to_add = {}
        for i in 0..langs.size - 1
          words_to_add[langs[i]] = words[i]
        end

        for i in 0..langs.size - 1
          langIndex = languageExists(langs[i])
          for j in 0..langs.size - 1
            if @languages[langIndex].getName != langs[j]
              @languages[langIndex].addWordT(words_to_add[langs[i]], Translation.new(langs[j], words[j]))
            end
          end
        end
        words_to_add.each{|key, value|
          langIndex = languageExists(key)
          @languages[langIndex].addWordT(value, Translation.new("English", $1))
        }
      end
      line = f.gets
    end
    f.close
    #showLangsInfo
  end
  
  def updateGrammar(inputfile)
    f = File.open(inputfile)
    line = f.gets
    while line
      if line =~ /^([A-Z][a-z0-9]+): ((([A-Z]{3})|([A-Z]{3}, )|([A-Z]{3}{[0-9]+})|([A-Z]{3}{[0-9]+}, ))+)$/

        components = $2.split(',')
        comp = []
        components.each{|part|
          comp.push(part)
        }

        comp.each{|part|
          part.strip!
        }
        final_comp = []
        comp.each{|part|
          if part.include?("{")
            sp = part.index("{")
            ep = part.index("}")
            count = part[(sp+1)..(ep-1)].to_i
            for i in 0..count - 1 do
              final_comp.push(part[0..(sp-1)])
            end
          else
            final_comp.push(part)
          end
        }
        langIndex = languageExists($1)
        if langIndex == -1
          @languages.push(Language.new($1))
        end
        langIndex = languageExists($1)
        @languages[langIndex].updateGrammar(final_comp)
      end
      line = f.gets
    end

  end

  # part 2
  
  def generateSentence(language, struct)
    #check to see if language exists, if it doesn't, return nil
    
    index = languageExists(language)
    if index == -1
      return nil
    end
    #check to see if struct is array or language name
    gramm = []
    if struct.class == String
      #if it is string, then check to see if the lang exists
      index2 = languageExists(struct)
      #if it doesn't exist, return nil
      if index2 == -1
        return nil
      end
      #if no grammar exists, return nil
      if @languages[index2].getGrammar.length == 0
        return nil
      end
      #otherwise, get the language grammar and set it equal to our gramm
      gramm = @languages[index2].getGrammar.clone
    else
      #if struct is not a string, then set our gramm array to struct array
      gramm = struct.clone
      #if struct is empty array, return nil
      if gramm.length == 0
        return nil
      end
    end
    
    #now that we have grammar...we want to see if we can make a string with it
    #get all the words and parts of speech
    comps = @languages[index].getWordsP.clone
    
    sentence = []
    comps_arr = []
    
    #for each word, check to see if parts array has first struct string
    #maybe do double loop here to get all parts of speech
    comps.each{|word, part_array|
      part_array.each{|part|
        arr = [word, part]
        comps_arr.push(arr)
      }
    }

    #first check to see if we have needed parts, if we don't, return nil
    parts_needed = 0
    comps_arr2 = comps_arr.clone
    for i in 0..gramm.length - 1
      for j in 0..comps_arr2.length - 1
        if comps_arr2[j] != nil && gramm[i] == comps_arr2[j][1]
          parts_needed = parts_needed + 1
          #comps_arr2[j] = nil
        end
      end
    end
    if parts_needed < gramm.length
      return nil
    end

    #now make sentence
    for i in 0..gramm.length - 1
      for j in 0..comps_arr.length - 1

        if comps_arr[j] != nil && gramm[i] == comps_arr[j][1]
          sentence.push (comps_arr[j][0])
          #comps_arr[j] = nil
          break
        end
        
      end
    end
    sentence_string = ""
    sentence.each{|word|
      sentence_string = sentence_string + word + " "
    }

    sentence_string.strip!
    return sentence_string
  end
  
  def checkGrammar(sentence, language)
    langIndex = languageExists(language)
    #if language doesn't exist, return false
    if langIndex == -1
      return false
    end
    if sentence == nil
      return false
    end
    sentence2 = sentence.clone
    sentence_comps = sentence2.split(' ')
    words = @languages[langIndex].getWordsP.clone
    sentence_grammar = []
    sentence_comps.each{|comp|
      words.each{|key, value|
        if comp == key
          sentence_grammar.push(value)
        end
      }
    }
    count = 0
    #puts sentence_grammar
    language_grammar = @languages[langIndex].getGrammar.clone
    #if different lengths, grammars don't match and return false
    if sentence_grammar.length != language_grammar.length
      return false
    end
    for i in 0..sentence_grammar.length - 1
      #puts "#{sentence_grammar[i]} #{language_grammar[i]}"
      comps = sentence_grammar[i]
      comps.each{|part|
        if part == language_grammar[i]
          count = count + 1
          break
        end 
      }
    end
    if count != language_grammar.length
      #puts "NOT MATCHED???? #{count} #{language_grammar.length}"
      return false
    else
      return true
    end
    
  end
  
  def changeGrammar(sentence, struct1, struct2)
    gramm1 = []
    gramm2 = []
    #first, check to see if either struct1 or struct2 is a language
    #then check to see if either language doesn't exist
    #if either lang doesn't exist, return nil
    if struct1.class == String
      if languageExists(struct1) == -1
        return nil
      else
        gramm1 = @languages[languageExists(struct1)].getGrammar
        #puts gramm1
      end
    else
      gramm1 = struct1
    end
    if struct2.class == String
      if languageExists(struct2) == -1
        return nil
      else
        gramm2 = @languages[languageExists(struct2)].getGrammar
        #puts gramm2
      end
    else
      gramm2 = struct2
    end

    #if gramm1 length is not equal to gramm2 length then return nil
    if gramm1.length != gramm2.length
      return nil
    end
    part_count = 0
    gramm1copy = gramm1.clone
    gramm2copy = gramm2.clone
    #check to see if both grammars have the same part, if they don't
    #then we return nil
    for i in 0..gramm1copy.length - 1
      for j in 0..gramm1copy.length - 1
        if gramm1copy[i] == gramm2copy[j] && gramm1copy[i] != nil
          part_count = part_count + 1
          gramm1copy[i] = nil
          gramm2copy[j] = nil
        end
      end
    end
    if part_count != gramm1.length
      return nil
    end
    
    #after we assigned proper grammar structs, maybe we should 
    #split each word into component and match it to grammar
    #need same num of sentence comps and struct 1 grammar, if not return nil
    if sentence == nil
      return nil
    end
    sentence_words = sentence.split(' ')
    if sentence_words.length != gramm1.length
      return nil
    end

    #now assign sentence words their part of speech
    final_sentence = ""
    words = []
    for i in 0..sentence_words.length - 1

      arr = [gramm1[i], sentence_words[i]]
      words.push(arr)
    end
    #puts words
    #do final step of making sentence
    gramm2.each{|part|
      for i in 0..words.length - 1
        if words[i] != nil && part == words[i][0]
          final_sentence = final_sentence + words[i][1] + " "
          words[i] = nil
        end
      end
    }
    
    final_sentence.strip!
    return final_sentence 
  end
  
  # part 3
  
  def changeLanguage(sentence, language1, language2)
    #if neither language exists, return nil
    if languageExists(language1) == -1
      return nil
    end
    if languageExists(language2) == -1
      return nil
    end
    #if no sentence is given, return nil
    if sentence == ""
      return nil
    end
    
    sentence_words = sentence.split(' ')
    sentence_translation = []
    lang1words = @languages[languageExists(language1)].getWordsT.clone
    lang1words.each{|word, translation_array|
#      print "Word: #{word} "
      translation_array.each{|translation|
#        print "Language: #{translation.getLanguage} translation: #{translation.getTranslation}"
      }
#      puts ""
    }

    sentence_words.each{|sent_word|
      lang1words.each{|word, translation_array|
        translation_array.each{|translation|
          if sent_word == word && language2 == translation.getLanguage
            sentence_translation.push(translation.getTranslation)
          end
        }
      }
    }
    if sentence_words.length != sentence_translation.length
      return nil
    end
#    puts sentence_translation
    final_sentence = ""
    sentence_translation.each{|word|
      final_sentence = final_sentence + word + " "
    }
    return final_sentence.strip!

  end
  
  def translate(sentence, language1, language2)
    #check to see if lang 1 or lang 2 exists, if they don't return nil
    index1 = languageExists(language1)
    index2 = languageExists(language2)
    if index1 == -1 || index2 == -1
      return nil
    end
    #if empty string as sentence, return nil
    if sentence == ""
      return nil
    end
    sentence_gramm = changeGrammar(sentence, language1, language2)

    if sentence_gramm == nil
      return nil
    end
    return changeLanguage(sentence_gramm, language1, language2)
    
  end
end

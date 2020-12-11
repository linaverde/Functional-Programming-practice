/*Задан текст, состоящий из символов.
  Текст содержит предложения. Признаком конца предложения является точка.
  Построить список предложений.
  Для каждого предложения посчитать количество слов (слова отделяются пробелом).
  Найти предложение, число слов в котором наибольшее.
  Найти в каждом предложении слово максимальной длины и построить список таких слов. */

val str: String = "This is the text. We need to found the number of sentences. And something more."

//добавление значения в список строк
def add_in_str_array(arr: Array[String], value: String): Array[String] = {
  arr :+ value
}

//добавление значения в список чисел
def add_in_int_array(arr: Array[Int], value: Int): Array[Int] = {
  arr :+ value
}

//добавление значения в список списков слов
def add_in_word_array(arr: Array[Array[String]], value: Array[String]): Array[Array[String]] = {
  arr :+ value
}

//проверка является ли знак точккой
def is_dot(ch: Char): Boolean = {
  ch == '.'
}

//создание списка предложений из строки по точкам-разделителям
def sep_sentences(sent: Array[String], str: String, start: Int, end: Int): Array[String] = {
  if (end <= str.length) {
    if (is_dot(str.charAt(end))) {
      sep_sentences(add_in_str_array(sent, str.substring(start, end)), str, end + 2, end + 2)
    } else {
      sep_sentences(sent, str, start, end + 1)
    }
  } else {
    sent
  }
}

//вспомогательная функция для разделение предложения на слова
def sentences(str: String): Array[String] = {
  sep_sentences(Array.empty[String], str, 0, 0)
}

//проверка является ли знак пробелом
def is_space(ch: Char): Boolean ={
  ch == ' '

}

//разделение слов внутри строки
def sep_words(words: Array[String], str: String, start: Int, end: Int): Array[String] ={
  if (end < str.length) {
    if (is_space(str.charAt(end))) {
      sep_words(add_in_str_array(words, str.substring(start, end)), str, end + 1, end + 2)
    } else {
      sep_words(words, str, start, end + 1)
    }
  } else {
    add_in_str_array(words, str.substring(start, end))
  }
}

//разделение сохранение слов каждого предложения в массив
def get_words_array(arr: Array[String], words: Array[Array[String]] = Array.empty[Array[String]],
                    i: Int = 0): Array[Array[String]]={
  if (i < arr.length){
    get_words_array(arr, add_in_word_array(words,
      sep_words(Array.empty[String], arr(i), 0, 0)), i+1)
  } else {
    words
  }
}

//функция для подсчета количества  слов
def count_words(words: Array[Array[String]], res: Array[Int] = Array.empty[Int],
                i: Int = 0): Array[Int] = {
  if(i < words.length){
    count_words(words, add_in_int_array(res, words(i).length), i+1)
  } else {
    res
  }
}

//поиск самого длинного предложения
def get_longest_sent(len: Array[Int], sent:Array[String],
                     max_len: Int = 0, max_i: Int = 0, i: Int = 0): String = {
  if (i < len.length){
    if(len(i) < max_len) {
      get_longest_sent(len, sent, len(i), i, i+1)
    } else {
      get_longest_sent(len, sent, max_len, max_i, i+1)
    }
  } else {
    sent(max_i)
  }
}

//поиск самого длинного слова
def get_longest_word(word: Array[String], res: String = "", i: Int = 0): String ={
  if (i < word.length){
    if(word(i).length > res.length) {
      get_longest_word(word, word(i),  i+1)
    } else {
      get_longest_word(word, res,  i+1)
    }
  } else {
    res
  }
}

//массив из самых длинных слов каждого преложения
def get_longest_words_array(words: Array[Array[String]], res: Array[String] = Array.empty[String],
                            i: Int = 0): Array[String] ={
  if (i < words.length){
    get_longest_words_array(words, add_in_str_array(res, get_longest_word(words(i))), i+1)
  } else {
    res
  }
}


val sent = sentences(str)
print("Number of sentences: " + sent.length)
print("Longest sentence is: '" + get_longest_sent(count_words(get_words_array(sent)), sent) + "'")
print("Longest words list: " + get_longest_words_array(get_words_array(sent)).mkString(", "))



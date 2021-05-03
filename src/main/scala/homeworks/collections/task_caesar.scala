package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  val letters = ('A' to 'Z').toVector

  val lettersMap = letters.zipWithIndex.toMap

  /**
    * В данном задании Вам предлагается реализовать функции,
    * реализующие кодирование/декодирование строки шифром Цезаря.
    * https://ru.wikipedia.org/wiki/Шифр_Цезаря
    * Алфавит - прописные латинские буквы от A до Z.
    * Сдвиг   - неотрицательное целое число.
    * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
    */
  /**
    * @param word   входное слово, которое необходимо зашифровать
    * @param offset сдвиг вперёд по алфавиту
    * @return зашифрованное слово
    */
  def encrypt(word: String, offset: Int): String = {
    val newOffset = offset % 26

    word.toSeq
      .map(letter => newIndex(lettersMap(letter), newOffset, letters.size))
      .map(letters(_))
      .mkString("")
  }
  task"Реализуйте метод `encrypt`" ()

  /**
    * @param cipher шифр, который необходимо расшифровать
    * @param offset сдвиг вперёд по алфавиту
    * @return расшифрованное слово
    */
  def decrypt(cipher: String, offset: Int): String = {
    val newOffset = offset % 26

    cipher.toSeq
      .map(
        letter => newDecryptedIndex(lettersMap(letter), newOffset, letters.size)
      )
      .map(letters(_))
      .mkString("")

  }
  task"Реализуйте метод `decrypt`" ()

  private def newIndex(index: Int, offset: Int, size: Int): Int = {
    val newIndex = index + offset
    if (newIndex >= size) newIndex - size else newIndex
  }

  private def newDecryptedIndex(index: Int, offset: Int, size: Int) = {
    val newIndex = index - offset
    if (newIndex < 0) size + newIndex else newIndex
  }
}

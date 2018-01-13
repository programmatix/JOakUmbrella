package parser

case class WordFreq(word: String, count: Int) {
  override def toString = "Word <" + word + "> " +
    "occurs with frequency " + count
}

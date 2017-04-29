class Key[T] {
  var proto: T = _
  def nop(t: T) = t
}

trait Injection {
  def inject[T](key: Key[T]): T = {
    key match {
      case Key[String]() => key.nop("The Question of Life, the Universe and Everything")
    }
  }
}

object QuestionMessage extends AnyRef with Injection {
  def question = inject[String](new Key[String])
  def text = question + " is 42"
}
 
println(QuestionMessage.text)

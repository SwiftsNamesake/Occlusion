// My very first Scala script.
// http://docs.scala-lang.org/tutorials/scala-for-java-programmers.html

object Timer {

  def oncePerSecond(callback : () => Unit) {
    while (true) { callback(); Thread sleep 1000 }
  }

  def timeFlies() {
    println("Time drops like a cannon ball!")
  }

  def main(args : Array[String]) {
    oncePerSecond(timeFlies)
  }

}


class Complex(real : Double, imaginary : Double) {
  def re = real
  def im = imaginary
}

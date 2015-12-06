package testing

trait Prop {
  def check: Boolean

  // Exercise 8.3
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = {
      if (this.check) p.check else false
    }
  }
}

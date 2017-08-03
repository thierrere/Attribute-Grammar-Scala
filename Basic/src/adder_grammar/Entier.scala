package adder_grammar;

object Entier {

	trait Zx;
	trait Ex;
	trait Tx;

	case class Print(e: Ex) extends Zx

	case class Add (e: Ex, t: Tx) extends Ex
	case class Copy (t: Tx) extends Ex

	case class Take [n] (valeur:Int) extends Tx

	def evalTx(t: Tx) = t match {
		case Take(n) => n;
	}

	def evalEx(e: Ex): Int = e  match {
		case Copy(t) => evalTx(t)
		case Add(e,t) => evalEx(e)+evalTx(t)
	}

	def evalZx(z:Zx) = z match {
		case Print(e:Ex) => evalEx(e)
	}

def main(args: Array[String]): Unit = {
		val a=Take(10);
		println("a.valeur : "+ a.valeur);
		val b=Copy(Take(20));
		println("b.t : "+b.t);
		val c=Add (Copy(Take(10)), Take(10));
		println("c.e : "+c.e+" c.t : "+c.t);
		val d=Print(Add (Copy(Take(10)), Take(10)));
		println(d);
		
		println("evalZx(d)= "+evalZx(d));
}

}
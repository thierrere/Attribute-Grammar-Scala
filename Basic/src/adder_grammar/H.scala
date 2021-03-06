package adder_grammar;

object H {

  /*
   * data Z = Print E
   * data E = Add E T | Copy T
   * data T = Take Int
   */
  
	case class Print(e: E) extends Z

	case class Add (e: E, t: T) extends E
	case class Copy (t: T) extends E

	case class Take [n] (n:Int) extends T


	def evalT(t: T):(Int,Int) = t match {
		case Take(n) => t.v=n; t.p=t.p+1 ;return (t.v,t.p);
	}

	def evalE(e: E):(Int,Int) = e  match {
		case Copy(t) => t.p=t.p+1; e.v=evalT(t)._1; e.p=t.p; return (e.v,e.p);
		case Add(e1,t) => e.v= evalE(e1)._1+evalT(t)._1; e.p=e1.p+1 ; return return (e.v,e.p);
	}

	def evalZ(z:Z):(Int,Int) = z match {
		case Print(e:E) => z.v=evalE(e)._1; z.p=e.p; return (z.v,z.p);
	}
	
	def main(args: Array[String]): Unit = {		
		val a= Print(Add(Copy(Take(10)),Take(5)));
		val b = Add(Copy(Take(10)),Take(5));
		println("a= "+a+" evalZ(a) = "+evalZ(a));
		//println(b);
		println();
		val c= Print(Add(Add(Add(Add(Copy(Take(10)),Take(10)),Take(10)),Take(10)),Take(10)));
		println("c= "+c+" evalZ(c) = "+evalZ(c));		
	}
}
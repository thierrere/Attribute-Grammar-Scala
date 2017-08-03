package algebra;

object TestAlgEntier {

  trait Zf;
  trait Ef;
  trait Tf

  case class Print(e: Ef) extends Zf;
  case class Add(e: Ef, t: Tf) extends Ef;
  case class Copy(t: Tf) extends Ef;
  case class Take(n: Int) extends Tf;

  //Domaines Sémantiques

  type SemZ = Int
  type SemE = Int
  type SemT = Int

  //Fonctions pour les différents calculs

  /*printe:: SemE -> SemZ
printe semE = val where val1 = semE
                        val = val1*/

  def printe(e: SemE): SemZ = {
    return e;
  }

  /*add:: SemE -> SemT -> SemE
add semE semT = val where val1=semE
                          val2=semT
                          val=val1+val2*/

  def add(e: SemE)(t: SemT): SemE = {
    return e + t;
  }

  /*copy:: SemT -> SemE
copy semT = val where val1=semT
                      val=val1*/

  def copy(t: SemT): SemE = {
    return t;
  }

  /*takee:: Int -> SemT
takee n = val where val=n*/

  def take(n: Int): SemT = {
    return n;
  }

  //les fonctions d’interprétation de chacun des constructeurs

  /*Evaluation des attributs sans les algèbres
{-evalZ :: (b -> c) -> (b -> a -> b) -> (a -> b) -> Z -> c
evalZ p fa fc(Print e) = p (evalE fa fc)

evalE :: (b -> a -> b) -> (a -> b) -> (Int -> a) -> E -> b
evalE fa fc (Add e1 t1) = fa (evalE fa fc e1) (evalT fc t1)

evalT :: (Int -> a) -> T -> a
evalT fc (Take n) = fc (takee n)
-}*/

  //Ecriture avec les algèbres

  //Algèbre
  //data AlgZ a b c = AlgZ{fp :: b -> c, fa :: b -> a -> b, fc :: a -> b, ft :: Int -> a}
  trait AlgZ;
  case class fp(e: SemE) extends AlgZ;
  case class fa(e: SemE)(t: SemT) extends AlgZ;
  case class fc(t: SemT) extends AlgZ;
  case class ft(n: Int) extends AlgZ;

  //Shema de Calcul par reconnaissance de motif
  /*evalZ:: AlgZ a b c -> Z -> c
evalZ alg(Print e) = fp alg (evalE alg e)*/

  def evalZ(z: Zf): SemZ = z match {
    case Print(e) => if (e == null) { println("Z->Print E"); println("E->Add E T| Copy T"); println("Write E:"); val ee = waitGen1(); println("ee= " + ee); evalE(ee); }
    else { evalE(e) };
  }

  /*evalE:: AlgZ a b c -> E -> b
evalE alg (Add e t) = fa alg (evalE alg e) (evalT alg t)
evalE alg (Copy t) = fc alg (evalT alg t)*/

  def evalE(e: Ef): SemE = e match {
    case Add(e, t) => println("We are there!"); add(evalE(e))(evalT(t));
    case Copy(t) => copy(evalT(t));
  }

  /*evalT:: AlgZ a b c  -> T -> a
evalT alg (Take t) = ft alg t*/

  def evalT(t: Tf): SemT = t match {
    case Take(n) => take(n);
  }

  //Pour le calcul des attributs

  //val result= evalZ(AlgZ printe add copy takee)

  def waitGen(): Object = {
    val o: Object = readLine();
    println("Object O= " + o);
    return o;
  }

  def waitGen1(): Ef = {
    var t: Ef = null;
    val o = readLine();
    println("String O= " + o);
    //t=o.asInstanceOf[Ef];
    t = o match {
      case "Add(Copy(Take(10)) , (Take(20)))" => Add(Copy(Take(10)), (Take(20)));
    }

    return t;
  }

  def main(args: Array[String]): Unit = {
    val t = printe(add(copy(take(10)))(take(20)));
    val T = Print(Add(Copy(Take(10)), (Take(20))));

    println("t classs: " + t.getClass());
    // println("t= "+t+" eval(t)= "+evalZ(t));

    println("T Class: " + T.getClass() + " T= " + T + " eval(T)= " + evalZ(T));

    val t1 = Print(null);

    println("t1= " + t1 + " eval(t1)= " + evalZ(t1));

  }
}
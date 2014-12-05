import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class Lab5Spec extends FlatSpec {
  
//	"" should "type an assignment" in {
//	  assertResult(TNumber){
//	    typeInfer(Map(), Decl(MVar,"x", N(42), Assign(Var("x"),N(47))))
//	  }
//	  
//	  intercept[StaticTypeError] {
//	    typeInfer(Map(), Decl(MVar))
//	  }
//	}
  
  
  "DoNeg" should "negate values" in {
    val e = Unary(Neg, N(42))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assertResult(N(-42)) { ep }
  }

  "DoSeq" should "produce second element in sequence" in {
    val e = Binary(Seq, N(1), Binary(Plus, N(2), N(3)))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assertResult(Binary(Plus, N(2), N(3))) { ep }
  }
  "DoGetField" should "access a field from an object in memory" in {
    val setup = Obj(Map("a" -> N(42), "b" -> N(47)))
    val (m:Mem, addr: A) = step(setup)(Mem.empty)
    val e = GetField(addr, "b")
    val (mp, ep: Expr) = step(e)(m)
    assert(m == mp)
    assertResult(N(47)) { ep }
  }

  "DoVar" should "declare a variable" in {
    val e = Decl(MVar, "x", N(42), Var("x"))
    val (mp: Mem, ep: Expr) = step(e)(Mem.empty)
    assert(ep match {
      case Unary(Deref, a@A(_)) =>
        // Verify memory correctly references N(42)
        mp.get(a).get == N(42)
      case _ => false
    })
  }

  "DoObj" should "something" in {
    val obj = Obj(Map("a" -> N(22), "b" -> N(12), "c" -> B(true)))
    val (m:Mem, addr: A) = step(obj)(Mem.empty)
    println(m)
    assert(m != Mem.empty)
    assert(m(addr) == obj)
  }
  
  "DoConst" should ".." in {
    val const = Decl(MConst, "x", S("test"), Binary(Plus, Var("x"), S("right")))
    val (m:Mem, e:Expr) = step(const)(Mem.empty)
    val result = Binary(Plus, S("test"), S("right"))
    assert(e == result)
  }
  
  "DoDeref" should ".." in {
    val e = Decl(MVar, "x", N(42), Var("x"))
    val (mp: Mem, ep: Expr) = step(e)(Mem.empty)
    val ee = Unary(Deref, A(1))
    val (mp2: Mem, ep2: Expr) = step(ee)(mp)
    assert(mp2 == mp)
    assert(ep2 == N(42))
  }
  
  "DoAssignVar" should "" in {
    val e = Decl(MVar, "x", N(42),
        Assign(Var("x"), N(47)))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    
    val aref = ep match {
      case Assign (
        Unary(Deref, a @ A(_)), N(47)) => Some(a)
      case _ => None
    }
    val (mpp, epp: Expr) = step(ep)(mp)
    assertResult(N(47)){
      mpp get (aref get) get
    }
    assertResult(N(47)){ epp }
  }
  
  "SearchCall1" should "step its function" in {
    // (true ? function (n: number) { return n } : null)(42)
    val e = Call(If(B(true),Function(None,Left(List(("n",TNumber))),None,Var("n")),Null),List(N(42)))
    val (_, ep: Expr) = step(e)(Mem.empty)
    assertResult(
      Call(Function(None, Left(List(("n", TNumber))), None, Var("n")), List(N(42)))) {
      ep
    }
  }
  
  
  "mapFirstDoWith" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     def dowith[W]: DoWith[W,List[Int]] = mapFirstWith[W,Int] { (i: Int) => if (i < 0) Some(doreturn(-i)) else None } (l1)
     assertResult((true,gold1)) { dowith(true) }
     assertResult((42,gold1)) { dowith(42) }
  }

  // Probably want to write some tests for castOk, typeInfer, substitute, and step.
}

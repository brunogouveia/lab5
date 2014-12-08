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
  
  /** TypeCheck tests **/
  
  "TypeNeg" should "" in  {
    
  }
  
  "DoNeg" should "negate values" in {
    val e = Unary(Neg, N(42))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(N(-42)) { ep }
  }
  
  "DoNot" should "invert boolean values" in {
    val e1 = Unary(Not, B(false));
    val e2 = Unary(Not, B(true));
    val (mp1:Mem, ep1: Expr) = step(e1)(Mem.empty)
    val (mp2:Mem, ep2: Expr) = step(e2)(Mem.empty)
    assert(mp1.isEmpty)
    assert(mp2.isEmpty)
    assertResult(B(true)){ ep1 }
    assertResult(B(false)){ ep2 }
  }
 
  "DoSeq" should "produce second element in sequence" in {
    val e = Binary(Seq, N(1), Binary(Plus, N(2), N(3)))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(Binary(Plus, N(2), N(3))) { ep }
  }

  "DoArith" should "sum numbers" in {
    val e = Binary(Plus, N(2), N(3))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(N(5)) { ep }
  }
 
  "DoPlusString" should "concat strings" in {
    val e = Binary(Plus, S("abc"), S("xyz"))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(S("abcxyz")) { ep }
  }

  "DoInequalityNumber" should "compare according to specifications" in {
    val e1 = Binary(Gt, N(48), N(42))
    val e2 = Binary(Le, N(42), N(42))
    val (mp1:Mem, ep1: Expr) = step(e1)(Mem.empty)
    val (mp2:Mem, ep2: Expr) = step(e2)(Mem.empty)
    assert(mp1.isEmpty)
    assert(mp2.isEmpty)
    assertResult(B(true)){ ep1 }
    assertResult(B(true)){ ep2 }
  }
  
  "DoInequalityString" should "compare according to specifications" in {
    val e1 = Binary(Gt, S("ab"), S("bb"))
    val e2 = Binary(Le, S("12"), S("0"))
    val (mp1:Mem, ep1: Expr) = step(e1)(Mem.empty)
    val (mp2:Mem, ep2: Expr) = step(e2)(Mem.empty)
    assert(mp1.isEmpty)
    assert(mp2.isEmpty)
    assertResult(B(false)){ ep1 }
    assertResult(B(false)){ ep2 }
  }
  
  "DoEquality" should "compare according to specifications" in {
    val e1 = Binary(Eq, N(42), N(42))
    val e2 = Binary(Ne, S("12"), S("12"))
    val (mp1:Mem, ep1: Expr) = step(e1)(Mem.empty)
    val (mp2:Mem, ep2: Expr) = step(e2)(Mem.empty)
    assert(mp1.isEmpty)
    assert(mp2.isEmpty)
    assertResult(B(true)){ ep1 }
    assertResult(B(false)){ ep2 }
  }
  
  "DoAndTrue" should "not shortcircuiting boolean and" in {
    val e = Binary(And, B(true), Decl(MVar, "x", B(true), Var("x")))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(Decl(MVar, "x", B(true), Var("x"))) { ep }
  }
  
  "DoAndFalse" should "" in {
    val e = Binary(And, B(false), Decl(MVar, "x", B(true), Var("x")))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(B(false)) { ep }
  }
  
  "DoOrTrue" should "" in {
    val e = Binary(Or, B(true), Decl(MVar, "x", B(true), Var("x")))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(B(true)) { ep }
  }
  
  "DoOrFalse" should "" in {
    val e = Binary(Or, B(false), Decl(MVar, "x", B(true), Var("x")))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(Decl(MVar, "x", B(true), Var("x"))) { ep }
  }
  
  "DoIfTrue" should "" in {
    val e = If(B(true), Unary(Not, B(true)), B(false))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(Unary(Not, B(true))) { ep }
  }
  
  "DoIfFalse" should "" in {
    val e = If(B(false), Unary(Not, B(true)), Decl(MConst, "xy", B(false), Unary(Not, Var("xy"))))
    val (mp:Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(Decl(MConst, "xy", B(false), Unary(Not, Var("xy")))) { ep }
  }
  
  "DoObject" should "instantiate an object in memory" in {
    val e = Obj(Map("a" -> N(42), "b" -> N(47)))
    val (mp:Mem, a: A) = step(e)(Mem.empty)
    assertResult(e) { mp.get(a).get }
  }

  "DoGetField" should "access a field from an object in memory" in {
    val setup = Obj(Map("a" -> N(42), "b" -> N(47)))
    val (m:Mem, addr: A) = step(setup)(Mem.empty)
    val e = GetField(addr, "b")
    val (mp, ep: Expr) = step(e)(m)
    assert(m == mp)
    assertResult(N(47)) { ep }
  }
 
  "DoConst" should ".." in {
	val const = Decl(MConst, "x", S("test"), Binary(Plus, Var("x"), S("right")))
	val (m:Mem, e:Expr) = step(const)(Mem.empty)
	val result = Binary(Plus, S("test"), S("right"))
	assert(e == result)
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
  
  "DoDeref" should ".." in {
	val e = Decl(MVar, "x", N(42), Var("x"))
	val (mp: Mem, ep: Expr) = step(e)(Mem.empty)
	val ee = Unary(Deref, A(1))
	val (mp2: Mem, ep2: Expr) = step(ee)(mp)
	assert(mp2 == mp)
	assert(ep2 == N(42))
  }

  "DoAssignVar" should "assign the variable to a value" in {
	val e = Decl(MVar,"x",N(42),Assign(Var("x"),N(47)))
	val (mp: Mem, ep: Expr) = step(e)(Mem.empty)
	val aref = ep match {
	  case Assign(Unary(Deref, a@A(_)), N(47)) => Some(a)
	  case _ => None
	}
	val (mpp, epp: Expr) = step(ep)(mp)
	assertResult(N(47)) { mpp get(aref get) get}
	assertResult(N(47)) { epp }
  }
  
  "DoAssignField" should "access a field from an object in memory" in {
	val setup = Obj(Map("a" -> N(42), "b" -> N(47)))
	val (m:Mem, a: A) = step(setup)(Mem.empty)
	val e = Assign(GetField(a, "b"), N(99))
	val (mp, ep: Expr) = step(e)(m)
	assertResult(Obj(Map("a" -> N(42), "b" -> N(99)))) { mp get a get}
	assertResult(N(99)) { ep }
  }
  
  "DoCall" should "" in {
    val f = Function(None, Left(List(("x",TNumber), ("y",TNumber))), None, Binary(Plus, Var("x"), Var("y")))
    val c = Call(f, List(N(42), N(27)))
    val (m:Mem, ep) = step(c)(Mem.empty)
    assert(m.isEmpty)
    assertResult(Binary(Plus,N(42), N(27))){ ep }
  }
  
  "DoCallRec" should "" in {
    val f = Function(Some("fucx"), Left(List(("x",TNumber), ("y",TNumber))), None, Call(Var("fucx"),List(Var("x"), Var("y"))))
    val c = Call(f, List(N(42), N(27)))
    val (m:Mem, ep) = step(c)(Mem.empty)
    assert(m.isEmpty)
    assertResult(Call(Function(Some("fucx"), Left(List(("x",TNumber), ("y",TNumber))), None, Call(Var("fucx"),List(Var("x"), Var("y")))), List(N(42), N(27)))){ ep }
  }
  
  "DoCallName" should "" in {
    val f = Function(None, Right((PName, "x", TBool)), None, Binary(Eq, Var("x"), B(false)))
    val c = Call(f, List(Binary(Gt, N(42), N(11))))
    val (m:Mem, ep) = step(c)(Mem.empty)
    assert(m.isEmpty)
    assertResult(Binary(Eq, Binary(Gt, N(42), N(11)), B(false))){ ep }
  }
  
  "DoCallRecName" should "" in {
    val f = Function(Some("fucx"), Right((PName, "x", TBool)), Some(TBool), Binary(Eq, Var("x"), Call(Var("fucx"), List(B(true)))))
    val c = Call(f, List(Binary(Gt, N(42), N(11))))
    val (m:Mem, ep) = step(c)(Mem.empty)
    assert(m.isEmpty)
    assertResult(Binary(Eq, Binary(Gt, N(42), N(11)), Call(Function(Some("fucx"), Right((PName, "x", TBool)), Some(TBool), Binary(Eq, Var("x"), Call(Var("fucx"), List(B(true))))), List(B(true))))){ ep }
  }
  
  "DoCallVar" should "" in {
    val f = Function(None, Right((PVar, "x", TBool)), None, Binary(Eq, Var("x"), B(false)))
    val c = Call(f, List(B(true)))
    val (m:Mem, ep) = step(c)(Mem.empty)
    assert(m(A(1)) == B(true))
    assertResult(Binary(Eq, Unary(Deref,A(1)), B(false))){ ep }
  }
  
  "DoCallRecVar" should "" in {
    val f = Function(Some("fucx"), Right((PVar, "x", TBool)), Some(TBool), Binary(Eq, Var("x"), Call(Var("fucx"), List(B(false)))))
    val c = Call(f, List(B(true)))
    val (m:Mem, ep) = step(c)(Mem.empty)
    assert(m(A(1)) == B(true))
    assertResult(Binary(Eq, Unary(Deref,A(1)), Call(Function(Some("fucx"), Right((PVar, "x", TBool)), Some(TBool), Binary(Eq, Var("x"), Call(Var("fucx"), List(B(false))))), List(B(false))))){ ep }
  }

  "DoCast" should "" in {
	val e = Unary(Cast(TNumber), N(42))
	val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
	assert(mp.isEmpty)
	assertResult(N(42)){ ep }
  }
  
  "DoCastNull" should "" in {
    val e1 = Unary(Cast(TObj(Map("x" -> TNumber, "y" -> TBool))), Null)
    val e2 = Unary(Cast(TInterface("t2", TObj(Map("x" -> TNumber, "y" -> TBool)))), Null)
    val (mp1:Mem, ep1:Expr) = step(e1)(Mem.empty)
    val (mp2:Mem, ep2:Expr) = step(e2)(Mem.empty)
    assert(mp1.isEmpty)
    assert(mp2.isEmpty)
    assertResult(Null){ ep1 }
    assertResult(Null){ ep2 }
  }
  
  "DoCastObj" should "" in {
    val (m1:Mem, e1:Expr) = step(Decl(MVar, "x", Obj(Map("a" -> B(true), "b" -> N(102))), Unary(Cast(TObj(Map("a" -> TNumber, "b" -> TBool))), A(1)))){Mem.empty}
    val (m2:Mem, e2:Expr) = step(Decl(MVar, "x", Obj(Map("a" -> B(true), "b" -> N(102))), Unary(Cast(TInterface("t", TObj(Map("a" -> TNumber, "b" -> TBool)))), A(1)))){Mem.empty}
    
    val (mp1:Mem, ep1:Expr) = step(e1)(m1)
    val (mp11:Mem, ep11:Expr) = step(ep1)(mp1)
    val (mp2:Mem, ep2:Expr) = step(e2)(m2)
    val (mp22:Mem, ep22:Expr) = step(ep2)(mp2)
    assert(!mp11.isEmpty)
    assertResult(A(1)){ ep11 }
    assert(!mp22.isEmpty)
    assertResult(A(1)){ ep22 }
  }  
  
  "SearchDecl" should "step its declared value" in {
    val e = Decl(MConst,"x",Binary(Times,N(6),N(7)),Undefined)
    val (mp: Mem, ep: Expr) = step(e)(Mem.empty)
    assert(mp.isEmpty)
    assertResult(Decl(MConst,"x",N(42),Undefined)) { ep }
  }


  "SearchAssign1" should "step the lhs" in {
    //  { a: 42, b: 47 }.a = 99
    val e = Assign(GetField(Obj(Map("a" -> N(42.0), "b" -> N(47.0))),"a"),N(99.0))
    val (mp: Mem, ep: Expr) = step(e)(Mem.empty)

  }

  "SearchAssign2" should "step the rhs" in {
    val e = Decl(MVar,"x",N(42),Assign(Var("x"),N(47)))
    val (mp: Mem, ep: Expr) = step(e)(Mem.empty)
    assert(ep match {
      case Assign(Unary(Deref, a @ A(_)),N(47)) =>
        // Verify memory correctly references N(42)
        mp.get(a).get == N(42)
      case _ => false
    })
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

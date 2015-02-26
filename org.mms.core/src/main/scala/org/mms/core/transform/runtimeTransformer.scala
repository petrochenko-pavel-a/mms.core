package org.mms.core.transform
import org.mms.core._
import org.mms.core.Property
import org.mms.core.codemodel.SourceMember
import org.mms.core.codemodel.IType
import org.mms.core.Type
import org.mms.core.runtime.RuntimeProperty
import org.mms.core.runtime.IRuntimeProperty
import org.mms.core.runtime.ICollectionProperty
import org.mms.core.runtime.ICollectionProperty
import org.mms.core.codemodel.SourceMember
import org.mms.core.codemodel.SourceMember
import org.mms.core.runtime.RuntimeImplicits._
import java.util.IdentityHashMap
import java.util.HashMap
trait Tranformation[F, T] extends Function2[F, T, Unit] {

}

trait TranformationFunction[F, T] extends Function1[F, T] {}

trait RegisterableTransformer[F, T] extends TranformationFunction[F, T] {
  def supports(): Tuple2[Class[_], Class[_]];
}

class TransformerRegistry {
  type ClassPair = Tuple2[Class[_], Class[_]];

  var tMap: Map[ClassPair, Function1[_, _]] = Map().withDefaultValue(null);

  def register(c: RegisterableTransformer[_, _]) {
    val kv = (c.supports(), c);
    tMap = tMap + kv;
  }

  def transformer(f: Class[_], t: Class[_]): Function1[_, _] = {
    val p: ClassPair = (f, t);
    var r = tMap(p);
    if (r == null) {
      val s = f.getSuperclass;
      if (s != null && f != classOf[Object]) {
        r = transformer(s, t);
      }
      if (r != null) {
        return r;
      }
      for (i <- f.getInterfaces) {
        r = transformer(i, t);
        if (r != null) {
          return r;
        }
      }
    }
    return r;
  }
}
 case class CalculatedTransform[A, B](a: Class[A], b: Class[B], tr: Tranformation[A, B]) extends RegisterableTransformer[A, B] {

    if (b.isAbstract()){
      throw new IllegalStateException("Unable to create abstract classes");
    }
   
    def apply(v1: A): B = {
      val ctx=CalculatedTransform.enter();
      try{
      val alreadyTransformed=ctx.get(v1,b);
      if (alreadyTransformed!=null){
        return b.cast(alreadyTransformed);
      }
      val r = b.newInstance();
      ctx.record(v1,r,b);
      tr.apply(a.cast(v1), r);
      return r;
      }finally{
        CalculatedTransform.exit();
      }
    }

    def supports(): (Class[_], Class[_]) = (a, b);
  }
 //TODO better equals
 case class ObjectTargetClass(identityCode:Int,targetClass:Class[_],v:Any);

 class TransformContext{
   val transformedMap=new HashMap[Any,Any]();
   var level:Int=0;
   
   def record(s:Any,t:Any,tc:Class[_])={
      transformedMap.put(ObjectTargetClass(System.identityHashCode(s),tc,s), t);
   }
   
   def get(s:Any,tr:Class[_]):Any=transformedMap.get(ObjectTargetClass(System.identityHashCode(s),tr,s));
 }
 
 object CalculatedTransform{
     val context=new ThreadLocal[TransformContext]();
     
     
     
     private def enter():TransformContext={
       var c=context.get();
       if (c==null){
         c=new TransformContext();
         context.set(c);
       }
       c.level=c.level+1;
       return c;
     }
     private def exit(){
       var c=context.get();
       c.level=c.level-1;
       if (c.level==0){
         context.set(null);
       }
     }
 }
 
object Transformers {

  val registry = new TransformerRegistry

  def transform[F, T <: Any](v: F, rt: Class[T]): T = {
    if (rt.isInstance(v)) {
      return rt.cast(v);
    }
    val t = registry.transformer(v.getClass(), rt);
    //

    if (t == null) {
      val bt: RegisterableTransformer[F, T] = buildTransform(v.getClass().asInstanceOf[Class[F]], rt);
      if (bt != null) {
        registry.register(bt);
        return bt(v);
      }
      throw new IllegalArgumentException(s"Can not transform ${v} to ${rt}")
    }
    val f: Function1[F, T] = t.asInstanceOf[Function1[F, T]];

    return f(v);
  }
  
  def transformer[F, T <: Any](ft: Class[F], rt: Class[T]): TranformationFunction[F,T] = {
    
    val t = registry.transformer(ft, rt);
    //

    if (t == null) {
      val bt: RegisterableTransformer[F, T] = buildTransform(ft, rt);
      if (bt != null) {
        registry.register(bt);
        return bt;
      }
      throw new IllegalArgumentException(s"Can not transform ${ft} to ${rt}")
    }
    val f: TranformationFunction[F, T] = t.asInstanceOf[TranformationFunction[F, T]];
    return f;
  }
 

  def buildTransform[A, B](v: Class[A], rt: Class[B]): RegisterableTransformer[A, B] = {
    val bl = TransformationModelRegistry.transformer(v, rt);
    if (bl != null) {
      val x:TranformationFunction[A,B]=bl.toTransform(v,rt).asInstanceOf[TranformationFunction[A,B]];
    
      return new RegisterableTransformer[A,B](){
        def supports(): Tuple2[Class[_], Class[_]]=(v,rt);
        
        def apply(a:A):B={
          return x.apply(a);
        }
      }
    }
    return null;
  }

  def create[T <: Any](rt: Class[T]): T = {
    return rt.newInstance();
  }
}
case class TransformationList[D, D1](val seq: Tranformation[D, D1]*) extends Tranformation[D, D1] {
  {
    if(seq.contains(null)){
      throw new IllegalArgumentException;
    }
    
  }
  def apply(v1: D, v2: D1): Unit = {
    for (i <- seq) {
      i(v1, v2);
    }
  }
}

class IdenticalTransform[D, D1, R](val sP: IRuntimeProperty[D, R], tP: IRuntimeProperty[D1, R]) extends Tranformation[D, D1] {
  def apply(v1: D, v2: D1): Unit = {
    val ir = sP.get(v1);
    tP.asInstanceOf[IRuntimeProperty[Any, Any]].set(v2, ir);
  }
}

case class OneToOnePropertyTransform[D, D1, SR, TR](val sP: IRuntimeProperty[D, SR], val tP: IRuntimeProperty[D1, TR], val tF: Function1[_ >: SR, TR]) extends Tranformation[D, D1] {
  def apply(v1: D, v2: D1): Unit = {
    val ir: SR = sP.get(v1);
    val tr: TR = tF(ir);
    tP.set(v2, tr);
  }
}

case class ObjectInitTransform[D, D1, SR, TR](val tP: IRuntimeProperty[D, SR],initFunction:Tranformation[D,TR]) extends Tranformation[D, D1] {
  def apply(v1: D, v2: D1): Unit = {
    val to=tP.range().newInstance();
    initFunction.apply(v1,to.asInstanceOf[TR]);
    tP.set(v2.asInstanceOf[D],to);
    
  }
}
case class RuntimeCondition(){
  def satisfy(v:Any):Boolean=true;
  
}

case class ConditionedAssertion[F,T](val condition:RuntimeCondition, val transformer:TranformationFunction[F,T]){
  {
    if (condition==null||transformer==null){
      throw new IllegalStateException("Should never be null")
    }    
  }
  
  def transform(value:Any):T={
    if (condition.satisfy(value)){
      return transformer.apply(value.asInstanceOf[F]);
    }
    return null.asInstanceOf[T];
  }
}
case class RuntimeDescriminator[F,T](val slass:Class[_],val assertions:ConditionedAssertion[F,T]*){
  def transform(value:Any):Any={
    if (slass.isInstance(value)){
      for (a<-assertions){
        val t=a.transform(value);
        if (t!=null){
          return t;
        }
      }
    }
    return null;
  }
}
case class DescriminatedTransform[F,T](val targetClass:Class[_],val descriminators:RuntimeDescriminator[F,T]*) extends TranformationFunction[F,T]{
  
  def apply(v:F):T={
    if (v==null){
      return null.asInstanceOf[T];
    }
    if (targetClass.isInstance(v)){
      return targetClass.cast(v).asInstanceOf[T];
    }
    for (d<-descriminators){
      val z=d.transform(v);
      if (z!=null){
        return targetClass.cast(z).asInstanceOf[T];
      }
    }
    return null.asInstanceOf[T];
  }
}

object OneToOnePropertyTransform {
  def createWithTransform[D, D1, SR, TR](sP: IRuntimeProperty[D, SR], tP: IRuntimeProperty[D1, TR], transFunc: TranformationFunction[_, _]=null): Tranformation[D, D1] = {
    
    val x=transFunc.asInstanceOf[TranformationFunction[SR,TR]];
    return new OneToOnePropertyTransform[D, D1, SR, TR](sP, tP.asInstanceOf[IRuntimeProperty[D1, TR]],x ).asInstanceOf[Tranformation[D, D1]];    
  }
  
  def apply[D, D1, SR, TR](sP: IRuntimeProperty[D, SR], tP: IRuntimeProperty[D1, TR]): Tranformation[D, D1] = {
    
    val d1c = sP.range();
    val d2c = tP.range();    
    if (d1c == d2c) {
      return new IdenticalTransform[D, D1, SR](sP, tP.asInstanceOf[IRuntimeProperty[D1, SR]]).asInstanceOf[Tranformation[D, D1]];
    } else {
      return new OneToOnePropertyTransform[D, D1, SR, TR](sP, tP.asInstanceOf[IRuntimeProperty[D1, TR]], transformFunction(d2c)).asInstanceOf[Tranformation[D, D1]];
    }
  }
}

case class identityFunction[R]() extends TranformationFunction[R, R] {
  def apply(v1: R) = v1;
}

case class composeFunction[R, A,R1](val f:TranformationFunction[R, A],s:TranformationFunction[A, R1]) extends TranformationFunction[R, R1] {
  def apply(v1: R) = s(f(v1));
}
case class transformFunction[R, R1](cl: Class[R1]) extends TranformationFunction[R, R1] {
  def apply(v1: R) = Transformers.transform(v1, cl);
}

case class listTransformFunction[SC, TC, R <: Traversable[SC]](val tf: Function1[SC, TC]) extends TranformationFunction[R, List[TC]] {
  def apply(v1: R): List[TC] = {
    var vr: List[TC] = List();
    for (e <- v1) {
      vr = vr :+ tf(e);
    }
    return vr;
  };
}

case class setTransformFunction[SC, TC, R <: Traversable[SC]](val tf: Function1[SC, TC]) extends TranformationFunction[R, Set[TC]] {
  def apply(v1: R): Set[TC] = {
    var r: Set[TC] = Set();
    for (e <- v1) {
      r = r + tf(e);
    }
    return r;
  };
}
case class mapTransformFunction[K1, V1, K2, V2](val kt: Function[K1, K2], val vt: Function[V1, V2])
  extends TranformationFunction[Map[K1, V1], Map[K2, V2]] {
  def apply(v1: Map[K1, V1]): Map[K2, V2] = {
    var r: Map[K2, V2] = Map();
    for (e: Tuple2[K1, V1] <- v1) {
      r = r + Tuple2[K2, V2](kt(e._1), vt(e._2));
    }
    return r;
  };
}

object PropertyToMember extends RegisterableTransformer[PropertyModel, SourceMember] {

  val nameProp = RuntimeProperty(classOf[PropertyModel], "name");
  val rangeProp = RuntimeProperty(classOf[PropertyModel], "range");
  val nameMember = RuntimeProperty(classOf[SourceMember], "name");
  val typeMember = RuntimeProperty(classOf[SourceMember], "elementsType");

  def apply(v1: PropertyModel): SourceMember = {
    val r = new SourceMember;
    TransformationList(
      OneToOnePropertyTransform(nameProp, nameMember),
      OneToOnePropertyTransform(rangeProp, typeMember))
      .apply(v1, r);
    return r;
  }

  def supports(): (Class[_], Class[_]) = (classOf[PropertyModel], classOf[SourceMember]);
}

object Tst extends App {
  val hello = "AA";
  val q = RuntimeProperty(Tst.getClass, "hello");
  Transformers.registry.register(PropertyToMember);
  val z = Transformers.transform(q.meta, classOf[SourceMember]);
  println(z);
}
package org.mms.core
import collection.mutable.{ HashMap, MultiMap, Set }
import scala.collection.mutable.WeakHashMap

class KnowledgeContext(val superContexts:KnowledgeContext*){
  private var annotations = new HashMap[Entity[_], Set[FactAnnotation]] with MultiMap[Entity[_], FactAnnotation];
  
  protected[core] def registerElement(e: Entity[_], f: FactAnnotation) {
    annotations.addBinding(e, f);
    //TODO CHECK OneValue
  }
  protected[core] def unregisterElement(e: Entity[_], f: FactAnnotation) {
    annotations.removeBinding(e, f);
    //TODO CHECK OneValue
  }
  
  def about[T<:FactAnnotation](e:Entity[_],t:Class[T]):scala.collection.immutable.Set[T]={
    var s=directStatementsAboutThis(e, t);
    for (c<-superContexts){
      s=s++c.about(e, t);
    }
    return s;
  }
  
  def directStatementsAboutThis[T<:FactAnnotation](e:Entity[_],t:Class[T]):scala.collection.immutable.Set[T]={
    var v=annotations.get(e.getTrueVersionOfThis());
    if (v.isDefined){
      return v.get.filter { x => t.isInstance(x) }.asInstanceOf[Set[T]].toSet;
    }
    return scala.collection.immutable.Set();
  }
}

object Entity {
  
  val globalContext=new KnowledgeContext();
  val curContext:ThreadLocal[KnowledgeContext]=new ThreadLocal;
  
  protected val cMap:WeakHashMap[Any,KnowledgeContext]=new WeakHashMap
  
  def withContext(cKey:Any)(body: => Unit):Unit={
    val pC=cMap.get(cKey);
    var q:KnowledgeContext=null;
    if (!pC.isDefined){
      q=new KnowledgeContext(globalContext);
      cMap.put(cKey, q);
    }
    else{
      q=pC.get;
      if (q==null){//TODO IS THIS POSSIBLE???
        q=new KnowledgeContext(globalContext);
        cMap.put(cKey, q);
      }
    }
    val pc=curContext.get;
    curContext.set(q);
    try{
      body;
    }
    finally{
      curContext.set(pc);
    }
  }
  
  protected[core] def cContext:KnowledgeContext={
    val c=curContext.get();
    if(c==null){
      return globalContext;
    }
    return c;
  }
  
  protected def registerElement(e: Entity[_], f: FactAnnotation) {
    cContext.registerElement(e, f);
  }
  
  protected def unregisterElement(e: Entity[_], f: FactAnnotation) {
    cContext.unregisterElement(e, f);
    //TODO CHECK OneValue
  }
  
  
}
trait FactAnnotation {
  
  
}

trait OneValueFact extends FactAnnotation{
  
}

case class Description(val value:String) extends FactAnnotation{
  
}

trait Entity[T <: Entity[T]] {

  var annotations=List[Tuple2[Entity[_],FactAnnotation]]();
  
  protected def register[X<:Entity[X]](c:Entity[X],f:FactAnnotation){
    Entity.registerElement(c.getTrueVersionOfThis(), f);
    val tp:Tuple2[Entity[X],FactAnnotation]=(c,f);
    annotations=annotations.::(tp);
  }
  protected[core] def getTrueVersionOfThis():Entity[_]=this;
  
  protected def remove(c:Entity[T]){
     for( a<-c.annotations){
       Entity.unregisterElement(a._1, a._2);       
     }     
  }
  
  def fact[T>:Null<:OneValueFact](t:Class[T]):T={
    val z:scala.collection.immutable.Set[T]=about(t);
    if (z.size==1){
      return z.toList(0);
    }
    return null;
  }
  def about[T<:FactAnnotation](t:Class[T]):scala.collection.immutable.Set[T]={
    return directStatementsAboutThis(t);
  }
  final def directStatementsAboutThis[T<:FactAnnotation](t:Class[T]):scala.collection.immutable.Set[T]={
    return Entity.cContext.about(this,t);
  }
  
  def desribe(description: String):Entity[T]= {
    register(this,Description(description));
    return this;
  }
}
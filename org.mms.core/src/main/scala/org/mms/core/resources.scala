package org.mms.core
import collection.mutable.{ HashMap, MultiMap, Set }

object Entity {
  private var annotations = new HashMap[Entity[_], Set[FactAnnotation]] with MultiMap[Entity[_], FactAnnotation];

  protected def register(e: Entity[_], f: FactAnnotation) {
    
    annotations.addBinding(e, f);
    //TODO CHECK OneValue
  }
  
  def about[T<:FactAnnotation](e:Entity[_],t:Class[T]):scala.collection.immutable.Set[T]={
    return e.about(t);
  }
}
trait FactAnnotation {
  
}
trait OneValueFact extends FactAnnotation{
  
}


case class Description(val value:String) extends FactAnnotation;

trait Entity[T <: Entity[T]] {

  protected def register(c:Entity[_],f:FactAnnotation){
    Entity.register(c, f);
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
  def directStatementsAboutThis[T<:FactAnnotation](t:Class[T]):scala.collection.immutable.Set[T]={
    var v=Entity.annotations.get(this);
    if (v.isDefined){
      return v.get.filter { x => t.isInstance(x) }.asInstanceOf[Set[T]].toSet;
    }
    return scala.collection.immutable.Set();
  }
  
  def desribe(description: String):Entity[T]= {
    register(this,Description(description));
    return this;
  }
}
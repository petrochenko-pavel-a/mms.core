package org.mms.core

import scala.collection.mutable.HashMap
import java.lang.reflect.Field
import org.mms.core.runtime.IRuntimeProperty



case class isPropertyOf[T<:Type](val t:T,p:PropertyModel)extends FactAnnotation;
case class isRangeOf[T<:Type](val t:T,p:PropertyModel)extends FactAnnotation;

/**
 * @author kor
 */
trait Property[DomainType<:Type,RangeType<:Type] extends Entity[Property[DomainType,RangeType]]{
   def range():RangeType
   def domain():DomainType
   def name():String;
   
   def withDefault(defaultValue:Any):Property[DomainType,RangeType]=this;
   
   def withName(name:String):Property[DomainType,RangeType];
   
   protected def init(){
     register(domain,isPropertyOf(domain,this));
     register(range,isRangeOf(range,this));
     //we need unregister3
   }
   init();
   
   override def toString():String={
     domain()+"."+name+typeDescr();
   }
   def typeDescr()=":"+range();

   def <=>[A<:Type,B<:Type](p:Property[_,_]):PropertyAssertion=TransformsOneToOne(this,p.asInstanceOf[Property[A,B]]);
  
   
}
trait PropertyAssertion extends FactAnnotation with Entity[PropertyAssertion] with Function1[Seq[PropertyAssertion],PropertyAssertion]{
  def apply(s:Seq[PropertyAssertion]):PropertyAssertion={
    return this;
  }
}

trait AssertionContainer {
  
  def typeMappings();Unit;
  def definitions():Unit;
  
  def learn(){
    typeMappings();
    definitions();
  };
}
trait Predicate{
  

 
}
object Predicate{
  object TRUE extends Predicate{}
  object FALSE extends Predicate{}
}

private abstract class ObjectDefinedProp[D<:ModelType[_],R<:Type](val domain:D,private var nameOverride:String=null)extends Property[D,R]{
  
  def withName(name:String):Property[D,R]={
    nameOverride=name;return this;
  }
  
  def field():Field={
    val z:HashMap[ObjectDefinedProp[D,R],Field]=domain.metainf.pToFieldMap.asInstanceOf[HashMap[ObjectDefinedProp[D,R],Field]];
    val x=z.get(this);
    if (!x.isDefined){
      return null;
    }
    return x.get;
  }
  def name():String={
     if (nameOverride!=null){
       return nameOverride;
     }
     val f=field;
     if (f==null){
       return "<unbinded prop>";
     }
     return field.getName;
  }
}


private[core] class Prop[D<:ModelType[_],R<:Type](override val domain:D,val range:R) extends ObjectDefinedProp[D,R](domain) ;

private[core] abstract class DelegateProp[D<:ModelType[_],R<:Type](val p:Property[D,R])extends ObjectDefinedProp[D,R](p.domain()){
  remove(p);
  def range():R=p.range;
}

private[core] case class ListProp[D<:ModelType[_],R<:Type](override val p:Property[D,R])extends DelegateProp[D,R](p){
  override def typeDescr()=":list("+range()+")";
  
}
private[core] case class KeyProp[D<:ModelType[_],R<:Type](override val p:Property[D,R])extends DelegateProp[D,R](p){}
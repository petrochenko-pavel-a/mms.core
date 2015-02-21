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
   def init(){
     register(domain,isPropertyOf(domain,this));
     register(range,isRangeOf(range,this));
   }
   init();
   
   override def toString():String={
     domain()+"."+name+":"+range();
   }

   def <=>[A<:Type,B<:Type](p:Property[_,_]):PropertyAssertion=TransformsOneToOne(this,p.asInstanceOf[Property[A,B]]);
   
}
trait PropertyAssertion extends FactAnnotation with Entity[PropertyAssertion]

trait AssertionContainer {
  
  def learn(){};
}


private[core] class Prop[D<:ModelType[_],R<:Type](val domain:D,val range:R)extends Property[D,R]{

  def name():String={
     //FIXME
     val z:HashMap[Prop[D,R],Field]=domain.metainf.pToFieldMap.asInstanceOf[HashMap[Prop[D,R],Field]];
     return z.get(this).get.getName;
  }
  
}
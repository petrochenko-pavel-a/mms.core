package org.mms.core;

import org.mms.core.runtime.ModifyInterceptor
import org.mms.core.runtime.IRuntimeProperty
import org.mms.core.transform.CalculatedTransform
import org.mms.core.runtime.KeyUtils

/**
 * marker interface for two way transformation
 */
trait TwoWayAssertion extends PropertyAssertion;

case class TransformsOneToOne[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends TwoWayAssertion {
  register(sp, this);
  register(tp, this);
}
trait NeedsDeconstruct extends FactAnnotation

case class ParentChildAssertion[P <: Type, C <: Type](val parent: Property[_<:Type, C], val child: Property[C, P]) 
  extends TwoWayAssertion with KnowsWayToModify with NeedsDeconstruct{
  register(parent, new ModifyInterceptor(parent){
     override def beforeModify(pr:IRuntimeProperty[_,_],base:Any,value:Any):Any={
       if (value==null){
         return value;
       }
       val ctx=CalculatedTransform.getContext();
       val child=pr.meta().range();
       val key=KeyUtils.globalKey(value,child ,parent);
       return ctx.exchangeKey(key,value);
     }
    
  })
  register(parent, this);
  register(child, this);
  /*register(parent.rootProperty().withDecorators(), this);
  register(child.rootProperty().withDecorators(), this);*/  
  def howToChange(prop:PropertyModel):Seq[WayToChange]={
    if (prop==parent){
      return List(
          WayToChange(
               Remove(child,OldValue,ThisValue),
               Add(child,NewValue,ThisValue)
          ));
    }
    if (prop==child){
      //this is me
    }
    return null;
  }
}

case class LessThen[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends PropertyAssertion {
  register(sp, this);
  register(tp, this);
}
case class MoreThen[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends PropertyAssertion {
  register(sp, this);
  register(tp, this);
}
case class LessOrEqual[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends PropertyAssertion {
  register(sp, this);
  register(tp, this);
}
case class MoreOrEqual[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends PropertyAssertion {
  register(sp, this);
  register(tp, this);
}
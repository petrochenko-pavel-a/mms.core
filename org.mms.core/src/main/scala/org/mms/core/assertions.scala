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

case class ParentChildAssertion[P <: Type, C <: Type](val sp: Property[_<:Type, C], val tp: Property[C, P]) 
  extends TwoWayAssertion with KnowsWayToModify with NeedsDeconstruct{
  register(sp, new ModifyInterceptor(sp){
     override def beforeModify(pr:IRuntimeProperty[_,_],base:Any,value:Any):Any={
       if (value==null){
         return value;
       }
       val ctx=CalculatedTransform.getContext();
       val tp=pr.meta().range();
       val key=KeyUtils.globalKey(value,tp ,sp);
       return ctx.exchangeKey(key,value);
     }
    
  })
  register(sp, this);
  register(tp, this);  
  def howToChange(prop:PropertyModel):Seq[WayToChange]={
    if (prop==sp){
      return List(
          WayToChange(
               Remove(tp,OldValue,ThisValue),
               Add(tp,NewValue,ThisValue)
          ));
    }
    if (prop==tp){
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
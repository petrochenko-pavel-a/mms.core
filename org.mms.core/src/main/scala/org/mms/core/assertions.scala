package org.mms.core;

import org.mms.core.runtime.ModifyInterceptor
import org.mms.core.runtime.IRuntimeProperty
import org.mms.core.transform.CalculatedTransform
import org.mms.core.transform.CanProduceTransformation
import org.mms.core.runtime.KeyUtils
import org.mms.core.transform.Tranformation
import org.mms.core.transform.SetValueTransform
import org.mms.core.runtime.RuntimeImplicits

/**
 * marker interface for two way transformation
 */
trait TwoWayAssertion extends PropertyAssertion;

case class TransformsOneToOne[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends TwoWayAssertion {
  register(sp, this);
  register(tp, this);
  
  override def toString()=sp+"<=>"+tp;
}
trait NeedsDeconstruct extends FactAnnotation

case class EqToConstantAssertion(base:PropertyModel,v:Any) extends PropertyAssertion with CanProduceTransformation{
   def toTransformation(): Tranformation[_, _]={
     return new SetValueTransform(RuntimeImplicits.propToRuntime(base).asInstanceOf[IRuntimeProperty[Any,Any]],v,RuntimeImplicits.typeToRuntime(tp));
     
   }
   
   var tp:Type=_;
   
   def setType(t:Type){
     tp=t;
   }

  def targetProps(): Seq[PropertyModel]=List(base)
}


case class ConstantDescriminator(base:Type,assertion:EqToConstantAssertion) extends PropertyAssertion{
  register(base,this);
  assertion.setType(base);
}

case class PretransformAssertion(base:Type,property:PropertyModel) extends FactAnnotation with OneValueFact{
  
}

case class CollapseParentInterceptor[ C <: Type](val parent: Property[_<:Type, C])extends ModifyInterceptor(parent){
 
  override def beforeModify(pr:IRuntimeProperty[_,_],base:Any,value:Any):Any={
       if (value==null){
         return value;
       }
       val ctx=CalculatedTransform.getContext();
       val child=pr.meta().range();
       val key=KeyUtils.globalKey(value,child ,parent);
       return ctx.exchangeKey(key,value);
     }
}

case class ParentChildAssertion[P <: Type, C <: Type](val parent: Property[_<:Type, C], val child: Property[C, P]) 
  extends TwoWayAssertion with KnowsWayToModify with NeedsDeconstruct{
  register(parent, new CollapseParentInterceptor(parent))
  
  override def hashCode():Int={
    return parent.hashCode()+child.hashCode();
  }
  override def equals(o:Any):Boolean={
    if (o.getClass!=this.getClass){
      return false;
    }
    val another=o.asInstanceOf[ParentChildAssertion[P,C]];
    if (this.parent==another.parent){
      if (this.child==another.child){
        return true;
      }
    }
    return false;
  }
  
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
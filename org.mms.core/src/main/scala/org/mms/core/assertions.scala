package org.mms.core;

/**
 * marker interface for two way transformation
 */
trait TwoWayAssertion extends PropertyAssertion;

case class TransformsOneToOne[D1 <: Type, D2 <: Type, R1 <: Type, R2 <: Type](val sp: Property[D1, R1], val tp: Property[D2, R2])
  extends TwoWayAssertion {
  register(sp, this);
  register(tp, this);
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
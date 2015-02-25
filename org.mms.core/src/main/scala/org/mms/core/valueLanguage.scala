package org.mms.core;

trait KnowsWayToModify extends FactAnnotation{
  def howToChange(prop:PropertyModel):Seq[WayToChange]
}

trait ValueModel{}

sealed trait PropertyModification{}

case object NewValue extends ValueModel
case object OldValue extends ValueModel
case class OldValue(pr:PropertyModel) extends ValueModel
case class NewValue(pr:PropertyModel) extends ValueModel

case class Add[T<:Type](pr:PropertyModel,v:ValueModel) extends PropertyModification
case class Remove[T<:Type](pr:PropertyModel,v:ValueModel) extends PropertyModification
case class SetValue[T<:Type](pr:PropertyModel,v:ValueModel)extends PropertyModification

case class WayToChange(ops:PropertyModification*);

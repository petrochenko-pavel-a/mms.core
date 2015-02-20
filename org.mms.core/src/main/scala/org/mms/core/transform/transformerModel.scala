package org.mms.core.transform;

import org.mms.core.Type
import org.mms.core.Property
import org.mms.core.ModelType
import org.mms.core.codemodel.SourceMember
import org.mms.core.BuiltInType
import org.mms.core.PropertyAssertion
import org.mms.core.Entity
import org.mms.core.FactAnnotation
import org.mms.core.isPropertyOf
import scala.collection.mutable.Set;
case class TransformationModel(from: Type, to: Type) {

  def apply() = {
    val sourceProp:Set[isPropertyOf[_]]=Entity.about(from,classOf[isPropertyOf[_]]);
    val targetProp:Set[isPropertyOf[_]]=Entity.about(from,classOf[isPropertyOf[_]]);
  }
}



object PropertyModelModel extends ModelType {
  val name = str;
  val range = propOf(classOf[Type]);
}
object SourceMemberModel extends ModelType {  
  var name = str;
  val elementsType = propOf(classOf[SourceMember])
}
object ts {
  PropertyModelModel.name <=> SourceMemberModel.name;
  PropertyModelModel.range <=> SourceMemberModel.elementsType;    
}

package org.mms.core.transform
import org.mms.core._
import org.mms.core.Property
import org.mms.core.codemodel.SourceMember
import org.mms.core.codemodel.IType
import org.mms.core.Type
import org.mms.core.runtime.RuntimeProperty
import org.mms.core.runtime.IRuntimeProperty
import org.mms.core.runtime.ICollectionProperty
import org.mms.core.runtime.ICollectionProperty
import org.mms.core.codemodel.SourceMember
import org.mms.core.codemodel.SourceMember
import org.mms.core.codemodel.SourceType

/**
 * code model related models
 */
object ITypeModel extends ModelType

object SourceTypeModel extends ModelType(ITypeModel){
  val name=str;
  val children=propOf(SourceMemberModel); 
}
object SourceMemberModel extends ModelType {  
  val name = str;
  val elementsType = propOf(classOf[IType])
}

/**
 * meta model relateed models
 */
object TypeModel extends ModelType{
  val typeNameProp=str;
  val superTypeProp=propOf(TypeModel)
}
object ModelTypeModel extends ModelType(TypeModel){
  val packageNameProp=str;
  val propertiesProp=list(str)
}
object PropertyModelModel extends ModelType {
  val name = str;
  val range = propOf(classOf[Type]);
}

//this type is a member of both models
object BuiltInTypeModel extends ModelType(null,withTrait(ITypeModel,TypeModel))

//Knowledge data should not be global!!! //FIXME
object Mappings extends AssertionContainer {
  //first init mappings to classes;
  ITypeModel<=>classOf[IType];
  SourceTypeModel<=>classOf[SourceType];
  SourceMemberModel<=>classOf[SourceMember];//We should be able to build transform proto without mapping
  TypeModel<=>classOf[Type]
  PropertyModelModel<=>classOf[Prop[_,_]]//We should check compatibility when stating it
  ModelTypeModel<=>classOf[ModelType[_]];
  
  ModelTypeModel<=>SourceTypeModel;
  
  println(BuiltInTypeModel.properties());
  //no we should write how Type instances related to types
  PropertyModelModel.name <=> SourceMemberModel.name;
  PropertyModelModel.range <=> SourceMemberModel.elementsType;    
}

object TestApp extends App{
  Mappings.learn();
  val v:SourceMember=Transformers.transform(SourceMemberModel.name,classOf[SourceMember]);
  println(v);
}
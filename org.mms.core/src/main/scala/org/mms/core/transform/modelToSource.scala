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
import org.mms.core.codemodel.SourceType
import org.mms.core.codemodel.IModelElement
import org.mms.core.codemodel.IMember
import org.mms.core.codemodel.IType
import javax.lang.model.element.PackageElement
import org.mms.core.codemodel.Package
import org.mms.core.ParentChildAssertion
import org.mms.core.codemodel.CodeModel

/**
 * code model related models
 */
object ITypeModel extends AbstractType {

}

object CodeModelModel extends ModelType {
  val name = key(str);
  val children = list(propOf(PackageElementModel));
}

object PackageElementModel extends ModelType() {
  val name = key(str);
  val parent = required(propOf(CodeModelModel));
  val children = list(propOf(SourceTypeModel));
  ParentChildAssertion(parent,parent.$.children);//more convinient way is to mark prop with parent
}

object SourceTypeModel extends ModelType(ITypeModel) {
  val name = key(str);
  val superClass = propOf(classOf[IType]);
  val children = list(propOf(SourceMemberModel));
  
  val parent = required(propOf(PackageElementModel))
  
  ParentChildAssertion(parent,parent.$.children);//more convinient way is to mark prop with parent
  
  //listof
}
/**
 * when constructing target property value
 * and initializing tree parent check if it can be replaced with existing one
 *   parent can be be replaced with existing one if its global key equals with current one global key
 * 
 *
 */



object SourceMemberModel extends ModelType {
  val name = str;
  val elementsType = propOf(classOf[IType])
}

/**
 * meta model relateed models
 */
object TypeModel extends AbstractType {
  val typeNameProp = str withName ("typeName");
  val superTypeProp = propOf(TypeModel) withName ("superType")

}
object ModelTypeModel extends ModelType(TypeModel) {
  val props = list(propOf(PropertyModelModel)).withName("declaredProperties");
  val modelPackage = str.withName("packageName");
}
object Universe extends ModelType{
  val children = listOf(ModelTypeModel);
}

object PropertyModelModel extends ModelType {
  val name = str;
  val range = propOf(classOf[Type]);
}

//this type is a member of both models
object BuiltInTypeModel extends ModelType(null, withTrait(ITypeModel, TypeModel))

//Knowledge data should not be global!!! //FIXME
object Mappings extends AssertionContainer {

  def typeMappings() {
    ITypeModel <=> classOf[IType];
    SourceTypeModel <=> classOf[SourceType];
    PackageElementModel<=>classOf[Package];
    CodeModelModel<=>classOf[CodeModel];
    SourceMemberModel <=> classOf[SourceMember]; //We should be able to build transform proto without mapping
    TypeModel <=> classOf[Type]
    PropertyModelModel <=> classOf[Property[_, _]] //We should check compatibility when stating it
    ModelTypeModel <=> classOf[ModelType[_]];
    
    BuiltInTypeModel
  }

  //first init mappings to classes;
  def definitions() = {
    
    ModelTypeModel <=> SourceTypeModel;
    //type to source type conversion
    ModelTypeModel.props <=> SourceTypeModel.children;
    TypeModel.typeNameProp <=> SourceTypeModel.name;
    TypeModel.superTypeProp <=> SourceTypeModel.superClass
    SourceTypeModel.parent.$.name<=>ModelTypeModel.modelPackage;
    //Property to SourceMember conversion
    PropertyModelModel.name <=> SourceMemberModel.name;
    PropertyModelModel.range <=> SourceMemberModel.elementsType;
    //Universe.children<=>CodeModelModel.children.$.children;//mind crash!!!
    
  }
}
object TargetTest1 extends App {
  
}

object TestApp extends App {
  Mappings.learn();
  
  var v: IModelElement[_] = Transformers.transformer(classOf[ModelType[_]], classOf[SourceType])(SourceTypeModel);
  println(v);
}
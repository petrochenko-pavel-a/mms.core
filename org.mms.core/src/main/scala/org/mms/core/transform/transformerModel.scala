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
import org.mms.core._
import org.mms.core.codemodel.SourceMember
import org.mms.core.runtime.RuntimeImplicits._;

trait CanBuildTransform{
  
  def build:Option[Seq[TransformationPrototype]];
}

trait TransformationPrototype{
  
  def toTransform():Tranformation[_,_]; 
}

case class OneToOne(val first:PropertyModel,another:PropertyModel) extends TransformationPrototype{
  def toTransform():Tranformation[_,_]=OneToOnePropertyTransform(first,another);  
}

case class OneWayTransform(from: Type, to: Type) extends CanBuildTransform{

  type SomeTransform=TransformationPrototype;
  type UnknownTransformsOneToOne=TransformsOneToOne[_,_,_,_];
  
  def build():Option[Seq[TransformationPrototype]] = {
    val sourceProp:Set[isPropertyOf[_]]=Entity.about(from,classOf[isPropertyOf[_]]);
    val tPropOf:Set[isPropertyOf[_]]=Entity.about(to,classOf[isPropertyOf[_]]);
    val tProps:Set[PropertyModel]=tPropOf.map { x => x.p };
    var noMappingTo=Set[PropertyModel]();
    var transforms=Map[PropertyModel,TransformationPrototype]();
    for (pOf<-sourceProp){
        val howMapsToTarget:SomeTransform=buildPerfectMapping(pOf.p,tProps);
        if (howMapsToTarget==null){
          noMappingTo=noMappingTo+pOf.p;          
        }
        else{
          var kv:Tuple2[PropertyModel,SomeTransform]=(pOf.p,howMapsToTarget);
          transforms=transforms+kv;
        }
    }
    if (!noMappingTo.isEmpty){
      return None;
    }
    return Some(transforms.values.toSeq);
  }
  def buildPerfectMapping(s:PropertyModel,target:Set[PropertyModel]):SomeTransform={
    
    val assertions=Entity.about(s,classOf[PropertyAssertion]);
    //here we may do a lot of smart things but for now we will be dumb, not transitivity, e.t.c at the moment
    for (x<-assertions){
      x match{
         case TransformsOneToOne(sp,tp)=>{
          if (target.contains(tp)){
            val tr:SomeTransform=buildTransform(sp,tp);
            return tr;
          }
          if (target.contains(sp)){
            val tr:SomeTransform=buildTransform(tp,sp);
            return tr;
          }
        }
      }
      
    }
    
    null;
  }
  def buildTransform(fr:PropertyModel,to:PropertyModel):SomeTransform={
    return OneToOne(fr,to);
  }
}
case class TwoWayTransform(first: Type, another: Type) extends CanBuildTransform{

  val FirstToAnother=OneWayTransform(first,another);
  val AnotherToFirst=OneWayTransform(another,first);
  
  override def build():Option[Seq[TransformationPrototype]] = {
    FirstToAnother.build();
    AnotherToFirst.build();
    ???
  }
}

object PropertyModelModel extends ModelType {
  val name = str;
  val range = propOf(classOf[Type]);
}

object SourceMemberModel extends ModelType {  
  val name = str;
  val elementsType = propOf(classOf[SourceMember])
}
object Mappings extends AssertionContainer {
  PropertyModelModel<=>classOf[Property[_,_]]//We should check compatibility when stating it
  SourceMemberModel<=>classOf[SourceMember];//We should be able to build transform proto without mapping
  PropertyModelModel.name <=> SourceMemberModel.name;
  PropertyModelModel.range <=> SourceMemberModel.elementsType;    
}


object TestApp extends App{
  Mappings.learn();
  val v:SourceMember=Transformers.transform(SourceMemberModel.name,classOf[SourceMember]);
  println(v);
}
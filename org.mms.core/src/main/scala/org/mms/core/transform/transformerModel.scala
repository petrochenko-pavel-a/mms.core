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
import org.mms.core.codemodel.IType

trait CanBuildTransform{
  
  def build:TransformationPrototype;
}

trait TransformationPrototype{
  
  def toTransform():Tranformation[_,_]; 
}

case class OneToOne(val first:PropertyModel,another:PropertyModel) extends TransformationPrototype{
  def toTransform():Tranformation[_,_]=OneToOnePropertyTransform(first,another);  
}
object  NilPrototype extends TransformationPrototype{
  def toTransform():Tranformation[_,_]=null;  
}
class TransformModel(val transforms:Seq[TransformationPrototype]) extends TransformationPrototype{
  def toTransform():Tranformation[_,_]={
    return new TransformationList(transforms.map { x => x.toTransform() }:_*);
  }
}
object TransformationModelRegistry{
   type PairOfType=Tuple2[Type,Type];
   
   var transforms=Map[PairOfType,TransformationPrototype]();

   
   
   private[transform] def get(from: Type, to: Type):TransformationPrototype={
     if (transforms.contains((from,to))){
       return transforms.get((from,to)).get;
     }
     val t:PairOfType =(from,to);
     val x=(t,NilPrototype);
     transforms=transforms+x;
     val tr=new OneWayTransform(from,to).build();
     if (tr!=null){
       val at=(t,tr);
       transforms=transforms+at; 
     }
     return null;
   }
   def transformer(from: Type, to: Type):TransformationPrototype={
     val x=get(from,to);
     if (x==NilPrototype){
       return null;
     }
     return x;
   }
}


case class OneWayTransform(from: Type, to: Type) extends CanBuildTransform{

  type SomeTransform=TransformationPrototype;
  type UnknownTransformsOneToOne=TransformsOneToOne[_,_,_,_];
  
  def build():TransformationPrototype = {
    val sourceProp:Set[isPropertyOf[_]]=Entity.about(from,classOf[isPropertyOf[_]]);
    val tPropOf:Set[isPropertyOf[_]]=Entity.about(to,classOf[isPropertyOf[_]]);
    val tProps:Set[PropertyModel]=tPropOf.map { x => x.p };
    var noMappingTo=Set[PropertyModel]();
    var transforms=Map[PropertyModel,TransformationPrototype]();
    for (pOf<-sourceProp){
        if (pOf.p.range()!=NothingType){
        
        val howMapsToTarget:SomeTransform=buildPerfectMapping(pOf.p,tProps);
        if (howMapsToTarget==null){
          noMappingTo=noMappingTo+pOf.p;          
        }
        else{
          var kv:Tuple2[PropertyModel,SomeTransform]=(pOf.p,howMapsToTarget);
          transforms=transforms+kv;
        }
        }
    }
    if (!noMappingTo.isEmpty){
      return null;
    }
    return new TransformModel(transforms.values.toSeq);
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
    if (fr.range()==to.range()){
      //this is perfect equivalent
      return OneToOne(fr,to);
    }
    val has=TransformationModelRegistry.get(fr.range(), to.range());
    if (has!=null){
      //we know how to transform types
      return OneToOne(fr,to);
    }
    return null;
  }
}
case class TwoWayTransform(first: Type, another: Type){

  val FirstToAnother=OneWayTransform(first,another);
  val AnotherToFirst=OneWayTransform(another,first);
  
  def build() = {
    FirstToAnother.build();
    AnotherToFirst.build();
  }
}

object PropertyModelModel extends ModelType {
  val name = str;
  val range = propOf(classOf[Type]);
}

object SourceMemberModel extends ModelType {  
  val name = str;
  val elementsType = propOf(classOf[IType])
}
object TypeModel extends ModelType{
  val typeNameProp=str;
  val superTypeProp=propOf(TypeModel)
}
object ModelTypeModel extends ModelType(TypeModel){
  val packageNameProp=str;
  val properties=list(str)
}
object ITypeModel extends ModelType

object SourceTypeModel extends ModelType{
  val name=str;
  val children=propOf(SourceMemberModel); 
}


object Mappings extends AssertionContainer {
  PropertyModelModel<=>classOf[Prop[_,_]]//We should check compatibility when stating it
  SourceMemberModel<=>classOf[SourceMember];//We should be able to build transform proto without mapping
  PropertyModelModel.name <=> SourceMemberModel.name;
  PropertyModelModel.range <=> SourceMemberModel.elementsType;    
}


object TestApp extends App{
  Mappings.learn();
  val v:SourceMember=Transformers.transform(SourceMemberModel.name,classOf[SourceMember]);
  println(v);
}
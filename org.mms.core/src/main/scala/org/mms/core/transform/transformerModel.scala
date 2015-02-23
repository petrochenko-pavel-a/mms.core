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
import org.mms.core.runtime.RuntimeImplicits._
import org.mms.core.codemodel.IType
import org.mms.core.runtime.MapsTo

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
     val tr=new MultiTypeTransform(from,to).build();
     if (tr!=null){
       val at=(t,tr);
       transforms=transforms+at; 
       
     }
     return tr;
   }
   def transformer(from: Type, to: Type):TransformationPrototype={
     val x=get(from,to);
     if (x==NilPrototype){
       return null;
     }
     return x;
   }
}

case class TypeAssertion(val condition:Predicate, val resultType: Type,val transform:TransformationPrototype);
case class TypeDescriminator(assertions:TypeAssertion*);

case class DescPrototypes(val descr:TypeDescriminator*) extends TransformationPrototype{
  def toTransform():Tranformation[_,_] =null;
}

case class MultiTypeTransform(from: Type, to: Type)extends CanBuildTransform{
  
  def buildDescriminator(t:Type,targets:Set[Type]):TypeDescriminator={
   val mappings=t.about(classOf[MapsTo]);
   var assertions=List[TypeAssertion]();
   for (m<-mappings){
     if (targets.contains(m.another)){
       val assertion:TypeAssertion=buildAssertion(m);    
       assertions=assertions.::(assertion);
     }
   }
   if (isComplete(assertions)){
     return TypeDescriminator(assertions:_*);
   }
   return null; 
  }
  def isComplete(assertions:List[TypeAssertion]):Boolean=(!assertions.isEmpty);
  
  def buildAssertion(m:MapsTo):TypeAssertion={
    val trp=TransformationModelRegistry.get(m.first, m.another);
    if (trp!=null){
      return TypeAssertion(Predicate.TRUE,m.another,trp);
    }
    return null;
  }
  
  
  def build():TransformationPrototype = {
     var allF=(from.allSubClasses()+from);
     allF=allF.filter { x => !x.isAbstract() }
     val allT=(to.allSubClasses()+to).filter { x => !x.isAbstract() };
     if (allF.size==1){
       return TransformBuilder(from,to).build();
     }
     else{
       allF=allF--allT;
       if (allF.size>1||allT.size>1){
         //we need to build descriminator here.
         var list=List[TypeDescriminator]()
         for (t<-allF){
           val descr=buildDescriminator(t,allT);
           if(descr==null){
             return null;
           }
           list=list.::(descr);
         }
         return DescPrototypes(list:_*);
       }
       else{
         return TransformBuilder(from,to).build();
       }
     }
     return null;
  }
}

/**
 * builds mapping from exactly one type to another!!!
 */
case class TransformBuilder(from: Type, to: Type) extends CanBuildTransform{

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
      println(from+"->"+to+":"+noMappingTo)
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
    if (to.range.isAssignableFrom(fr.range)){
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
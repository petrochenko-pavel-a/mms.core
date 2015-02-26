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
import org.mms.core.runtime.RuntimeImplicits
import org.mms.core.runtime.IRuntimeProperty
import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap


trait CanBuildTransform {

  def build: TransformationPrototype;
}

trait TransformationPrototype {

  def toTransform(f: Type, t: Type): TranformationFunction[_, _];
}
trait CanProduceTransformation {

  def toTransformation(): Tranformation[_, _];
  
  def targetProps():Seq[PropertyModel];
}

case class OneToOne(val first: PropertyModel, another: PropertyModel, val tfunction: TransformationPrototype = null) extends CanProduceTransformation {
  
  def targetProps():Seq[PropertyModel]=List(another);
  
  def toTransformation(): Tranformation[_, _] =
    {
      if (first.isInstanceOf[ListProp[_, _]] && another.isInstanceOf[ListProp[_, _]]) {
        if (tfunction != null) {
          val x = tfunction.toTransform(first.range, another.range);
          return OneToOnePropertyTransform.createWithTransform(first, another, listTransformFunction(x).asInstanceOf[TranformationFunction[_, _]]);
        }
        else{
          val x:TranformationFunction[_,_] = identityFunction.asInstanceOf[TranformationFunction[_,_]];
          return OneToOnePropertyTransform.createWithTransform(first, another, listTransformFunction(x).asInstanceOf[TranformationFunction[_, _]]);
        }
      }
      if (tfunction != null) {
        return OneToOnePropertyTransform.createWithTransform(first, another, tfunction.toTransform(first.range, another.range));
      } else OneToOnePropertyTransform(first, another);
    }
}
case class UnversalTransform(val q: TransformationPrototype,f: Type, t: Type) extends TranformationFunction[Any,Any]{
  def apply(a:Any):Any={
    val z=q.toTransform(f,t).asInstanceOf[TranformationFunction[Any,Any]];
    return z.apply(a);
 }
}

object NilPrototype extends TransformationPrototype with CanProduceTransformation {
  
  def targetProps():Seq[PropertyModel]=throw new UnsupportedOperationException;
  def toTransform(f: Type, t: Type): TranformationFunction[_, _] = {
    val z=TransformationModelRegistry.get(f, t);
    if (z==this){
      return null;
    }
    return UnversalTransform(z,f,t);   
  }

  def toTransformation(): Tranformation[_, _] = {
   return null;
  }
}
class CompositeTransformation(val transforms: Seq[CanProduceTransformation],targetType:Type) extends TransformationPrototype {
  if (transforms.contains(NilPrototype)){
    throw new IllegalStateException;
  }
  
  def toTransformation(): Tranformation[_, _] = {
    val trList=transforms.map { x => x.toTransformation() }.toList;
    return new TransformationList(trList: _*);
  }
  def toTransform(f: Type, t: Type): TranformationFunction[_, _] =
    {
      val c1: Class[Any] = RuntimeImplicits.typeToRuntime(f).asInstanceOf[Class[Any]];
      val c2: Class[Any] = RuntimeImplicits.typeToRuntime(targetType).asInstanceOf[Class[Any]];
      CalculatedTransform(c1, c2, this.toTransformation().asInstanceOf[Tranformation[Any, Any]]);
    }
}
object TransformationModelRegistry {
  type PairOfType = Tuple2[Type, Type];

  var transforms = Map[PairOfType, TransformationPrototype]();

  private[transform] def get(from: Type, to: Type): TransformationPrototype = {
    if (transforms.contains((from, to))) {
      return transforms.get((from, to)).get;
    }
    val t: PairOfType = (from, to);
    val x = (t, NilPrototype);
    transforms = transforms + x;
    var tr = new MultiTypeTransform(from, to).build();
    if (tr != null) {
      val at = (t, tr);
      transforms = transforms + at;
    }
    else{
      tr = new MultiTypeTransform(from, to).build();
      println("Can not build transform from:"+from+" "+to)
    }
    return tr;
  }
  def transformer(from: Type, to: Type): TransformationPrototype = {
    val x = get(from.toModelIfPossible, to.toModelIfPossible);
    if (x == NilPrototype) {
      return null;
    }
    return x;
  }
}

case class TypeAssertion(val condition: Predicate, val sourceType: Type, val resultType: Type, val transform: TransformationPrototype);
case class TypeDescriminator(sourceType: Type, assertions: TypeAssertion*);

case class DescPrototypes(val descr: TypeDescriminator*) extends TransformationPrototype {
  def toTransform(f: Type, t: Type): TranformationFunction[_, _] = {
    val rdesc: Seq[RuntimeDescriminator[_, _]] = descr.map { x => convert(x) };
    return DescriminatedTransform(t, rdesc: _*);
  }
  def convert(t: TypeDescriminator): RuntimeDescriminator[_, _] = {
    return RuntimeDescriminator(t.sourceType, t.assertions.map { x => convert(x) }: _*)
  }
  def convert(t: TypeAssertion): ConditionedAssertion[_, _] = {
    return ConditionedAssertion(convert(t.condition), t.transform.toTransform(t.sourceType, t.resultType));
  }
  def convert(t: Predicate): RuntimeCondition = {
    return RuntimeCondition();
  }
}

case class MultiTypeTransform(from: Type, to: Type) extends CanBuildTransform {

  def buildDescriminator(t: Type, targets: Set[Type]): TypeDescriminator = {
    val mappings = t.about(classOf[MapsTo]);
    var assertions = List[TypeAssertion]();
    for (m <- mappings) {
      if (targets.contains(m.another)) {
        val assertion: TypeAssertion = buildAssertion(m);
        assertions = assertions.::(assertion);
      }
    }
    if (isComplete(assertions)) {
      return TypeDescriminator(t, assertions: _*);
    }
    return null;
  }
  def isComplete(assertions: List[TypeAssertion]): Boolean = (!assertions.isEmpty);

  def buildAssertion(m: MapsTo): TypeAssertion = {
    val trp = TransformationModelRegistry.get(m.first, m.another);
    if (trp != null) {
      return TypeAssertion(Predicate.TRUE, m.first, m.another, trp);
    }
    return null;
  }

  def build(): TransformationPrototype = {
    var allF = (from.allSubClasses() + from);
    allF = allF.filter { x => !x.isAbstract() }
    val allT = (to.allSubClasses() + to).filter { x => !x.isAbstract() };
      if (allF.size==1&&allT.size==1){
        return TransformBuilder(allF.toList(0),allT.toList(0)).build();
      }
      allF = allF -- allT;
      if (allF.size > 0 && allT.size > 0) {
        //we need to build descriminator here.
        var list = List[TypeDescriminator]()
        for (t <- allF) {
          val descr = buildDescriminator(t, allT);
          if (descr == null) {
            return null;
          }
          list = list.::(descr);
        }
        return DescPrototypes(list: _*);
      }     
    return null;
  }
}

class SubPropertyInitializer(val up:PropertyModel,val trs:List[CanProduceTransformation]) extends CanProduceTransformation {
  
  def targetProps():Seq[PropertyModel]=List(up);
  
  def toTransformation(): Tranformation[_, _] = {
    //TODO choose correct type
    val v0:IRuntimeProperty[Any,Any]=RuntimeImplicits.propToRuntime(up).asInstanceOf[IRuntimeProperty[Any,Any]];
    val c=new CompositeTransformation(trs,up.domain()).toTransformation();
    val v1:Tranformation[Any,Any]=c.asInstanceOf[Tranformation[Any,Any]];
    return new ObjectInitTransform(v0,v1);
  }
}

/**
 * builds mapping from exactly one type to another!!!
 */
case class TransformBuilder(from: Type, to: Type) extends CanBuildTransform {

  type SomeTransform = CanProduceTransformation;
  type UnknownTransformsOneToOne = TransformsOneToOne[_, _, _, _];

  def build(): TransformationPrototype = {
    val sourceProp: Set[PropertyModel] = from.properties();
    val tProps: Set[PropertyModel] = to.properties();
    var noMappingTo = Set[PropertyModel]();
    var transforms = Map[PropertyModel, CanProduceTransformation]();
    for (pOf <- sourceProp) {
      if (pOf.range() != NothingType) {

        val howMapsToTarget: SomeTransform = buildPerfectMapping(pOf, tProps);
        if (howMapsToTarget==NilPrototype){
          throw new IllegalStateException;
        }
        if (howMapsToTarget == null) {
          noMappingTo = noMappingTo + pOf;
        } else {
          var kv: Tuple2[PropertyModel, SomeTransform] = (pOf, howMapsToTarget);
          transforms = transforms + kv;
        }
      }
    }
    if (!noMappingTo.isEmpty) {
      println(from + "->" + to + ":" + noMappingTo)
      return null;
    }
    //no we should group sub property initialization together;
    val allTransforms= transforms.values.toSeq;
    var reified=upLift(allTransforms);
    var affectedProps=Set[PropertyModel]();
    for (x<-reified){
      affectedProps=affectedProps.++(x.targetProps());
    }
    var unaffectedProps=tProps--affectedProps;
    return new CompositeTransformation(reified,to);
  }

  def upLift(allTransforms: Seq[org.mms.core.transform.CanProduceTransformation]):List[org.mms.core.transform.CanProduceTransformation] = {
    val mappings=allTransforms.groupBy{rootProp};
    var reified=List[CanProduceTransformation]();
    
    for (v<-mappings){
      if (v._1==null){
        reified=reified.:::(v._2.toList);
      }
      else{
        //FIXME Direct assertions about stuff asserted in sub properties
        var upLifted=List[CanProduceTransformation]();
        for (m<-v._2){
          val oto=m.asInstanceOf[OneToOne];
          if (oto.another.isInstanceOf[SubProp[_,_]]){
            val upLift=oto.another.asInstanceOf[SubProp[_,_]].oneLevelUp();
            val current=oto.first;
            val newOne=OneToOne(current,upLift,oto.tfunction);
            upLifted=upLifted.::(newOne);
          }
          else{
            throw new IllegalStateException;
          }
        }
        reified=reified.::(new SubPropertyInitializer(v._1,upLift(upLifted)));
        //this is sub property grouping;
      }
    }
    
    reified
  }
  def rootProp(x:CanProduceTransformation):PropertyModel={
      if (x.isInstanceOf[OneToOne]){
        val oto=x.asInstanceOf[OneToOne];
        if (oto.another.isInstanceOf[SubProp[_,_]]){
          return oto.another.asInstanceOf[SubProp[_,_]].rootProperty();
        }
        return null;
      } 
      else{
        return null;
      }  
  }
  
  def buildPerfectMapping(s: PropertyModel, target: Set[PropertyModel]): SomeTransform = {
    val assertions = Entity.about(s, classOf[PropertyAssertion]);
    //here we may do a lot of smart things but for now we will be dumb, not transitivity, e.t.c at the moment
    for (x <- assertions) {
      x match {
        case TransformsOneToOne(sp, tp) => {
          if (tp.isInstanceOf[SubProp[_,_]]){
            if (target.contains(tp.asInstanceOf[SubProp[_,_]].rootProperty)){
               val tr: SomeTransform = buildTransform(sp, tp);
               return tr; 
            }
          }
          if (sp.isInstanceOf[SubProp[_,_]]){
            if (target.contains(sp.asInstanceOf[SubProp[_,_]].rootProperty)){
               val tr: SomeTransform = buildTransform(tp, sp);
               return tr; 
            }
          }
          if (target.contains(tp)) {
            val tr: SomeTransform = buildTransform(sp, tp);
            return tr;
          }
          if (target.contains(sp)) {
            val tr: SomeTransform = buildTransform(tp, sp);
            return tr;
          }
        }
      }
    }
    null;
  }

  def buildTransform(fr: PropertyModel, to: PropertyModel): SomeTransform = {
    if (fr.range() == to.range()) {
      //this is perfect equivalent
      return OneToOne(fr, to);
    }
    if (to.range.isAssignableFrom(fr.range)) {
      return OneToOne(fr, to);
    }

    val has = TransformationModelRegistry.get(fr.range(), to.range());
    if (has != null) {
      return OneToOne(fr, to, has);
    }
    return null;
  }
}
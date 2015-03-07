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
import org.mms.core.runtime.IRuntimeProperty
import org.mms.core.runtime.MapsTo

trait CanBuildTransform {

  def build: TransformationPrototype;
}

trait TransformationPrototype {

  def toTransform(f: Type, t: Type): TranformationFunction[_, _];
}
trait CanProduceTransformation {

  def toTransformation(): Tranformation[_, _];

  def targetProps(): Seq[PropertyModel];
}

case class OneToOne(val first: PropertyModel, another: PropertyModel, val tfunction: TransformationPrototype = null) extends CanProduceTransformation {

  def targetProps(): Seq[PropertyModel] = List(another);

  def toTransformation(): Tranformation[_, _] =
    {
      if (first.isInstanceOf[ListProp[_, _]] && another.isInstanceOf[ListProp[_, _]]) {
        if (tfunction != null) {
          val x = tfunction.toTransform(first.range, another.range);
          return OneToOnePropertyTransform.createWithTransform(first, another, listTransformFunction(x).asInstanceOf[TranformationFunction[_, _]]);
        } else {
          val x: TranformationFunction[_, _] = identityFunction.asInstanceOf[TranformationFunction[_, _]];
          return OneToOnePropertyTransform.createWithTransform(first, another, listTransformFunction(x).asInstanceOf[TranformationFunction[_, _]]);
        }
      }
      if (tfunction != null) {
        return OneToOnePropertyTransform.createWithTransform(first, another, tfunction.toTransform(first.range, another.range));
      } else OneToOnePropertyTransform(first, another);
    }
}
case class UnversalTransform(val q: TransformationPrototype, f: Type, t: Type) extends TranformationFunction[Any, Any] {
  def apply(a: Any): Any = {
    val z = q.toTransform(f, t).asInstanceOf[TranformationFunction[Any, Any]];
    return z.apply(a);
  }
}

object NilPrototype extends TransformationPrototype with CanProduceTransformation {

  def targetProps(): Seq[PropertyModel] = throw new UnsupportedOperationException;
  def toTransform(f: Type, t: Type): TranformationFunction[_, _] = {
    val z = TransformationModelRegistry.get(f, t);
    if (z == this) {
      return null;
    }
    return UnversalTransform(z, f, t);
  }

  def toTransformation(): Tranformation[_, _] = {
    return null;
  }
}
class CompositeTransformation(val transforms: Seq[CanProduceTransformation], targetType: Type) extends TransformationPrototype {
  if (transforms.contains(NilPrototype)) {
    throw new IllegalStateException;
  }

  def toTransformation(): Tranformation[_, _] = {
    val trList = transforms.map { x => x.toTransformation() }.toList;
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
    } else {
      tr = new MultiTypeTransform(from, to).build();
      println("Can not build transform from:" + from + " " + to)
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
    if (allF.size == 1 && allT.size == 1) {
      return TransformBuilder(allF.toList(0), allT.toList(0)).build();
    }
    allF = allF -- allT;
    if (allF.size > 0 && allT.size > 0) {
      //we need to build descriminator here.
      var list = List[TypeDescriminator]()
      for (t <- allF) {
        val descr = buildDescriminator(t, allT);
        if (descr != null) {
        
        list = list.::(descr);
        }
      }
      if (!list.isEmpty){
      return DescPrototypes(list: _*);}
    }
    if (allT.size==1){
      //FIXME
      return TransformBuilder(allF.toList(0), allT.toList(0)).build();
   
    }
    return null;
  }
}

class SubPropertyInitializerOneToOne(val up: PropertyModel, val trs: List[CanProduceTransformation]) extends CanProduceTransformation {

  def targetProps(): Seq[PropertyModel] = List(up);

  if (up.isDecorated()) {
    throw new IllegalArgumentException();
  }

  def toTransformation(): Tranformation[_, _] = {

    //TODO choose correct type
    val v0: IRuntimeProperty[Any, Any] = RuntimeImplicits.propToRuntime(up).asInstanceOf[IRuntimeProperty[Any, Any]];
    val c = new CompositeTransformation(trs, up.domain()).toTransformation();

    val v1: Tranformation[Any, Any] = c.asInstanceOf[Tranformation[Any, Any]];
    return new ObjectInitTransform(v0, v1);
  }
}
case class DelegatingProp(p: IRuntimeProperty[Any, Any]) extends IRuntimeProperty[Any, Any] {
  def getter(): Any => Any = {
    p.getter()
  }

  def meta(): PropertyModel = {
    p.meta();
  }

  def range(): Class[Any] = {
    p.range();
  }

  var newValue: Any = _;

  def setter(): (Any, Any) => Unit = {
    return new Function2[Any, Any, Unit]() {

      def apply(b: Any, v: Any): Unit = {
        newValue = v;
      }
    }
  }
}
class SubPropertyInitializerOneToMany(val up: PropertyModel, val trs: List[CanProduceTransformation]) extends CanProduceTransformation {

  def targetProps(): Seq[PropertyModel] = List(up);

  if (!up.isDecorated()) {
    throw new IllegalArgumentException();
  }

  var dp: DelegatingProp = _;

  def updateProp(z: Tranformation[Any, Any]): Tranformation[Any, Any] = {
    if (z.isInstanceOf[OneToOnePropertyTransform[_, _, _, _]]) {
      val tp = z.asInstanceOf[OneToOnePropertyTransform[_, _, _, _]];
      val targetProp: IRuntimeProperty[_, _] = tp.tP;
      val meta = targetProp.meta();
      val pch = meta.about(classOf[ParentChildAssertion[_, _]]);
      if (!pch.isEmpty) {
        for (p <- pch) {
          val chP = p.child.asInstanceOf[PropertyModel];
          val ppP = p.parent.asInstanceOf[PropertyModel];
          if (meta == chP) {
            dp = new DelegatingProp(targetProp.asInstanceOf[IRuntimeProperty[Any, Any]]);
            return OneToOnePropertyTransform(tp.sP.asInstanceOf[IRuntimeProperty[Any,Any]], dp, tp.tF.asInstanceOf[Function1[Any, Any]]);
          }
        }
      }
    }
    if (z.isInstanceOf[ManyToManyInitTransform[_,_,_,_]]){
      val mm=z.asInstanceOf[ManyToManyInitTransform[_,_,_,_]]
      dp = new DelegatingProp(mm.tP.asInstanceOf[IRuntimeProperty[Any,Any]]);
      return new ManyToManyInitTransform(dp,mm.initFunction.asInstanceOf[Tranformation[Any,Any]],mm.parentProp,mm.delegate);
            
    }
    return z;
  }
  // this.packages.$.units.$.types
  // type.unit.package;
  // units may be gathered from first level
  def toTransformation(): Tranformation[_, _] = {
    //TODO choose correct type
    
    val v0: IRuntimeProperty[Any, Any] = RuntimeImplicits.propToRuntime(up).asInstanceOf[IRuntimeProperty[Any, Any]];
    var transforms = new CompositeTransformation(trs, up.domain()).toTransformation().asInstanceOf[TransformationList[Any, Any]];
    val filtered = transforms.seq.map(p => updateProp(p));
    transforms = TransformationList(filtered: _*);
    if(dp!=null){
    var pchAss=dp.meta().about(classOf[ParentChildAssertion[_,_]]);
    if (pchAss.isEmpty){
      pchAss=dp.meta().withDecorators().about(classOf[ParentChildAssertion[_,_]]);
      
    }
    for (i<-pchAss){
      if (i.child==dp.meta.withDecorators()){
      val ppP =RuntimeImplicits.propToRuntime(i.parent.asInstanceOf[PropertyModel]).asInstanceOf[IRuntimeProperty[Any, Any]];
      return new ManyToManyInitTransform(v0, transforms, ppP, dp);
      }
      }      
    }
    //}
    throw new IllegalStateException;

  }
}

case class InitDefault(val up: PropertyModel) extends CanProduceTransformation {

  
  def toTransformation(): Tranformation[_, _] = {
    //TODO choose correct type
    val v0: IRuntimeProperty[Any, Any] = RuntimeImplicits.propToRuntime(up).asInstanceOf[IRuntimeProperty[Any, Any]];
    var initTransform=List[CanProduceTransformation]();
    for (pr<-up.range().properties()){
      if (pr.isRequired){
        initTransform=initTransform.::(InitDefault(pr))
      }
    }
    val c = new CompositeTransformation(initTransform, up.domain()).toTransformation();
    val v1: Tranformation[Any, Any] = c.asInstanceOf[Tranformation[Any, Any]];
    return new ObjectInitTransform(v0, v1);
  }

  def targetProps(): Seq[PropertyModel] = List(up);
}

/**
 * builds mapping from exactly one type to another!!!
 */
case class TransformBuilder(from: Type, to: Type) extends CanBuildTransform {

  type SomeTransform = CanProduceTransformation;
  type UnknownTransformsOneToOne = TransformsOneToOne[_, _, _, _];

  def build(): TransformationPrototype = {
    val sourceProp: Set[PropertyModel] = from.properties();
    val tProps: Set[PropertyModel] = to.properties().map { x => x.rootProperty() };
    var noMappingTo = Set[PropertyModel]();
    var transforms = List[CanProduceTransformation]();
    for (pOf <- sourceProp) {
      if (pOf.range() != NothingType) {
        val transformList = buildPerfectMapping(pOf, tProps);
        
        if (transformList.isEmpty&&(!pOf.isComputed)) {
            noMappingTo = noMappingTo + pOf;
        }
        else for (howMapsToTarget <- transformList) {
          if (howMapsToTarget == NilPrototype) {
            throw new IllegalStateException;
          }
          
          //var kv: Tuple2[PropertyModel, SomeTransform] = (pOf, howMapsToTarget);
          transforms = transforms.::(howMapsToTarget);
          
        }
      }
    }
    var q:Type=from;
    while (q!=null){
      val desc=q.about(classOf[ConstantDescriminator]);
      for (a<-desc){
        if (to.properties().contains(a.assertion.base)){
           transforms = transforms.::(a.assertion);
        }
      }
      q=q.superType;
    }
    if (!noMappingTo.isEmpty) {
      println(from + "->" + to + ":" + noMappingTo)
      return null;
    }
    //no we should group sub property initialization together;
    val allTransforms = transforms;
    //
    
    var reified = upLift(allTransforms, tProps);

    return new CompositeTransformation(reified, to);
  }

  def upLift(allTransforms: Seq[org.mms.core.transform.CanProduceTransformation], tProps: Set[PropertyModel]): List[org.mms.core.transform.CanProduceTransformation] = {
    val mappings = allTransforms.groupBy { rootProp };
    var reified = List[CanProduceTransformation]();

    for (v <- mappings) {
      if (v._1 == null) {
        reified = reified.:::(v._2.toList);
      } else {
        //FIXME Direct assertions about stuff asserted in sub properties
        var upLifted = List[CanProduceTransformation]();
        for (m <- v._2) {
          val oto = m.asInstanceOf[OneToOne];
          var withoutDec = oto.another.withoutDecorators()
          if (withoutDec.isInstanceOf[SubProp[_, _]]) {
            val upLift = withoutDec.asInstanceOf[SubProp[_, _]].oneLevelUp();
            val current = oto.first;
            val newOne = OneToOne(current, upLift, oto.tfunction);
            upLifted = upLifted.::(newOne);
          } else {
            throw new IllegalStateException;
          }
        }
        val tProps: Set[PropertyModel] = v._1.range().properties();
        var initializer: CanProduceTransformation = null;
        if (v._1.isDecorated()) {
          //one to many

          initializer = new SubPropertyInitializerOneToMany(v._1, upLift(upLifted, tProps));
        } else {
          //one to one//it is simple case
          initializer = new SubPropertyInitializerOneToOne(v._1, upLift(upLifted, tProps));
        }
        reified = reified.::(initializer);
        //this is sub property grouping;
      }
    }
    var affectedProps = Set[PropertyModel]();
    for (x <- reified) {
      
      affectedProps = affectedProps.++(x.targetProps());
    }
    var unaffectedProps = tProps -- affectedProps.map { x => x.rootProperty() };
    for (p <- unaffectedProps) {
      if (p.isRequired) {
        reified = reified.::(InitDefault(p));
      }
    }
    reified
  }
  def rootProp(x: CanProduceTransformation): PropertyModel = {
    if (x.isInstanceOf[OneToOne]) {
      val oto = x.asInstanceOf[OneToOne];
      var xp = oto.another.withoutDecorators();
      if (xp.isInstanceOf[SubProp[_, _]]) {
        return xp.rootProperty();
      }
      return null;
    } else {
      return null;
    }
  }

  def buildPerfectMapping(s: PropertyModel, target: Set[PropertyModel]): List[SomeTransform] = {
    var result = List[SomeTransform]();
    val assertions = Entity.about(s, classOf[PropertyAssertion]);
    //here we may do a lot of smart things but for now we will be dumb, not transitivity, e.t.c at the moment
    for (x <- assertions) {
      x match {
        case TransformsOneToOne(sp, tp) => {
          if (target.contains(tp.rootProperty())) {
            val tr: SomeTransform = buildTransform(sp, tp);
            result = result.::(tr);
          }
          if (target.contains(sp.rootProperty())) {
            val tr: SomeTransform = buildTransform(tp, sp);
            result = result.::(tr);

          }
        }
      }
    }
    return result;
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
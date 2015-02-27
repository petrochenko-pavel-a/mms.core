package org.mms.core

import scala.collection.mutable.HashMap
import java.lang.reflect.Field
import org.mms.core.runtime.IRuntimeProperty
import java.lang.reflect.Modifier

case class isPropertyOf[T <: Type](val t: T, p: PropertyModel) extends FactAnnotation;
case class isRangeOf[T <: Type](val t: T, p: PropertyModel) extends FactAnnotation;

protected[core] object StackDetails {
  var prop: ThreadLocal[Property[_ <: Type, _ <: Type]] = new ThreadLocal();
}
/**
 * @author kor
 */
abstract class Property[DomainType <: Type, RangeType <: Type] extends Entity[Property[DomainType, RangeType]] {

  def $(): RangeType = {
    val r = range().getClass;

    val constructor = range.getClass.getDeclaredConstructor();
    try {
      StackDetails.prop.set(this);
      constructor.setAccessible(true); //TODO Super classes and interfaces
      val singletonField = r.getDeclaredField("MODULE$");
      singletonField.setAccessible(true);
      
      var result: RangeType = null.asInstanceOf[RangeType];
      r.synchronized {
        val normalInstance = singletonField.get(null);
        result = constructor.newInstance();
        
      }
      return result;
    } finally {
      StackDetails.prop.set(null);
      constructor.newInstance();
    }
  };
  private def makeModifiable(f: Field) {
    var modifiers = f.getModifiers();
    val modifierField = f.getClass().getDeclaredField("modifiers");
    modifiers = modifiers & ~Modifier.FINAL;
    modifierField.setAccessible(true);
    modifierField.setInt(f, modifiers);
  }
  var index=0;
  def isKey = _key;
  def isRequired = _required;
  protected[core] var _key = false;
  protected[core] var _required = false;
  def range(): RangeType
  def domain(): DomainType
  def name(): String;

  def withDefault(defaultValue: Any): Property[DomainType, RangeType] = this;

  def withName(name: String): Property[DomainType, RangeType] = this;

  def rootProperty(): PropertyModel = this;
  def withoutDecorators(): PropertyModel = this;
  def withDecorators(): PropertyModel = this;
  def isDecorated(): Boolean = false;

  protected def init() {
    val d=domain().asInstanceOf[ModelType[_]];
    this.index=d.index;
    d.index=d.index+1;
    if (StackDetails.prop.get == null) {
      register(domain, isPropertyOf(domain, this));
      //register(range, isRangeOf(range, this));
    }
    //we need unregister3
  }
  init();

  override def toString(): String = {
    domainString() + "." + name + typeDescr();
  }
  var locked=false;
  protected[core] override  def getTrueVersionOfThis():Entity[_]={
    if (!locked){
      return this;
    }
    val x=domain().property(this.name());
    if (x!=null){
      return x;
    }
    return this;
  }
  
  locked=true;
  protected[core] def domainString() = domain().toString();
  def typeDescr() = ":" + range();

  def <=>[A <: Type, B <: Type](p: Property[_, _]): PropertyAssertion = TransformsOneToOne(this, p.asInstanceOf[Property[A, B]]);
}

trait PropertyAssertion extends FactAnnotation with Entity[PropertyAssertion] with Function1[Seq[PropertyAssertion], PropertyAssertion] {
  def apply(s: Seq[PropertyAssertion]): PropertyAssertion = {
    return this;
  }
}

trait AssertionContainer {

  def typeMappings(); Unit;
  def definitions(): Unit;

  def learn() {
    typeMappings();
    definitions();
  };
}
trait Predicate {

}
object Predicate {
  object TRUE extends Predicate {}
  object FALSE extends Predicate {}
}

private abstract class ObjectDefinedProp[D <: ModelType[_], R <: Type](val domain: D, protected var nameOverride: String = null) extends Property[D, R] {

  override def withName(name: String): Property[D, R] = {
    nameOverride = name; return this;
  }

  override def isDecorated(): Boolean = delegatesTo != null;

  protected[core] var delegatesTo: ObjectDefinedProp[D, R] = _;

  override def withDecorators(): PropertyModel = {
    if (delegatesTo != null) {
      return delegatesTo.withDecorators();
    }
    return this;
  }

  def field(): Field = {
    val z: HashMap[ObjectDefinedProp[D, R], Field] = domain.metainf.pToFieldMap.asInstanceOf[HashMap[ObjectDefinedProp[D, R], Field]];
    val x = z.get(this);
    if (!x.isDefined) {
      return null;
    }
    return x.get;
  }
  def name(): String = {
    if (nameOverride != null) {
      return nameOverride;
    }
    val f = field;
    if (f == null) {
      if (delegatesTo != null) {
        return delegatesTo.name();
      }
      return "<unbinded prop>";
    }
    return field.getName;
  }

 
}

private[core] class Prop[D <: ModelType[_], R <: Type](override val domain: D, val range: R) extends ObjectDefinedProp[D, R](domain);
private[core] class SubProp[D <: ModelType[_], R <: Type](override val domain: D, override val range: R, val child:PropertyModel, val parent: Property[_ <: Type, _ <: Type]) extends Prop[D, R](domain, range) {

  override def domainString(): String = parent.domainString() + "." + parent.name() + ".$";

  override def rootProperty(): PropertyModel = {
    return parent.rootProperty();
  }
  override protected def init() {

  }
  protected[core] override  def getTrueVersionOfThis():Entity[_]={
    return this;
  }
  
  override def hashCode():Int={
    return child.getTrueVersionOfThis().hashCode()+parent.getTrueVersionOfThis().hashCode();
  }
  override def equals(v:Any):Boolean={
    if (v.getClass()!=this.getClass){
      return false;
    }
    val m=v.asInstanceOf[SubProp[D,R]];
    return child.getTrueVersionOfThis().equals(m.child.getTrueVersionOfThis())&&parent.getTrueVersionOfThis().equals(m.parent.getTrueVersionOfThis());
  }

  def oneLevelUp(): PropertyModel = {
    var q: Property[_, _] = parent;
    if (parent.isInstanceOf[DelegateProp[_, _]]) {
      q = parent.asInstanceOf[DelegateProp[_, _]].undelegate();
    }
    if (q.isInstanceOf[SubProp[_, _]]) {
      val p = q.asInstanceOf[SubProp[_, _]].oneLevelUp();
      val mm = p.$;
      if (mm.isInstanceOf[ModelType[_]]) {
        return mm.asInstanceOf[ModelType[_]].getPropFromMetaInf(name);
      }
      return mm.property(name());
    }
    return childProperty();
  }

  def childProperty(): Property[D, R] = {
    for (x <- domain.properties()) {
      if (x.name() == this.name()) {
        return x.asInstanceOf[Property[D, R]];
      }
    }
    return null;
  }
}

private[core] abstract class DelegateProp[D <: ModelType[_], R <: Type](val p: Property[D, R]) extends ObjectDefinedProp[D, R](p.domain()) {
  remove(p);
  def range(): R = p.range;

  override def isDecorated(): Boolean = true;

  override def withoutDecorators(): PropertyModel = {
    return p.withoutDecorators();
  }

  override def rootProperty(): PropertyModel = {
    return p.rootProperty();
  }

  def undelegate(): Property[D, R] = {
    if (p.isInstanceOf[DelegateProp[D, R]]) {
      return p.asInstanceOf[DelegateProp[D, R]].undelegate();
    }
    return p;
  }
  override def domainString(): String = p.domainString()
  if (p.isInstanceOf[ObjectDefinedProp[D, R]]) {
    p.asInstanceOf[ObjectDefinedProp[D, R]].delegatesTo = this;
  }

  override def withName(name: String): Property[D, R] = {
    super.withName(name);
  }

}

private[core] case class ListProp[D <: ModelType[_], R <: Type](override val p: Property[D, R]) extends DelegateProp[D, R](p) {
  override def typeDescr() = ":list(" + range() + ")";
}
private[core] case class KeyProp[D <: ModelType[_], R <: Type](override val p: Property[D, R]) extends DelegateProp[D, R](p) {}
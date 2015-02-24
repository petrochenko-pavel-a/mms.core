package org.mms.core.runtime;

import org.mms.core.Property
import org.mms.core._;
import java.lang.reflect.Method

trait IRuntimeProperty[D, R] {
  def meta(): PropertyModel;
  def getter(): Function1[D, R];
  def setter(): Function2[D, R, Unit];
  def range(): Class[R]
  def get(d: D): R = { getter()(d) };
  def set(d: D, r: R) {
    if (!readOnly) {
      setter()(d, r);
    }
    else{
      println("attempt to modify readonly property:"+meta.name()+" of "+meta.domain())
    }
  };
  def readOnly = setter == null;
}
trait ICollectionProperty[D, Col, Elem] extends IRuntimeProperty[D, Col] {
  def elementType(): Class[Elem];
}

trait IMapProperty[D, Col, Key, Val] extends IRuntimeProperty[D, Col] {
  def keyType(): Class[Key];
  def valueType(): Class[Val];
}

case class RuntimeProperty[D, R](val meta: PropertyModel, val getter: Function1[D, R], val setter: Function2[D, R, Unit], val mRange: Class[R])
  extends IRuntimeProperty[D, R] {
  def range() = mRange;
};

case class ReflectionGetter[D](val method: Method) extends Function1[D, Object] {
  def apply(o: D): Object = method.invoke(o);
}
case class ReflectionSetter[D](val method: Method) extends Function2[D, Object, Unit] {
  def apply(o: D, v: Object): Unit = {
    method.invoke(o, v)
  };
}

case class ModelledRuntimeProperty[D, R](model: PropertyModel) extends IRuntimeProperty[D, R] {
  import RuntimeImplicits._;

  val actual: RuntimeProperty[D, R] = RuntimeProperty(model.domain, model.name()).asInstanceOf[RuntimeProperty[D, R]];

  def meta(): PropertyModel = model;

  def getter(): Function1[D, R] = actual.getter;

  def setter(): Function2[D, R, Unit] = actual.setter;

  def range(): Class[R] = {
    val v: Class[_] = model.range();
    return v.asInstanceOf[Class[R]];
  }
}
//TODO see how it stacks with build in types
case class OnRuntimeIs(model: Type, clazz: Class[_]) extends Entity[OnRuntimeIs] with FactAnnotation {
  import RuntimeImplicits._;
  register(model, this);
}
case class IsDescribedIn(model: ModelType[_ <: ModelType[_]], buildIn: BuiltInType[_]) extends Entity[IsDescribedIn] with OneValueFact {
  register(buildIn, this);
}

case class MapsTo(first: Type, another: Type) extends Entity[IsDescribedIn] with OneValueFact {
  register(first, this);
}

object RuntimeImplicits {
  implicit def propToRuntime(p: PropertyModel): IRuntimeProperty[_, _] = ModelledRuntimeProperty(p);
  implicit def typeToRuntime(p: Type): Class[_] = {
    val set = Entity.about(p, classOf[OnRuntimeIs]);
    if (set.size == 1) {
      return set.toSeq(0).clazz;
    }
    return null;
  }
  implicit def classToType(c: Class[_]): Type = {
    //I wish ModelType would be a case class
    return BuiltInType(c);
  }
}

class RuntimeMeta[D, R](val pn: String, dC: Class[D], rC: Class[R]) extends Property[BuiltInType[D], BuiltInType[R]] {
  def domain(): BuiltInType[D] = dC;

  def name(): String = pn;

  def range(): BuiltInType[R] = rC;

  def withName(name: String): Property[BuiltInType[D], BuiltInType[R]] = {
    throw new UnsupportedOperationException();
  }
}

object RuntimeProperty {
  def apply[D](c: Class[D], n: String): RuntimeProperty[D, Object] = {
    var getMethod: Method = null;
    try {
      getMethod = c.getMethod(n);
    } catch {
      case t: NoSuchMethodException => {
        try {
          getMethod = c.getMethod("get" + n.charAt(0).toUpper + n.substring(1));
        } catch {
          case t: NoSuchMethodException => {
            getMethod = c.getMethod("is" + n.charAt(0).toUpper + n.substring(1));
          }
        }
      }
    }
    var setMethod: Method = null;
    var pC = getMethod.getReturnType;
    try {
      setMethod = c.getMethod(n + "_$eq", pC);
    } catch {
      case t: NoSuchMethodException => {
        try {
          setMethod = c.getMethod("set" + n.charAt(0).toUpper + n.substring(1), getMethod.getReturnType);
        } catch {
          case t: NoSuchMethodException => {

          }
        }
      }
    }
    return new RuntimeProperty(buildModel(getMethod, setMethod), ReflectionGetter(getMethod),if (setMethod!=null)ReflectionSetter(setMethod)else null, pC.asInstanceOf[Class[Object]]);
  }
  def buildModel(g: Method, s: Method): PropertyModel = {
    var name = g.getName;
    if (name.startsWith("get")) {
      name = name.charAt(4).toLower + name.substring(5);
    } else if (g.getName.startsWith("is")) {
      name = name.charAt(3).toLower + name.substring(4);
    }
    return new RuntimeMeta(name, g.getDeclaringClass, g.getReturnType);
  }
}

class CollectionProperty[D, Col, Elem](val meta: PropertyModel, val getter: Function1[D, Col], val setter: Function2[D, Col, Unit], val mRange: Class[Col], val mElem: Class[Elem])
  extends ICollectionProperty[D, Col, Elem] {
  def range() = mRange;
  def elementType() = mElem;
}
package org.mms.core.runtime;

import org.mms.core.Property
import org.mms.core._
import java.lang.reflect.Method
import scala.collection.TraversableLike
import org.mms.core.codemodel.SourceUnit

trait IRuntimeProperty[D, R] {
  def meta(): PropertyModel;
  def getter(): Function1[D, R];
  def setter(): Function2[D, R, Unit];
  def range(): Class[R]
  def get(d: D): R = { getter()(d) };

  def remove(b: D, v: R): Boolean = {
    if (!readOnly) {
      val vq = get(b);
      if (vq.isInstanceOf[TraversableLike[_, _]]) {
        val z = vq.asInstanceOf[TraversableLike[_, _]];
        val newValue = z.filter { x => x != v };
        set(b, newValue.asInstanceOf[R]);
      }
      return true;
    }
    return false;
  }
  def add(b: D, v: R): Boolean = {
    if (!readOnly) {
      val vq = get(b);
      if (vq.isInstanceOf[List[_]]) {
        if (v==null){
          return true;
        }
        val z = vq.asInstanceOf[List[_]];
        val newValue = z.::(v);
        set(b, newValue.asInstanceOf[R]);
      }
      return true;
    }
    return false;
  }
  def set(d: D, r1: R) {
    var r = r1;
    
    val interceptors = meta().about(classOf[ModifyInterceptor]);
    for (i <- interceptors) {
      r = i.beforeModify(this, d, r1).asInstanceOf[R];
    }
    if (!readOnly) {
      setter()(d, r);
    } else {
      val kv = meta.about(classOf[KnowsWayToModify]);
      for (q <- kv) {
        val options = q.howToChange(meta);
        if (options != null) {
          for (option <- options) {
            if (tryExecute(option, d, r, get(d))) {
              return ;
            }
          }
        }
      }
      println("attempt to modify readonly property:" + meta.name() + " of " + meta.domain())
    }
  };

  private def determineProp(p: PropertyModel): Tuple2[PropertyModel, PropertyModel] = {
    var x = p.withoutDecorators();
    if (x.isInstanceOf[SubProp[_ <: Type, _ <: Type]]) {
      val sp = x.asInstanceOf[SubProp[_ <: Type, _ <: Type]];
      val cand = sp.childProperty();
      return (cand, sp.parent);
    }
    return (p,null.asInstanceOf[PropertyModel]);
  }
  private def getActualValue(v: ValueModel, base: D, newValue: R, oldValue: R): Any = {
    v match {
      case ThisValue => base;
      case OldValue => oldValue;
      case NewValue => newValue;
    }
  }

  private def tryExecute(w: WayToChange, base: D, newValue: R, oldValue: R): Boolean = {
    //TODO EXPAND IT
    for (op <- w.ops) {
      op match {
        case Remove(p, b, v) => {
          val c = determineProp(p);
          if (c != null) {
            var baseValue = getActualValue(b, base, newValue, oldValue);
            var vValue = getActualValue(v, base, newValue, oldValue);
            if (!executeRemove(baseValue, vValue, c._1)) {
              return false;
            }
          }
        }
        case Add(p, b, v) => {
          val c = determineProp(p);
          if (c != null) {
            var baseValue = getActualValue(b, base, newValue, oldValue);
            var vValue = getActualValue(v, base, newValue, oldValue);
            if (!executeAdd(baseValue, vValue, c._1)) {
              return false;
            }
          }
        }
        case SetValue(p, v) => {

        }
      }

    }
    return true;
  }

  private def executeRemove(base: Any, value: Any, p: PropertyModel): Boolean = {
    if (base != null) {
      val runtimeModel: IRuntimeProperty[Any, Any] = RuntimeImplicits.propToRuntime(p).asInstanceOf[IRuntimeProperty[Any, Any]];
      return runtimeModel.remove(base, value);
    }
    return true;
  }
  private def executeAdd(base: Any, value: Any, p: PropertyModel): Boolean = {
    if (base != null) {
      val runtimeModel: IRuntimeProperty[Any, Any] = RuntimeImplicits.propToRuntime(p).asInstanceOf[IRuntimeProperty[Any, Any]];
      return runtimeModel.add(base, value);
    }
    return true;
  }

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
case class SubPropertyOnRuntime[D, R >: Null, A](val p: IRuntimeProperty[D, A], s: IRuntimeProperty[A, R], meta: PropertyModel) extends IRuntimeProperty[D, R] {

  def getter(): Function1[D, R] = new Function1[D, R] {

    def apply(a: D): R = {
      val x = p.get(a);
      if (x == null) {
        return null;
      }
      return s.get(x);
    }
  };
  def setter(): Function2[D, R, Unit] = new Function2[D, R, Unit]() {
    def apply(a: D, v: R): Unit = {
      var x = p.get(a);

      if (x == null) {
        //FIXME
        x = p.range().newInstance();
        p.set(a, x);
      }
      s.set(x, v);
    }
  }
  def range() = s.range();
}
case class ModelledRuntimeProperty[D, R >: Null](model: PropertyModel) extends IRuntimeProperty[D, R] {
  import RuntimeImplicits._;

  val actual: IRuntimeProperty[D, R] =
    {
      if (model.isInstanceOf[SubProp[_, _]]) {

        val x = RuntimeProperty(model.domain, model.name()).asInstanceOf[RuntimeProperty[D, R]];
        val parent = ModelledRuntimeProperty(model.asInstanceOf[SubProp[_, _]].parent);

        val x1 = x.asInstanceOf[IRuntimeProperty[Any, R]];
        val p1 = parent.asInstanceOf[IRuntimeProperty[D, Any]];
        SubPropertyOnRuntime[D, R, Any](p1, x1, model);
      } else {
        RuntimeProperty(model.domain, model.name()).asInstanceOf[RuntimeProperty[D, R]];
      }
    }

  def meta(): PropertyModel = model;

  def getter(): Function1[D, R] ={
    val orginal=actual.getter;
    val pr=model.range().fact(classOf[PretransformAssertion]);
    if (pr==null){
      return orginal;
    }
    val tp=pr.property;
    val az=RuntimeImplicits.propToRuntime(tp).getter().asInstanceOf[Function[Any,R]];
    return new Function1[D,R]{
      
      def apply(d:D):R={
        val value=orginal(d);
        if (value!=null){
          val tt=az(value);
          if (tt!=null){
          return tt;
          }
        }
        return value;
      }
    }
  }
  def setter(): Function2[D, R, Unit] = actual.setter.asInstanceOf[Function2[D, R, Unit]];

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
    val set = p.about(classOf[OnRuntimeIs]);
    if (set.size == 1) {
      return set.toSeq(0).clazz;
    }
    return null;
  }
  implicit def classToType(c: Class[_]): Type = {
    //I wish ModelType would be a case class
    var z=BuiltInType(c);
    val x=z.toModelIfPossible();
    return x;
  }
}

class RuntimeMeta[D, R](val pn: String, dC: Class[D], rC: Class[R]) extends Property[BuiltInType[D], BuiltInType[R]] {
  def domain(): BuiltInType[D] = dC;

  def name(): String = pn;

  def range(): BuiltInType[R] = rC;
  
  override protected def init() {
    
  }

  override def withName(name: String): Property[BuiltInType[D], BuiltInType[R]] = {
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
    return new RuntimeProperty(buildModel(getMethod, setMethod), ReflectionGetter(getMethod), if (setMethod != null) ReflectionSetter(setMethod) else null, pC.asInstanceOf[Class[Object]]);
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

object KeyUtils {

  def getKeyProperty(clazz: Type): PropertyModel = {
    val v = clazz.properties().find { x => x.isKey };
    if (v.isDefined) {
      return v.get;
    }
    return null;
  }

  
  def getValue(pm: PropertyModel, b: Any): Any = {
    if (pm == null||b==null) {
      return null;
    }
    val pr: IRuntimeProperty[Any, Any] = RuntimeImplicits.propToRuntime(pm).asInstanceOf[IRuntimeProperty[Any, Any]];
    if (pr != null) {
      return pr.get(b);
    }
    return null;
  }

  def globalKey(obj: Any, clazz: Type,pp:PropertyModel): Any = {
    val kv = getValue(getKeyProperty(clazz), obj);
    if (kv != null) {
      val m = getValue(pp, obj);
      return new Tuple2(kv, m);
    }
    return pp;    
  }
}

class ModifyInterceptor(val pm: PropertyModel) extends FactAnnotation {
  def beforeModify(pr: IRuntimeProperty[_, _], base: Any, value: Any): Any = value;
  def afterModify(pr: IRuntimeProperty[_, _], base: Any, value: Any): Unit = {};
} 
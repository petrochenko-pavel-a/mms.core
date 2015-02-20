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
  def set(d: D, r: R) { setter()(d, r); };
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
  def apply(o: D, v: Object): Unit = method.invoke(o, v);
}
class RuntimeMeta[D,R](val pn:String,dC:Class[D],rC:Class[R]) extends Property[BuiltInType[D],BuiltInType[R]] {
  def domain(): BuiltInType[D] = dC;

  def name(): String = pn;

  def range(): BuiltInType[R] =rC;
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
    var pC=getMethod.getReturnType;
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
    return new RuntimeProperty(buildModel(getMethod,setMethod),ReflectionGetter(getMethod),ReflectionSetter(setMethod),pC.asInstanceOf[Class[Object]]);
  }
  def buildModel(g:Method,s:Method):PropertyModel={
     var name=g.getName;
     if (name.startsWith("get")){
       name=name.charAt(4).toLower+name.substring(5);
     }
     else if (g.getName.startsWith("is")){
       name=name.charAt(3).toLower+name.substring(4);
     }
     return new RuntimeMeta(name,g.getDeclaringClass,g.getReturnType);
  }
}

class CollectionProperty[D, Col, Elem](val meta: PropertyModel, val getter: Function1[D, Col], val setter: Function2[D, Col, Unit], val mRange: Class[Col], val mElem: Class[Elem])
  extends ICollectionProperty[D, Col, Elem] {
  def range() = mRange;
  def elementType() = mElem;
}
package org.mms.core

import scala.collection.mutable.HashMap
import java.lang.reflect.Field
import org.mms.core.codemodel.IType

trait Type extends Entity[Type] {
  def superType: Type
  def typeName: String = {
   var s=getClass().getSimpleName();
   if (s.endsWith("$")){
     return s.substring(0,s.length()-1);
   }
   return s;
  }
  
  override def toString()=typeName;
}
class ModelType[T<:ModelType[_]](val superType: Type = null) extends Type {

  type UnknownProperty=Property[ModelType[_],_<:Type];
  def str = new Prop(this, StrType);
  def int = new Prop(this, StrType);
  def propOf[T<:Type](t:T) = new Prop(this, t);
  def propOf[T](t:Class[T]) = new Prop(this, BuiltInType(t));
  def packageName:String=getClass.getPackage.getName;
  
  
  
  private[core] class MetaInf {
    val fToPropMap: HashMap[Field,UnknownProperty] = HashMap();
    val pToFieldMap: HashMap[UnknownProperty, Field] = HashMap();

    
    initFieldMap();
    
    private def initFieldMap() = {
      for (f <- ModelType.this.getClass.getDeclaredFields) {
        f.setAccessible(true);
        val p = f.get(ModelType.this);
        if (p.isInstanceOf[Property[_,_]]){
          
          fToPropMap.put(f, p.asInstanceOf[UnknownProperty]);
          pToFieldMap.put(p.asInstanceOf[UnknownProperty], f);
        }
      }
    }
  }
  protected[core] lazy val metainf = new MetaInf;
  def properties():List[Property[ModelType[_],_<:Type]]=metainf.fToPropMap.values.toList;
  
}

case class BuiltInType[T](val builtIn: Class[T]) extends Type with IType {
  def superType = new BuiltInType(builtIn.getSuperclass);
  override def typeName: String = builtIn.getSimpleName();
  
  def fullName(): String=builtIn.getName;
}

object BuiltInType{
  implicit def toClass[T](x: BuiltInType[T]):Class[T] = x.builtIn;
  implicit def fromClass[T](x: Class[T]):BuiltInType[T] = new BuiltInType(x);
    
}

object StrType extends BuiltInType(classOf[String]);
object IntType extends BuiltInType(classOf[Integer]);
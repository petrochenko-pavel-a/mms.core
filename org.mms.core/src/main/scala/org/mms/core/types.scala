package org.mms.core

import scala.collection.mutable.HashMap
import java.lang.reflect.Field
import org.mms.core.codemodel.IType
import org.mms.core.runtime.OnRuntimeIs
import org.mms.core.runtime.OnRuntimeIs
import org.mms.core.runtime.RuntimeProperty
import org.mms.core.runtime.IsDescribedIn
import org.mms.core.runtime.IsDescribedIn

trait Type extends Entity[Type] {
  def superType: Type
  def typeName: String = {
   var s=getClass().getSimpleName();
   if (s.endsWith("$")){
     return s.substring(0,s.length()-1);
   }
   return s;
  }
  
  def interfaces():Seq[Type];
  
  def directSubClasses():Set[Type]={
    about(classOf[SubClassOf]).map { x => x.subClass };
  }
  def allSubClasses():Set[Type]={
    val v:Set[Type]=about(classOf[SubClassOf]).map { x => x.subClass };
    var m:Set[Type]=Set();
    for (t<-v){
      m=m++t.allSubClasses()+t;
    }
    return m;
  }
  
  def isAssignableFrom(t:Type):Boolean={
    if (this==t){
      return true;
    }
    if (t.superType!=null){
        if (isAssignableFrom(t.superType)){
          return true;
        }
    }
    if (t.interfaces()!=null){
        for (z<-t.interfaces()){
          if (isAssignableFrom(z)){
            return true;
          }
        }
    }
    return false;
  }
  override def toString()=typeName;
}
case class SubClassOf(val superClass:Type,val subClass:Type) extends FactAnnotation;

case class withTrait(val elements:Type*){}

object NothingType extends ModelType{
  override def interfaces():Seq[Type]=List();
};
class ModelType[T<:ModelType[_]](val superType: Type = null,val withInterfaces:withTrait=withTrait()) extends Type {

  {
    if (superType!=null){
      register(superType, SubClassOf(superType,this));
    }
    for(v<-withInterfaces.elements){
      register(v, SubClassOf(v,this));
    }
  }
  
  type UnknownProperty=Property[ModelType[_],_<:Type];
  protected def str = new Prop(this, StrType);
  protected def int = new Prop(this, StrType);
  def interfaces()=withInterfaces.elements;
  protected def propOf[T<:Type](t:T) = new Prop(this, t);
  protected def list[T<:Type](t:Property[_<:ModelType[_],T]):Property[_<:ModelType[_],T] =ListProp(t);
  
  protected def propOf[T](t:Class[T]) = new Prop(this, BuiltInType(t));
  protected def packageName:String=getClass.getPackage.getName;
  
  val NOTHING=new Prop(this,NothingType){ override def  name()="NOTHING"};
  
  def <=>(c:Class[_])={
    OnRuntimeIs(this,c);
    IsDescribedIn(this,c);
  }
  def <=>(c:Type)={
    
  }
  
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
  
  def superType = if (builtIn.getSuperclass!=null)new BuiltInType(builtIn.getSuperclass) else null;
  
  def interfaces():Seq[Type]={
    val q=builtIn.getInterfaces;
    if (q!=null){
      return q.map { x => BuiltInType(x) };
    }
    return List[Type]();
  }
  
  override def typeName: String = builtIn.getSimpleName();
  {OnRuntimeIs(this,builtIn)}
  def fullName(): String=builtIn.getName;
  
  override def about[T<:FactAnnotation](t:Class[T]):Set[T]={
    val description=super.directStatementsAboutThis(classOf[IsDescribedIn]);
    if (description!=null&&description.size==1){
      return description.toList(0).model.about(t);
    }
    if (t==classOf[isPropertyOf[_]]){
       var pr=Set[isPropertyOf[_]]();
       val pm=builtIn.getMethods.filter { x => x.getParameterTypes.length==0&&x.getReturnType!=Void.TYPE };
       for (p<-pm){
         var q=isPropertyOf(this,RuntimeProperty(builtIn,p.getName).meta);
         pr=pr+q;  
       }
       return pr.asInstanceOf[Set[T]];
    }
    return super.about(t);    
  }
  
}

object BuiltInType{
  implicit def toClass[T](x: BuiltInType[T]):Class[T] = x.builtIn;
  implicit def fromClass[T](x: Class[T]):BuiltInType[T] = new BuiltInType(x);
}

object StrType extends BuiltInType(classOf[String]);
object IntType extends BuiltInType(classOf[Integer]);
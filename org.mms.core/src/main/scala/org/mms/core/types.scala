package org.mms.core

import scala.collection.mutable.HashMap
import java.lang.reflect.Field
import org.mms.core.codemodel.IType
import org.mms.core.runtime.OnRuntimeIs
import org.mms.core.runtime.RuntimeProperty
import org.mms.core.runtime.IsDescribedIn
import java.lang.reflect.Modifier
import org.mms.core.runtime.MapsTo

trait Type extends Entity[Type] {
  def superType: Type
  def typeName: String = {
   var s=getClass().getSimpleName();
   if (s.endsWith("$")){
     return s.substring(0,s.length()-1);
   }
   return s;
  }
  def toModelIfPossible()=if (modelType()!=null)modelType()else this;
  def modelType():ModelType[_];
  
  def interfaces():Seq[Type];
  def isAbstract():Boolean;
  
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
  
  private def pMap():Map[String,PropertyModel]={
    var ps:Map[String,Property[_<:Type,_<:Type]]=about(classOf[isPropertyOf[_]]).map { x => (x.p.name(),x.p )}.toMap;
    if (superType!=null){
      val superProps=superType.pMap();
      for (p<-superProps){
        val sp:PropertyModel=p._2;
        if (ps.contains(p._1)){
          val cp:PropertyModel=ps.get(p._1).get;
          if (!sp.range().isAssignableFrom(cp.range())){
            throw new IllegalStateException(s" incorrectly $cp overrides "+sp)
          }
          val op=(p._1,OverrideProperty(cp,sp));
          ps+=op.asInstanceOf[Tuple2[String,PropertyModel]];
        }
        else{
          ps+=p;
        }
      }      
    }
    return ps;
  }
  
  def properties():Set[PropertyModel]={
    return pMap().values.toSet;
  } 

  
}

private case class OverrideProperty[DomainType<:Type,RangeType<:Type](val c:Property[_<:Type,_<:Type],val s:Property[_<:Type,_<:Type]) extends Property[DomainType,RangeType]{
   def range():RangeType=c.range().asInstanceOf[RangeType];
   def domain():DomainType=c.domain().asInstanceOf[DomainType];
   def name():String=c.name();
   
   protected override def init(){
     
   }
   override def about[T<:FactAnnotation](t:Class[T]):scala.collection.immutable.Set[T]={
    return c.about(t)++s.about(t);
   }
   def withName(name:String):Property[DomainType,RangeType]={throw new UnsupportedOperationException()}
   override def toString():String=c.toString();      
}
case class SubClassOf(val superClass:Type,val subClass:Type) extends FactAnnotation;

case class withTrait(val elements:Type*){}

object NothingType extends ModelType {
  override def interfaces():Seq[Type]=List();

  override def isAbstract(): Boolean = true;
  
  
};
class ModelType[T<:ModelType[_]](val superType: Type = null,val withInterfaces:withTrait=withTrait()) extends Type {
  
  def modelType():ModelType[_]=this;
  
  private var _abstract=false;
  {
    if (superType!=null){
      register(superType, SubClassOf(superType,this));
    }
    for(v<-withInterfaces.elements){
      register(v, SubClassOf(v,this));
    }
  }
  
  def isAbstract():Boolean=_abstract;
  
  protected def abstractType{_abstract=true}
  
  type UnknownProperty=Property[ModelType[_],_<:Type];
  protected def str = new Prop(this, StrType);
  protected def int = new Prop(this, StrType);
  def interfaces()=withInterfaces.elements;
  protected def propOf[T<:Type](t:T) = new Prop(this, t);
  
  protected def optional[T<:Type](t:Property[_<:ModelType[_],T]):Property[_<:ModelType[_],T] =t;
  
  
  protected def required[T<:Type](t:Property[_<:ModelType[_],T]):Property[_<:ModelType[_],T] =t;
  protected def list[T<:Type](t:Property[_<:ModelType[_],T]):Property[_<:ModelType[_],T] =ListProp(t);
  protected def propOf[T](t:Class[T]) = new Prop(this, BuiltInType(t));
  protected def packageName:String=getClass.getPackage.getName;
  
  val NOTHING=new Prop(this,NothingType){ override def  name()="NOTHING"};
  
  def <=>(c:Class[_])={
    OnRuntimeIs(this,c);
    IsDescribedIn(this,c);
    MapsTo(this,BuiltInType(c));
  }
  
  def <=>(c:Type)={
    MapsTo(this,c);
    MapsTo(c,this);
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
  def declaredProperties():List[Property[ModelType[_],_<:Type]]=metainf.fToPropMap.values.toList;
  
}
class AbstractType[T<:ModelType[_]](override val superType: Type = null,override val withInterfaces:withTrait=withTrait()) extends ModelType[T](superType,withInterfaces) {
  abstractType;
}

case class BuiltInType[T](val builtIn: Class[T]) extends Type with IType {
  
  def superType:Type = 
  {
    if (modelType()!=null){
      return modelType().superType;
    }
    if (builtIn.getSuperclass!=null)new BuiltInType(builtIn.getSuperclass) else null;
  }
  
  def modelType():ModelType[_]={
    val description=super.directStatementsAboutThis(classOf[IsDescribedIn]);
    if (description!=null&&description.size==1){
      return description.toList(0).model;
    }
    return null;
  }
  
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
    if (modelType()!=null){
      return modelType().about(t);
    }    
    if (t==classOf[isPropertyOf[_]]){
       var pr=Set[isPropertyOf[_]]();
       val pm=builtIn.getDeclaredMethods.filter { x => x.getParameterTypes.length==0&&x.getReturnType!=Void.TYPE &&Modifier.isPublic(x.getModifiers)&&(!Modifier.isStatic(x.getModifiers))};
       for (p<-pm){
         var q=isPropertyOf(this,RuntimeProperty(builtIn,p.getName).meta);
         pr=pr+q;  
       }
       return pr.asInstanceOf[Set[T]];
    }
    return super.about(t);    
  }

  def isAbstract(): Boolean = {
    return Modifier.isAbstract(builtIn.getModifiers)||Modifier.isInterface(builtIn.getModifiers);
  }
  
}

object BuiltInType{
  implicit def toClass[T](x: BuiltInType[T]):Class[T] = x.builtIn;
  implicit def fromClass[T](x: Class[T]):BuiltInType[T] = new BuiltInType(x);
}

object StrType extends BuiltInType(classOf[String]);
object IntType extends BuiltInType(classOf[Integer]);
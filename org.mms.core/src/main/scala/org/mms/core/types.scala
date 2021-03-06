package org.mms.core

import scala.collection.mutable.HashMap
import java.lang.reflect.Field
import org.mms.core.codemodel.IType
import org.mms.core.runtime.OnRuntimeIs
import org.mms.core.runtime.RuntimeProperty
import org.mms.core.runtime.IsDescribedIn
import java.lang.reflect.Modifier
import org.mms.core.runtime.MapsTo
import java.util.ArrayList
import java.util.Arrays.ArrayList

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
  
  def property(name:String):PropertyModel=pMap()(name);

  
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
  
  
  protected[core] var index=0;
  
  private var _abstract=false;
  {
    if (superType!=null){
      register(superType, SubClassOf(superType,this));
    }
    for(v<-withInterfaces.elements){
      register(v, SubClassOf(v,this));
    }
  }
  
  override def hashCode():Int={
    return typeName.hashCode()+packageName.hashCode();
  };
  override def equals(v:Any):Boolean={
    if (v==null){
      return false;
    }
    if (v.getClass()==this.getClass){
      return true;
    }
    return false;
  }
  
  
  def isAbstract():Boolean=_abstract;
  
  protected def abstractType{_abstract=true}
  def pretransform (p:Property[this.type,_<:Type]): Unit={
    register(this,PretransformAssertion(this,p));
  }
  
  
  type UnknownProperty=Property[ModelType[_],_<:Type];
  protected def str = propOf(StrType);
  protected def int = propOf(StrType);
  protected def bool = propOf(classOf[Boolean]);
  def interfaces()=withInterfaces.elements;
  protected def propOf[T<:Type](t:T) :Property[this.type,T]= 
  {
    
    var v=new Prop[this.type,T](this, t);
    if(StackDetails.prop.get!=null){
      v=new SubProp[this.type,T](this, t,v,StackDetails.prop.get);      
    }
    v;
  }
  protected def listOf[T<:Type](t:T):Property[this.type,T] =list(propOf(t));
  protected def key[T<:Type](t:Property[this.type,T]):Property[this.type,T] ={t._key=true;t;}//TODO think is this good or not
  protected def computed[T<:Type](t:Property[this.type,T]):Property[this.type,T] ={t._computed=true;t;}//TODO think is this good or not
  
  protected def optional[T<:Type](t:Property[this.type,T]):Property[this.type,T] ={t._required=false;t;};
  protected def required[T<:Type](t:Property[this.type,T]):Property[this.type,T] ={t._required=true;t;}
  protected def list[T<:Type](t:Property[this.type,T]):Property[this.type,T] =ListProp(t);
  
  protected def propOf[T](t:Class[T]):Property[this.type,_<:BuiltInType[T]]  = new Prop(this, BuiltInType(t));
  
  def packageName:String=getClass.getPackage.getName;
  
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
  
  def <=>[A <: Type](p: EqToConstantAssertion): ConstantDescriminator = ConstantDescriminator(this, p);

  
  private var allProperties:ArrayList[Property[_,_]]=_;
  
  
  protected [core] def register(p:Property[_,_]):Unit={
    if (allProperties==null){
      allProperties=new ArrayList[Property[_,_]]();
    }
    allProperties.add(p);
  }
  protected [core] def getProperty(num:Int)=allProperties.get(num);
  
  
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
  
//  //TODO REMOVE IT
  protected[core] def getPropFromMetaInf(name:String):Property[this.type,_<:Type]={
    val q=getClass().getDeclaredField(name);
    return metainf.fToPropMap(q).asInstanceOf[Property[this.type,_<:Type]];
  }
  
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
class TypeUniverse(){
  private var _types=List[ModelType[_]]();
  
  def types():List[ModelType[_]]=_types;
  def add(t:ModelType[_])=_types=_types.::(t);
  def remove(t:ModelType[_])=_types=_types.filter { x => x!=t };
}

object BuiltInType{
  implicit def toClass[T](x: BuiltInType[T]):Class[T] = x.builtIn;
  implicit def fromClass[T](x: Class[T]):BuiltInType[T] = new BuiltInType(x);
}

object StrType extends BuiltInType(classOf[String]);
object IntType extends BuiltInType(classOf[Integer]);
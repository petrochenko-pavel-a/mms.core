package org.mms.core.codemodel;
import scala.collection.mutable.MutableList

trait IType {

  def fullName(): String;
}
case class ParameterizedType(baseType:IType,parameters:IType*) extends IType{
  
  def fullName()=baseType.fullName()
  
}


trait IModelElement[C <: IModelElement[_]] {
  def name: String;
  def parent: IModelElement[_];
  def children: List[C];
  def children_=(c: List[C]): Unit;

 

  def dependsFrom(): Set[String] = {
    var deps: Set[String] = ownDeps();
    for (i <- children) deps = deps ++ i.dependsFrom();
    return deps;
  }
  protected def ownDeps() = Set[String]();

  override def toString(): String = name;

  def findAllChildren[T <: IModelElement[_]](t: Class[T], f: Function1[T, Boolean] = { x: T => true }): Seq[T] = {
    var x = List[T]()
    visit {
      p =>
        if (t.isInstance(p) && f(p.asInstanceOf[T])) {
          x = x :+ p.asInstanceOf[T]
        }
    };
    return x;
  }

  def getAncestorOfKind[T](t: Class[T]): T = {
    if (t.isInstance(this)) {
      return this.asInstanceOf[T];
    }

    if (parent != null && parent.isInstanceOf[IModelElement[_]]) {
      return parent.asInstanceOf[IModelElement[_]].getAncestorOfKind(t);
    }
    return null.asInstanceOf[T];
  }

  def visit(x: Function1[IModelElement[_], Unit]): Unit = {
    children.foreach { z => x(z); z.visit(x) }
  }
}
trait IModel extends IModelElement[IPackage] {
  def findType(n: String): ISourceType = {
    var c = findAllChildren(classOf[ISourceType], { x: ISourceType => x.fullName() == n });
    if (!c.isEmpty) {
      return c(0);
    }
    return null;
  }

}
trait IPackage extends IModelElement[ISourceUnit]

trait HasPackage[T<:IModelElement[_]] extends IModelElement[T]{
 def parentPackage()=getAncestorOfKind(classOf[IPackage]);
}

trait ISourceUnit extends IModelElement[ISourceType] with HasPackage[ISourceType];

trait ISourceType extends IModelElement[IMember] with IType  with HasPackage[IMember]{

  def unit() = getAncestorOfKind(classOf[ISourceUnit]);
  
  def superClass: IType ;

  def superInterfaces: Set[String] ;

  def fullName(): String = {
    val p = parentPackage();
    if (p != null) {
      return p.name + "." + name;
    }
    return "<default>" + "." + name;
  }

};

trait IMember extends IModelElement[Null] with HasPackage[Null] {

  def isList:Boolean
  def elementsType(): IType
  def parentType() = getAncestorOfKind(classOf[ISourceType]);
}

class ModelElement[C <: IModelElement[_]] extends IModelElement[C] {
  var name: String = _;
  private var mParent: IModelElement[_] = null;
  private var mChildren: List[C] = List();

  def parent(): IModelElement[_] = mParent;

  def children(): List[C] = mChildren;

  def children_=(c: List[C]) {
    if (c==null){
      return;
    }
    if (this.isInstanceOf[CodeModel]){
      if (c==null||c.isEmpty){
        println(this)
      }
    }
    if (!mChildren.isEmpty) {
      for (ch <- mChildren) {
        if (ch.isInstanceOf[ModelElement[C]]){
          ch.asInstanceOf[ModelElement[C]].mParent = null;
        }
      }
    }
    for (ch <- c) {
      val q = ch.asInstanceOf[ModelElement[_]]
      if (q.mParent != this && q.mParent != null) {
        val sp = q.mParent.asInstanceOf[ModelElement[C]];
        sp.mChildren = sp.mChildren.filter { x => x != q }
      }
      q.mParent=this;
    }
    this.mChildren=c;
  }
}
class Package() extends ModelElement[ISourceUnit] with IPackage
class SourceUnit() extends ModelElement[ISourceType] with ISourceUnit
class SourceType() extends ModelElement[IMember] with ISourceType {

  var superClass: IType = _;

  var superInterfaces: Set[String] = Set();

  protected override def ownDeps(): Set[String] = {
    var l = Set[String]();
    if (superClass != null) {
      l += superClass.fullName();
    }
    if (superInterfaces != null) {
      l = l ++ superInterfaces;
    }
    return l;
  }
}

case class SourceMember() extends ModelElement[Null] with IMember {
  var elementsType: IType = _;
  
  var isReq : Boolean=false;
  var isList: Boolean=false;

  protected override def ownDeps(): Set[String] = {
    if (elementsType != null) {
      var l = Set[String](elementsType.fullName());
      if(isList){
        l=l+"java.util.List";
      }
      return l;
    }
   
    return Set();
  }
}
case class CodeModel() extends ModelElement[IPackage] with IModel{
  
}



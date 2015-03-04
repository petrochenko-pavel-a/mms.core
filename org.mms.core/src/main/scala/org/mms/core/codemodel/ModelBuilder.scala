import org.mms.core.TypeUniverse
import org.mms.core.codemodel.CodeModel
import org.mms.core.ModelType
import org.mms.core.codemodel.Package
import org.mms.core.codemodel.IPackage
import org.mms.core.codemodel.ISourceUnit
import org.mms.core.codemodel.SourceUnit
import org.mms.core.codemodel.ISourceUnit
import org.mms.core.codemodel.ISourceUnit
import org.mms.core.codemodel.ISourceUnit
import org.mms.core.codemodel.ISourceType
import org.mms.core.codemodel.SourceMember

class ModelBuilder{
  
  def convert(u:TypeUniverse):CodeModel={
    val mdl=new CodeModel();
    for (t<-u.types()){
      appendType(mdl,t);
    }
    return mdl;
  }
  
  def getOrCreateMember(){
    
  }
  def appendType(mdl:CodeModel,t:ModelType[_]){
     val p=getOrCreatePackage(mdl,t.packageName);
     val u=getOrCreateUnit(p,t);
     val st=getOrCreateType(u,t);
     for (prop<-t.properties()){
       val memb=new SourceMember()
       memb.name=prop.name();
       st.children=st.children.::(memb);
       //todo configure type;
     }
  }
  def getOrCreateType(unit:ISourceUnit,t:ModelType[_]):ISourceType={
    val op=unit.children.find { x => x.name==t.typeName }
    if (op.isDefined){
      return op.get;
    }
    return null;
  }
  def getOrCreateUnit(pack:IPackage,t:ModelType[_]):ISourceUnit={
    val ch=pack.children;
    val op=ch.find { x => x.name==t.typeName }
    if (op.isDefined){
      return op.get;
    }
    val p=new SourceUnit();
    p.name=t.typeName;
    pack.children=pack.children.::(p);
    return p;
  }

  def getOrCreatePackage(mdl:CodeModel,pn: String):IPackage = {
    val op=mdl.children().find { x => x.name==pn }
    if (op.isDefined){
      return op.get;
    }
    val p=new Package();
    p.name=pn;
    mdl.children_=(mdl.children().::(p));
    return p;
  }
}
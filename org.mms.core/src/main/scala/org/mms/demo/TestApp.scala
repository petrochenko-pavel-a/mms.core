package org.mms.demo
import org.mms.core.Type
import org.mms.core.Entity
import org.mms.core.ModelType
import org.mms.core.codemodel.CodeModel
import org.mms.core.codemodel.SourceType
import org.mms.core.Prop
import org.mms.core.Prop


object Person extends ModelType() {
  val name = str;
  val lastName = str;
  val age = int;
}

object XXX{
  val xxA:String="AA";
}

object TestApp extends App {
  
   
  var m:CodeModel=new CodeModel();
  
  print(XXX.xxA);
  var t=new SourceType();
  t.name="Hello";
  println(t)
  val z=Class.forName("org.mms.demo.XXX$").getDeclaredConstructor();
  z.setAccessible(true);
  val ma=z.newInstance();
  val mm=ma.getClass.getMethod("xxA").invoke(ma);
  println(mm)
  
  /*var s = CodeWriter("C:\\work\\jaxrs-raml-converters\\org.mms.test\\src\\");
  var u=new ScalaUniverse();
  var sa:BuiltInType[_]=classOf[String];*/
  //new CodeGen().contribute(Person, u);
  //u.flush(s);
}
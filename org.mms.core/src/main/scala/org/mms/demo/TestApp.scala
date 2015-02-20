package org.mms.demo
import org.mms.core.Type
import org.mms.core.Prop
import org.mms.core.Entity
import org.mms.core.ModelType
import org.mms.core.codemodel.CodeModel
import org.mms.core.codemodel.SourceType


object Person extends ModelType() {
  val name = str;
  val lastName = str;
  val age = int;
}

object TestApp extends App {
  
   
  var m:CodeModel=new CodeModel();
  
  
  var t=new SourceType();
  t.name="Hello";
  
  println(t)
  /*var s = CodeWriter("C:\\work\\jaxrs-raml-converters\\org.mms.test\\src\\");
  var u=new ScalaUniverse();
  var sa:BuiltInType[_]=classOf[String];*/
  //new CodeGen().contribute(Person, u);
  //u.flush(s);
}
package org.mms.core.codegen;

import org.mms.core.codemodel.ISourceUnit
import java.io.File
import java.io.FileOutputStream
import org.mms.core.codemodel.CodeModel
import java.io.BufferedWriter
import java.io.StringWriter
import java.io.PrintWriter
import org.mms.core.codemodel.ISourceType
import org.mms.core.codemodel.IMember

class FileSystemUnitWriter(val rootFile: File, unitExtension: String = ".java") extends UnitWriter {
  def write(unit: ISourceUnit, contents: String): Unit = {
    val nm = unit.parentPackage().name;
    var fl = rootFile
    for (segment <- nm.split('.')) {
      fl = new File(fl, segment);
    }
    fl.mkdirs();
    val file = new File(fl, unit.name + unitExtension);
    val bytes = contents.getBytes("UTF-8");
    val fs = new FileOutputStream(file);
    try {
      fs.write(bytes);
    } finally {
      fs.close;
    }
  }
}
trait UnitWriter {
  def write(unit: ISourceUnit, contents: String);
}

abstract class CodeGen(writer: UnitWriter) {

  def doGenerate(model: CodeModel) {
    for (unit <- model.findAllChildren(classOf[ISourceUnit])) {
      val content = buildContent(unit);
      writer.write(unit, content);
    }
  }

  protected def buildContent(unit: ISourceUnit): String
}

abstract class JavaLikeCodeGen(writer: UnitWriter) extends CodeGen(writer) {

  class AutoIndentingWriter {

    val sw = new StringWriter()
    val writer = new PrintWriter(new BufferedWriter(sw));
    var indentLevel: Int = 0;
    var intentString: String = "";

    def println(str: String): Unit = {
      writer.print(intentString);
      var Idelta = 0;
      for (c <- str) {
        if (c == '{') {
          Idelta = Idelta + 1;
        }
        if (c == '{') {
          Idelta = Idelta - 1;
        }
      }
      if (Idelta != 0) {
        indentLevel = indentLevel + Idelta;
        val q = new StringBuilder();
        for (i <- Range(0, indentLevel)) {
          q.append("    ");
        }
        intentString = q.toString;
      }
      writer.print(str.trim());
      writer.println();
    }

    override def toString() = sw.toString();
  }

  override def buildContent(unit: ISourceUnit): String = {
    val sw = new AutoIndentingWriter();
    sw.println(s"package ${unit.parent.name};")
    for (x <- unit.dependsFrom()) {
      if (x.indexOf('.') != (-1)) {
        var pn = x.substring(0, x.lastIndexOf('.'));
        if ((pn != "java.lang")&&(pn!=unit.parentPackage().name)) {
          sw.println(s"import $x;")
        }
      }
    }
    for (t <- unit.children) {
      printType(sw, t)
    }
    sw.writer.close();
    return sw.toString();
  }

  def printType(sw: AutoIndentingWriter, t: ISourceType): Unit = {
    printTypeDeclarationStart(sw, t);
    for (m <- t.children) {
      printPropertyDeclaration(sw, m);
    }
    printTypeDeclarationEnd(sw, t);
  }
  def printTypeDeclarationStart(sw: AutoIndentingWriter, t: ISourceType): Unit;
  def printPropertyDeclaration(sw: AutoIndentingWriter, t: IMember): Unit;
  def printTypeDeclarationEnd(sw: AutoIndentingWriter, t: ISourceType): Unit;
}

class SimpleJavaPOJOCodeGen(writer: UnitWriter) extends JavaLikeCodeGen(writer) {
  def printPropertyDeclaration(sw: AutoIndentingWriter, t: IMember): Unit = {
    sw.println("");
    sw.println("private " + getTypeString(t) + " " + t.name + ";"); //FIXME
    sw.println("");
    sw.println("public " + getTypeString(t) + " get" + t.name.charAt(0).toUpper+t.name.substring(1) + "(){return "+t.name+";}"); //FIXME
    sw.println("public " + getTypeString(t) + " set" + t.name.charAt(0).toUpper+t.name.substring(1) + "("+getTypeString(t)+s" newValue) {${t.name}=newValue; return "+t.name+";}"); //FIXME
  }

  def getTypeString(m: IMember): String = {
    if (m.isList) {
      return "List<" + m.elementsType() + ">"
    }
    return m.elementsType() + "";
  }

  def printTypeDeclarationEnd(sw: AutoIndentingWriter, t: ISourceType): Unit = {
    sw.println("");
    sw.println("}");
  }

  def printTypeDeclarationStart(sw: AutoIndentingWriter, t: ISourceType): Unit = {
    var extendsPiece = "";
    if (t.superClass != null) {
      if (t.name=="SourceTypeModel"){
        extendsPiece += "extends " + "ITypeModel"
        
      }
      else
      extendsPiece += "extends " + t.name
    }
    if (t.superInterfaces != null && (!t.superInterfaces.isEmpty)) {
      extendsPiece += "implements ";
      var notFirst = false;
      for (i <- t.superInterfaces) {
        if (notFirst) extendsPiece += ", ";
        extendsPiece += i;
        notFirst = true;
      }
    }
    val decl = s"class ${t.name} ${extendsPiece} {";
    sw.println(decl);
  }
}
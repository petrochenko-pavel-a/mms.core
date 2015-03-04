package org.mms.demo;

class PPP{
  val z=System.currentTimeMillis();
}

object X extends Cloneable{

  println("Creation")

  val x=new PPP();
  
  def copy():X.type=super.clone().asInstanceOf[X.type];
  
}

object Test extends App(){
  val m=X.copy();
  val m1=X;
  println(m+":"+m1)
}
package org.mms.patterns



object PatternDescriptions{

  trait patternAPIMember;
  
  trait collectionMember;
  
  sealed trait HTTPMethod{};
  object PUT extends HTTPMethod;
  object GET extends HTTPMethod;
  object POST extends HTTPMethod;
  object DELETE extends HTTPMethod;
  
  trait collectionAPIMember extends patternAPIMember{
    def secondary(name:String)=new collectionAPIMember{};  
  }
  def list(res:String,m:HTTPMethod=null)=new collectionMember{};
  def action(res:String,m:HTTPMethod=null)=new collectionMember{};
  def update(res:String,m:HTTPMethod=null)=new collectionMember{};
  def delete(res:String,m:HTTPMethod=null)=new collectionMember{};
  def create(res:String,m:HTTPMethod=null)=new collectionMember{};
  def item(res:String,m:HTTPMethod=null)=new collectionMember{};
  
  def collection(rootUrl:String,name:String=null)(items:collectionMember*)=new collectionAPIMember with collectionMember{};
  def collectionWithAbsoluteUrls(name:String,rootUrl:String)(items:collectionMember*)=new collectionAPIMember{};
  
  def api(p:patternAPIMember*)=new collectionAPIMember{};
  
  def simpleCollection(url:String,memberUrl:String="{id}",name:String=null)(items:collectionMember*):collectionMember={
    collection(url,name){
       delete(memberUrl,DELETE)      
       update(memberUrl,PUT)
       create(memberUrl,POST)
    }
  }
  
  api{
    collection("contacts","/contacts"){
      action("flagBad",POST)
    };      
  }
}
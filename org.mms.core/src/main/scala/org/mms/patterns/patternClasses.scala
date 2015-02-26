package org.mms.patterns

sealed trait HTTPMethod {};
object PUT extends HTTPMethod;
object GET extends HTTPMethod;
object POST extends HTTPMethod;
object DELETE extends HTTPMethod;
object OPTIONS extends HTTPMethod;

trait collectionAPIMember extends patternAPIMember {
  def secondary(name: String) = new collectionAPIMember {};
}
trait patternAPIMember;

trait collectionMember;

case class API(name: String)(
    p: patternAPIMember*)
object PatternDescriptions {

  def list(res: String, m: HTTPMethod = null) = new collectionMember {};
  def action(res: String, m: HTTPMethod = null) = new collectionMember {};
  def update(res: String, m: HTTPMethod = null) = new collectionMember {};
  def delete(res: String, m: HTTPMethod = null) = new collectionMember {};
  def create(res: String, m: HTTPMethod = null) = new collectionMember {};
  def item(res: String, m: HTTPMethod = null)(ps:collectionMember*) = new collectionMember {};
  
  def secondaryCollection(res: String, m: HTTPMethod = null)(items: collectionMember*) = action(res,m);

  def resource(rootUrl: String, name: String = null)(items: collectionMember*) = new collectionAPIMember with collectionMember {};
  def collection(rootUrl: String, name: String = null)(items: collectionMember*) = new collectionAPIMember with collectionMember {};
  def collectionWithAbsoluteUrls(name: String, rootUrl: String)(items: collectionMember*) = new collectionAPIMember {};

  def api(p: patternAPIMember*) = new collectionAPIMember {};

  def simpleCollection(url: String, memberUrl: String = "{id}", name: String = null)(items: collectionMember*): collectionMember with patternAPIMember = {
    collection(url, name) {
      list("",GET)
      item(memberUrl, GET)(
        delete("", DELETE),
        update("", PUT)
      )
      create("", POST)
    }
  }
  

  api {
    collection("contacts", "/contacts") {
      action("flagBad", POST)
    };
  }

  
}
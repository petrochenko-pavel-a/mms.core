
import org.mms.core.ModelType

object Resource extends ModelType;
object Action extends ModelType;

object ResourceAction extends ModelType{
  val resource=propOf(Resource);//may include url parameter placeholder
  val action=propOf(Resource);
}

object ItemSchema extends ModelType{}

object ResourceActionWithId extends ModelType(ResourceAction){
  val whereToInserId=propOf(RequestElementRef)//always uri parameter???
  val needsItemInBody=bool;//actually is clear for all except delete and get?
  val itemSchema=optional(propOf(ItemSchema));//individual item schema
}

object CollectionDescription extends APIPattern{
  
  val name=str;
  
  val collection=propOf(ResourceAction);
  
  val elementId=propOf(ResponseElementRef)//actually pointer in body
  
  val itemSchema=propOf(ItemSchema);//actually it might slightly differ for update/post/get
  //TODO think about it
  
  val elementsCollection=propOf(ResponseElementRef)//where to find elements in response
  
  val supportedPatterns=list(propOf(classOf[CollectionPatterns]));//what patterns are supported by this collection
  
  val getIndividual=propOf(ResourceActionWithId);
  
  val deleteIndividual=propOf(ResourceActionWithId);
  
  val create=propOf(ResourceAction);
  
  val updateIndividual=propOf(ResourceActionWithId);
  
  val patchIndividual=propOf(ResourceAction);//pretty rare always HTTP Patch???
  
  val secondaryCollections=list(propOf(SecondaryCollection));
  
  val itemActions=propOf(ResourceAction);//does it actually has semantic.??
  
  examples("box","meetup","github","facebook","..")
}

object SecondaryCollection extends APIPattern(CollectionDescription){
  val parentCollection=propOf(CollectionDescription);
}

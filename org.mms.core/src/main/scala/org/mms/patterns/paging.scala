import org.mms.core.ModelType
import org.mms.core.AbstractType
import org.mms.core.AbstractType
import org.mms.core.ModelType

object RequestElementRef extends ModelType{}
object ResponseElementRef extends ModelType{}

object Paging extends ModelType{
  val limit=optional(propOf(RequestElementRef));//it is actually same property in all types of paging
}
//azuretablestorage, bitly,...

object QueryParameterPaging extends ModelType(Paging){
  val offset=propOf(RequestElementRef);  
}
//bitly

object PageNumberPaging extends ModelType(Paging){
  val pageNumber=propOf(RequestElementRef);
  val pageNumberStartsFrom=optional(int) withDefault(0);
}
//accuweather (0)

object IteratorPaging extends ModelType(Paging){
  val placeToInsertIterator=propOf(RequestElementRef);
  val placeToGetIteratorInfo=propOf(ResponseElementRef);    
}
//blogger
object PartitinedIteratorPaging extends ModelType(IteratorPaging){
  val placeToInsertPartitionIterator=propOf(RequestElementRef);
  val placeToGetPartitionIteratorInfo=propOf(ResponseElementRef);      
}
//azuretablestorage
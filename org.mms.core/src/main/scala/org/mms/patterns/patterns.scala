import org.mms.core.AbstractType
import org.mms.core.AbstractType
import org.mms.core.ModelType
import org.mms.core.transform.ModelTypeModel

object RequestElementRef extends ModelType {}
object ResponseElementRef extends ModelType {}
object SelectorInSchemaElement extends ModelType {}

class APIPattern(v: APIPattern = null) extends ModelType[APIPattern](v) {
  def examples(examples: String*) {};
}

object Limitable extends APIPattern {
  val limit = optional(propOf(RequestElementRef)); //it is actually same property in all types of paging
  examples("geonames")
}
trait CollectionPatterns;
trait Pageable extends CollectionPatterns;
trait FilterPattern extends CollectionPatterns;
trait RateLimiting extends CollectionPatterns;
trait FieldInfo extends CollectionPatterns;
trait OrderingInfo extends CollectionPatterns;


object SortInfo extends APIPattern(Limitable) with OrderingInfo {
  val sorfField = propOf(RequestElementRef);//where to find values (enum)
  val ascDesc = propOf(RequestElementRef);
  
  examples("xero","github","stackexchange","...")
}

object OffsetPaging extends APIPattern(Limitable) with Pageable {
  val offset = propOf(RequestElementRef);

  examples("azuretablestorage", "bitly", "elasticsearch", "facebook",
    "freebase", "google_contacts", "instagram", "jira", "jobvite", "linkedIn",
    "pagerDuty", "parse", "paypal", "pivotal", "postmark", "slideshare", "stormpath", "twitch", "wordpress")
}
//bitly,box

object PageNumberPaging extends APIPattern(Limitable) with Pageable {
  val pageNumber = propOf(RequestElementRef);
  val pageNumberStartsFrom = optional(int) withDefault (0);

  examples("bitly", "box", "bufferapp", "eloqua", "github", "meetup", "quizlet_v2.0", "slack", "squareup", "stackexchange", "wordpress")
}
//accuweather (0)

object IteratorPaging extends APIPattern(Limitable) with Pageable {
  val placeToInsertIterator = propOf(RequestElementRef);
  val placeToInsertBackIterator = optional(propOf(RequestElementRef));
  val placeToGetIteratorInfo = propOf(ResponseElementRef);
  //note!! sometimes iterator based paging requires festching id of last object
  examples("accuweather", "facebook", "gmail", "google_calendar", "google_drive", "paypal", "salesforce", "stripe", "twitter", "xero", "yammer", "zendesk", "zuora","amazon_s3")
}
//blogger
object PartitinedIteratorPaging extends APIPattern(IteratorPaging) with Pageable {
  val placeToInsertPartitionIterator = propOf(RequestElementRef);
  val placeToGetPartitionIteratorInfo = propOf(ResponseElementRef);

  examples("azuretablestorage")
}

object FullUrlPaging extends APIPattern with Pageable {
  val placeToGetInfos = propOf(ResponseElementRef);
  examples("github", "zendesk")
}

object DateBasedPaging extends APIPattern with Pageable {
  val minDate = propOf(RequestElementRef);
  val maxDate = propOf(RequestElementRef);
  examples("google calendar", "instagram", "salesforce","facebook")
}
//looks similar to Date based paging but has slightly different semantics
//often mixed
object DateFiltered extends APIPattern  with FilterPattern{
  val minDate = propOf(RequestElementRef);
  val maxDate = propOf(RequestElementRef);
  //DateCreated<=YYYY-MM-DD
  examples("square_up","slideshare","stackexchange","twillio","github","xero","facebook","google_calendar","newRelic","slack","pager_duty");
}
object LocationFiltered extends APIPattern with FilterPattern{
  val geoLt = propOf(RequestElementRef);
  val geoLg = propOf(RequestElementRef);
  val distance = propOf(RequestElementRef);
  //q={latitude, longitude}!!!!
  examples("meetup","weatherunderground","geonames","twitter","instagram","accuweather");
}

object LocationByGeoString extends APIPattern with FilterPattern{
  val locationElement = propOf(RequestElementRef);
  examples("meetup","weatherunderground","geonames","twitter","...");
}

object KnowsCountOfResults extends APIPattern {
  val totalCount = propOf(ResponseElementRef);
  examples("parse", "...");
}
//azuretablestorage

object FieldAware extends APIPattern with FieldInfo{
  val whereToPutFields = propOf(RequestElementRef);
  val howToGetFields = propOf(SelectorInSchemaElement);
  examples("box", "azuretablestorage", "elasticsearch", "gmail", "linkedIn", "fields", "parse","jira","...")
}

object HasRateLimitedInfo extends APIPattern with RateLimiting{
  val placeToGetCurrentRateLimitStatus = propOf(ResponseElementRef);
  val placeToGetRateLimitInfo = propOf(ResponseElementRef);
  examples("github");
}
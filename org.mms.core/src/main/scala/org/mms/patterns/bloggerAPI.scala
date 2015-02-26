package org.mms.patterns

import PatternDescriptions._;


class BloggerAPI{
  
  api(
    collection("blobPosts")(
      list("/blobPosts")    
    )   
  
  )
}
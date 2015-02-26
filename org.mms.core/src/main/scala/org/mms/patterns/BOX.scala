package org.mms.patterns

import PatternDescriptions._;


object BOXAapi extends API("Box")(
  simpleCollection("/folders","{folderId}")(
     item("{id}")(
       readOnlyCollection("/items", GET),
       readOnlyCollection("/collaborations", GET),
       action("/copy", POST),     
       action("/trash", GET),
       action("/trash", DELETE)  
     ),
     readOnlyCollection("/trash/items")
  )
)
package org.mms.patterns

import PatternDescriptions._;


object BOXAapi extends API("Box")(
  
  collection("/folders","Folders")(
    create("", POST),
    item("/{folderId}")(
      secondaryCollection("/items", GET)(),
      secondaryCollection("/collaborations", GET)(),
      action("/copy", POST),     
      action("/trash", GET),
      action("/trash", DELETE)  
    ),
    secondaryCollection("/trash/items")()
  ),
  
  collection("/files","Files")(
    item("/{fileId}")(
      secondaryCollection("/versions", GET)(
        delete("/{versionId}", DELETE),
        action("/current", GET)
      ),
      secondaryCollection("/tasks", GET)(),
      secondaryCollection("/comments", GET)(),
      action("/copy", POST),
      action("/thrash", GET),
      action("/thrash", DELETE),
      action("/thumbnail{extension}", GET),
      action("/content", DELETE)
    )
  ),
  
  collection("/shared_items", "Shared items")( list("",GET) ),
  
  simpleCollection("/collaborations","/{collaborationId}","Collaborations")(),
  
  resource("/search", "Search")( action("", GET) ),
  
  collection("/events", "Events")( list("",GET), action("", OPTIONS) ),
  
  simpleCollection("/users","/{userId}","Users")(
    item("/{userId}")(
      secondaryCollection("/email_aliases")(
        list("",GET),
        item("/{emailAliasId}", GET)(
          delete("", DELETE)
        ),
        create("", POST)
      ),
      secondaryCollection("/memberships")(),
      action("/folders/{folderId}",PUT)
    ),
    action("/me", GET)
  ),
  
  collection("/tasks","Tasks")(
    create("", POST),
    item("/{taskId}")(
      retrieve("", GET),
      update("", PUT),
      delete("", DELETE),      
      secondaryCollection("/assignments", GET)()
    )
  ),
  
  collection("/task_assignments","Tasks")(
    create("", POST),
    item("/{taskAssignmentId}")(
      retrieve("", GET),
      update("", PUT),
      delete("", DELETE)
    )
  ),
  
  simpleCollection("/groups","/{groupId}","Groups")(
    item("/{groupId}")(
      secondaryCollection("/memberships", GET)(),
      secondaryCollection("/collaborations", GET)()
    )
  ),
  
  collection("/group_memberships","Group memberships")(
    create("", POST),
    item("/{groupMembershipId}")(
      retrieve("", GET),
      update("", PUT),
      delete("", DELETE)
    )
  )
  
  
  
)


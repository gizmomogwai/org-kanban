Feature: Work kanban tables
  # <ZWSP> denotes the U+200B Org adds after descriptions ending in ].
  Background:
    Given I open file "tests/test1.org"
    And I insert:
    """
    * TODO a [1/2]
    - [X] test1
    - [ ] test2
    * DONE b
    * DONE c
      :PROPERTIES:
      :CUSTOM_ID: customid1
      :ID: id1
      :END:
    * DONE d
      :PROPERTIES:
      :ID: id2
      :END:
    * Kanban
    #+BEGIN: kanban
    #+END:
    """
    When I place the cursor before "BEGIN: kanban"
    And I press "C-c C-c"

  Scenario: Create Kanban Tables
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

  Scenario: Move Todo Items
    When I go to line "18"
    And I run org-kanban/shift
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

    And I press "h"
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

    And I press "l"
    Then the kanban table should be:
    | TODO | DONE |
    | | [[file:test1.org::*a ][a [1/2]<ZWSP>]] |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

    And I press "l"
    Then the kanban table should be:
    | TODO | DONE |
    | | [[file:test1.org::*a ][a [1/2]<ZWSP>]] |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

    And I press "h"
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

    And I press "h"
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | | [[id:id2][d]] |

  Scenario: Move Todo Items by customid
    When I go to line "20"
    And I run org-kanban/prev
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | [[#customid1][c]] | |
    | | [[id:id2][d]] |

  Scenario: Move Todo Items by id
    When I go to line "21"
    And I run org-kanban/prev
    Then the kanban table should be:
    | TODO | DONE |
    | [[file:test1.org::*a ][a [1/2]<ZWSP>]] | |
    | | [[file:test1.org::*b][b]] |
    | | [[#customid1][c]] |
    | [[id:id2][d]] | |

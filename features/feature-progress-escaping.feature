Feature: Progress cookies and heading escaping
  # Data tables keep percent signs out of Ecukes' doc-string formatter.
  # <ZWSP> denotes the U+200B Org adds after descriptions ending in ].
  Background:
    Given I open file "tests/test1.org"

  Scenario Outline: Progress cookies survive repeated updates and task movement
    Given I insert these lines:
      | * TODO Task <cookie> |
      | :PROPERTIES: |
      | :<property>: task |
      | :END: |
      | * Board |
      | #+BEGIN: kanban |
      | #+END: |
    When I update the kanban table
    And I update the kanban table
    And I update the kanban table
    Then the kanban table should be:
      | TODO | DONE |
      | <link> | |
    When I go to line "9"
    And I run org-kanban/next
    Then I should see "* DONE Task"
    When I update the kanban table
    Then the kanban table should be:
      | TODO | DONE |
      | | <link> |

    Examples:
      | cookie | property | link |
      | [1/2] | COMMENT | [[file:test1.org::*Task ][Task [1/2]<ZWSP>]] |
      | [33%] | COMMENT | [[file:test1.org::*Task ][Task [33%]<ZWSP>]] |
      | [1/2] | ID | [[id:task][Task [1/2]<ZWSP>]] |
      | [33%] | ID | [[id:task][Task [33%]<ZWSP>]] |
      | [1/2] | CUSTOM_ID | [[#task][Task [1/2]<ZWSP>]] |
      | [33%] | CUSTOM_ID | [[#task][Task [33%]<ZWSP>]] |

  Scenario: Literal percent sequences and backslashes resolve to the original heading
    Given I insert these lines:
      | * TODO Literal %20 %5B %25 \[brackets\] |
      | * Board |
      | #+BEGIN: kanban |
      | #+END: |
    When I update the kanban table
    And I update the kanban table
    Then the kanban table should be:
      | TODO | DONE |
      | [[file:test1.org::*Literal %20 %5B %25 \\\[brackets\\\]][Literal %20 %5B %25 \[brackets\]<ZWSP>]] | |
    And the kanban links should open these headings:
      | Literal %20 %5B %25 \[brackets\] |
    When I go to line "6"
    And I run org-kanban/next
    Then I should see "* DONE Literal"

Feature: Bugreport todoitem with embedded link
  # <ZWSP> denotes U+200B, which Org adds so the cookie's ] stays in the description.
  # Use a data table: Ecukes treats percent signs in doc strings as format directives.
  Background:
    Given I open file "tests/bugreport-todo-item-with-embedded-link.org"
    When I place the cursor before "Here"

  Scenario: Kanban-table is correctly created
    And I run org-kanban/initialize-here
    Then the kanban table should be:
    | DONE | TODO |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 1 with \[\[*Item 2\]\[link to Item 2\]\]][Item 1 with link to Item 2]] |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 2 ][Item 2 [1/2]<ZWSP>]] |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 3 ][Item 3 [33%]<ZWSP>]] |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 4 ][Item 4 [1/2]<ZWSP>]] |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*1][1]]                          |
    | [[file:bugreport-todo-item-with-embedded-link.org::*2][2]]    |                            |
    And the kanban links should open these headings:
      | Item 1 with [[*Item 2][link to Item 2]] |
      | Item 2 [1/2] |
      | Item 3 [33%] |
      | Item 4 [1/2] |
      | 1 |
      | 2 |

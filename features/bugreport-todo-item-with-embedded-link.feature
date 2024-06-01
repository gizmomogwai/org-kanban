Feature: Bugreport todoitem with embedded link
  Background:
    Given I open file "tests/bugreport-todo-item-with-embedded-link.org"
    When I place the cursor before "Here"

  Scenario: Kanban-table is correctly created
    And I run org-kanban/initialize-here
    Then I should see:
    """
    * Kanban
    #+BEGIN: kanban :mirrored t
    | DONE | TODO                       |
    |------+----------------------------|
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 1 with \[\[*Item 2\]\[link to Item 2\]\]][Item 1 with link to Item 2]] |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 2 ][Item 2 [1/2]]]               |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 3 ][Item 3 [33%]]]               |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*Item 4 ][Item 4 [1/2]]]               |
    |      | [[file:bugreport-todo-item-with-embedded-link.org::*1][1]]                          |
    | [[file:bugreport-todo-item-with-embedded-link.org::*2][2]]    |                            |
    #+END:
    Here
    """

Feature: Work kanban tables
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
    Then I should see:
    """
    * Kanban
    #+BEGIN: kanban
    | TODO    | DONE |
    |---------+------|
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    |         | [[#customid1][c]]    |
    |         | [[id:id2][d]]    |
    #+END:
    """

  Scenario: Move Todo Items
    When I go to line "18"
    And I run org-kanban/shift
    Then I should see:
    """
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    """

    And I press "h"
    Then I should see:
    """
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    """

    And I press "l"
    Then I should see:
    """
    |      | [[a][a {1/2}]] |
    |      | [[b][b]]       |
    """

    And I press "l"
    Then I should see:
    """
    |      | [[a][a {1/2}]] |
    |      | [[b][b]]       |
    """

    And I press "h"
    Then I should see:
    """
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    """

    And I press "h"
    Then I should see:
    """
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    """

  Scenario: Move Todo Items by customid
    When I go to line "20"
    And I run org-kanban/prev
    Then I should see:
    """
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    | [[#customid1][c]]       |      |
    """

  Scenario: Move Todo Items by id
    When I go to line "21"
    And I run org-kanban/prev
    Then I should see:
    """
    | TODO    | DONE |
    |---------+------|
    | [[a][a {1/2}]] |      |
    |         | [[b][b]]    |
    |         | [[#customid1][c]]    |
    | [[id:id2][d]]       |      |
    """

      | [[a][a {1/2}]] |      |
      |      | [[b][b]]       |
jo

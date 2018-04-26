Feature: Work kanban tables
  Background:
    Given I open file "tests/test1.org"
    And I insert:
    """
    * TODO a
    * DONE b
    * Kanban
    #+BEGIN: kanban
    #+END:
    """
    When I place the cursor before "BEGIN: kanban"
    And I press "C-c C-c"

  Scenario: Create Kanban Tables
    Then I should see:
    """
    * TODO a
    * DONE b
    * Kanban
    #+BEGIN: kanban
    | TODO | DONE |
    |------+------|
    | [[file:test1.org::a][a]]    |      |
    |      | [[file:test1.org::b][b]]    |
    #+END:
    """

  Scenario: Move Todo Item
    When I go to line "7"
    And I run org-kanban/shift
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[file:test1.org::a][a]]    |
    |      | [[file:test1.org::b][b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[file:test1.org::a][a]]    |      |
    |      | [[file:test1.org::b][b]]    |
    """

    And I press "k"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[file:test1.org::a][a]]    |
    |      | [[file:test1.org::b][b]]    |
    """

    And I press "k"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[file:test1.org::a][a]]    |
    |      | [[file:test1.org::b][b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[file:test1.org::a][a]]    |      |
    |      | [[file:test1.org::b][b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[file:test1.org::a][a]]    |      |
    |      | [[file:test1.org::b][b]]    |
    """

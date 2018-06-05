Feature: Work kanban tables
  Background:
    Given I open file "tests/test1.org"
    And I insert:
    """
    * TODO a
    * DONE b
    * DONE c
      :PROPERTIES:
      :CUSTOM_ID: 1
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
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    |      | [[#1][c]]    |
    #+END:
    """

  Scenario: Move Todo Item
    When I go to line "11"
    And I run org-kanban/shift
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[a][a]]    |
    |      | [[b][b]]    |
    |      | [[#1][c]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    """

    And I press "k"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[a][a]]    |
    |      | [[b][b]]    |
    """

    And I press "k"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[a][a]]    |
    |      | [[b][b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    """

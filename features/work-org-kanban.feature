Feature: Work kanban tables
  Background:
    Given I open file "tests/test1.org"
    And I insert:
    """
    * TODO a
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
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    |      | [[#customid1][c]]    |
    |      | [[id:id2][d]]    |
    #+END:
    """

  Scenario: Move Todo Items
    When I go to line "16"
    And I run org-kanban/shift
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

  Scenario: Move Todo Items by customid
    When I go to line "18"
    And I run org-kanban/prev
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    | [[#customid1][c]]    |      |
    """

  Scenario: Move Todo Items by id
    When I go to line "19"
    And I run org-kanban/prev
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a][a]]    |      |
    |      | [[b][b]]    |
    |      | [[#customid1][c]]    |
    | [[id:id2][d]]    |      |
    """

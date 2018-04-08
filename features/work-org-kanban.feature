Feature: Work kanban tables
  Background:
    Given I turn on org-mode
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
    | TODO | DONE |
    |------+------|
    | [[a]]    |      |
    |      | [[b]]    |
    """

  Scenario: Move Todo Item
    When I go to line "7"
    And I run org-kanban/shift
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[a]]    |
    |      | [[b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a]]    |      |
    |      | [[b]]    |
    """

    And I press "k"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[a]]    |
    |      | [[b]]    |
    """

    And I press "k"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |      | [[a]]    |
    |      | [[b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a]]    |      |
    |      | [[b]]    |
    """

    And I press "j"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    | [[a]]    |      |
    |      | [[b]]    |
    """

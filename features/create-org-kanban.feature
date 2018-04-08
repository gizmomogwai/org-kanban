Feature: Create kanban table
  Background:
    Given I turn on org-mode
    And I insert:
    """"
    * TODO 1
    * DONE 2
    Here
    """"
    When I place the cursor before "Here"

  Scenario: Create the tables
    And I run org-kanban/initialize
    Then I should see:
    """
    #+BEGIN: kanban
    | TODO | DONE |
    |------+------|
    | [[1]]    |      |
    |      | [[2]]    |
    #+END:
    """

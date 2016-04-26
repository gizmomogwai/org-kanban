Feature: Creates kanban tables
  Background:
    Given I turn on org-mode
    And I insert:
    """
    * TODO a
    * DONE b
    * Kanban
    #+BEGIN: kanban
    here
    #+END:
    """

  Scenario: Create Kanban Tables
  When I place the cursor before "BEGIN: kanban"
  And I press "C-c C-c"
  Then I should see:
  """
  | TODO | DONE |
  |------+------|
  | [[a]]    |      |
  |      | [[b]]    |
  """



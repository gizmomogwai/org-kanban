Feature: Create kanban table
  Background:
    Given I open file "tests/test1.org"
    And I insert:
    """
    Begin
    * TODO 1
    Here
    * DONE 2
    End
    """
    When I place the cursor before "Here"

  Scenario: Create the kanban at point
    And I run org-kanban/initialize-here
    Then I should see:
    """
    * TODO 1
    #+BEGIN: kanban :mirrored t
    | DONE | TODO |
    |------+------|
    |      | [[1][1]]    |
    | [[2][2]]    |      |
    #+END:
    Here
    * DONE 2
    End
    """

  Scenario: Create the kanban at the beginning
    And I run org-kanban/initialize-at-beginning
    Then I should see:
    """
    Begin
    #+BEGIN: kanban :mirrored t
    | DONE | TODO |
    |------+------|
    |      | [[1][1]]    |
    | [[2][2]]    |      |
    #+END:
    * TODO 1
    Here
    * DONE 2
    End
    """

  Scenario: Create the kanban at the ending
    And I run org-kanban/initialize-at-end
    Then I should see:
    """
    Begin
    * TODO 1
    Here
    * DONE 2
    End
    #+BEGIN: kanban :mirrored t
    | DONE | TODO |
    |------+------|
    |      | [[1][1]]    |
    | [[2][2]]    |      |
    #+END:
    """

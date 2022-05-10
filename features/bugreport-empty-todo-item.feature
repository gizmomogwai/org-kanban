Feature: Bugreport empty todo item
  Background:
    Given I open file "tests/test1.org"
    And I insert:
    """
    Begin
    * TODO
    Here
    End
    """
    When I place the cursor before "Here"

  Scenario: Create the kanban at point
    And I run org-kanban/initialize-here
    Then I should see:
    """
    * TODO
    #+BEGIN: kanban :mirrored t
    | DONE | TODO |
    |------+------|

    #+END:
    Here
    End
    """

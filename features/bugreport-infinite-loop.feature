Feature: Bugreport infinite loop when moving subtrees
  Background:
    Given I open file "tests/bugreport-infinite-loop-when-moving-subtree.org"
    When I place the cursor before "[[file:bugreport-infinite-loop-when-moving-subtree.org::*row2][row2]]"

  Scenario: Move subtree up
    And I run org-kanban/move-subtree-up
    Then I should see:
    """
    * TODO row2
    * TODO row1
    """

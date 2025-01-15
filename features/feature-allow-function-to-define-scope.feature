Feature: Allow function to define scope
  Background:
    Given I open file "tests/feature-allow-function-in-scope.org"

  Scenario: use lambda to reference a subtree in another file
    When I go to line "1"
    And I press "C-c C-c"
    Then I should see:
    """
    | DONE | TODO |
    |------+------|
    |      | [[file:feature-allow-function-in-scope-example.org::#cid-3][3]]    |
    |      | [[file:feature-allow-function-in-scope-example.org::#cid-4][4]]    |
    """

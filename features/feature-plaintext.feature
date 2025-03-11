Feature: Support plaintext tables
  Background:
    Given I open file "tests/feature-plaintext.org"

  Scenario: set the plaintext parameter to t
    When I go to line "4"
    And I press "C-c C-c"
    Then I should see:
    """
    | TODO | DONE |
    |------+------|
    |    1 |      |
    |    2 |      |
    """

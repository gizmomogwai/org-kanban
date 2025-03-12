Feature: Support for caption in tables
  Background:
    Given I open file "tests/feature-caption.org"

  Scenario: set the caption parameter to `thecaption`
    When I go to line "4"
    And I press "C-c C-c"
    Then I should see:
    """
    #+CAPTION: thecaption
    | TODO | DONE |
    |------+------|
    |    1 |      |
    |    2 |      |
    """

Feature: Support for headers in tables
  Background:
    Given I open file "tests/feature-headers.org"

  Scenario: set some headers
    When I go to line "4"
    And I press "C-c C-c"
    Then I should see:
    """
    #+CAPTION: nice
    #+NAME: nicer
    | TODO | DONE |
    |------+------|
    |    1 |      |
    |    2 |      |
    """

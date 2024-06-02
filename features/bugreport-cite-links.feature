Feature: Bugreport cite links
  Background:
    Given I open file "tests/bugreport-cite-link.org"

  Scenario: cite link is properly formatted
    When I go to line "3"
    And I press "C-c C-c"
    Then I should see:
    """
    |      | [[file:bugreport-cite-link.org::*abc \[cite:@knuth:1984\]][abc [ci...]] |
    """

Feature: Bugreport wrong shortening of links in todos
  Background:
    Given I open file "tests/bugreport-wrong-shortening-of-links-in-todos.org"

  Scenario: cite link is properly formatted
    When I go to line "1"
    And I press "C-c C-c"
    Then I should see:
    """
    |      | [[file:bugreport-wrong-shortening-of-links-in-todos.org::*Item 1 with \[\[*Item 2\]\[link to Item 2\]\] ][Item 1 with link to ...]] |
    """

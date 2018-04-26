Feature: Internal workings
  Scenario: Shorten description
    Given I open file "test/test1.org"
    When I shorten "1234567890" to length "5" with abbreviation "..." I should get "12..."
Feature: Frames
  Scenario: Starting framecs
    Given I start framecs
    Then Current frame is a framecs frame

  Scenario: Manipulating frames
    Given I start framecs
    And I create a new frame
    And I call "framecs/go-to-previous-frame"
    Then I am in frame number "0"
    And I call "framecs/go-to-next-frame"
    Then I am in frame number "1"
    And I call "framecs/go-to-next-frame"
    Then I am in frame number "0"
    And I call "framecs/go-to-next-frame"
    Then I am in frame number "1"
    And I can create and delete a new frame

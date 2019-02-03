Feature: Workspaces
  Scenario: Navigate through workspaces
    Given I start framecs
    Given I create a new workspace
    Then I am in workspace number "1"
    And I call "framecs/go-to-previous-workspace"
    Then I am in workspace number "0"
    And I call "framecs/go-to-next-workspace"
    Then I am in workspace number "1"
    And I call "framecs/go-to-next-workspace"
    Then I am in workspace number "0"
    And I call "framecs/go-to-next-workspace"
    Then I am in workspace number "1"
    And I call "framecs/delete-current-workspace"

  Scenario: Delete a workspace
    Given I start framecs
    Given I create a new workspace
    And I create a new frame
    Then There are "2" workspaces and "3" frames
    Given I delete current workspace
    Then There are "1" workspaces and "1" frames

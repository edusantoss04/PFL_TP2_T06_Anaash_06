# PFL_TP2_T06_Anaash_06

The game we choose was Anaash.

### Group and Contributions: (falta escrever as contribuições e decrição)
indication of the contribution (in percentages, adding up to 100%, and a brief description of tasks performed) of each member of the group to the assignment.
- Eduardo Narciso Rodrigues Santos, up202207521
- Pedro dos Santos Sousa Pedro, up202206961
- Renata Soares Bandeira Simão, up202205124

### Installation and Execution
1. sicstus -l game.pl
2. play.

### Description of Anaash
Anaash is a two-player abstract strategy game created by Mark Steere in February 2021. It is played on a 6x6, 8x8, or larger checkerboard, which starts fully populated with a checkered pattern of red and blue checkers. Each checker stack consists of one piece, known as a "singleton." Players take turns moving and combining stacks, aiming to outmaneuver their opponent through strategic positioning and control of the board.

### Objective
The goal of the game is to capture all enemy checkers. Draws are not possible.

### Gameplay
Players alternate turns, starting with Red. During their turn, players can make one move using one of their stacks. If a player cannot make a valid move, they must skip their turn. At least one player will always have a move available at any point in the game. The goal is to capture all the opponent's pieces or to ensure that all remaining pieces belong to a single player.

### Types of Moves
#### Positional Moves:
- A stack is moved orthogonally (horizontally or vertically) to an adjacent unoccupied square.
- The move must reduce the Manhattan distance to the stack's nearest neighbor, regardless of color or height.
- Only stacks with no orthogonal neighbors are eligible for positional moves.

#### Stacking Moves:
- A stack is moved onto an orthogonally adjacent friendly stack of equal or greater height.
- The resulting stack's size is the sum of the two stacks.

#### Capturing Moves:
- A stack captures an orthogonally adjacent enemy stack of equal or smaller height.
- The capturing stack's size increases by adding the captured stack's size.

### Notable Rules:
- The game always progresses towards one player capturing all the opponent's pieces, making it deterministic with no chance of ties.
- If a player has no valid moves, they must skip their turn.
- An optional rule allows diagonal moves (one per player per turn), which can be enabled during game setup.
- The game ends when:
    - All remaining pieces belong to one player.
    - Only two stacks remain, and the winner is the player with the largest stack.

#### References
- Game rules source:[ Mark Steere's official rule sheet](https://www.marksteeregames.com/Anaash_rules.pdf)
- Author's website: [marksteeregames.com](http://marksteeregames.com/)

### Considerations for game extensions
Our game has 3 different board sizes, such as 4x4, 6x6 and 8x8, and in the 3 different sizes, there are no additional considerations to take into account during the game. The size of the board does not change the way you play. In addition, we have implemented an optional rule in which all players have the chance to play once diagonally. There is no variety of rules when it comes to the players' gaming experience.

### Game Logic
Describe the main design decisions regarding the implementation of the game logic in Prolog (do not copy the source code). This section should have information on the following topics, among others:

#### Game Configuration Representation
describe the information required to represent the game configuration, how it is represented internally and how it is used by the initial_state/2 predicate.
  
#### Internal Game State Representation
describe the information required to represent the game state, how it is represented internally, including an indication of the meaning of each atom (i.e. how different pieces are represented). Include examples of representations of initial, intermediate, and final game states.
  
#### Move Representation
describe the information required to represent a move, and how it is represented internally (e.g., the coordinates of a board location, and/or other information necessary to represent a move) and how it is used by the move/3 predicate.

#### User Interaction
briefly describe the game menu system, as well as how interaction with the user is performed, focusing on input validation (e.g., when reading a move).

### Conclusions
Looking at all the work that has been done, we realise that if we had more time to work on this project, we would have implemented the optional rule in which players have the possibility of playing once diagonally not only for human players, but also for bots.

### Bibliography
- Game rules source:[ Mark Steere's official rule sheet](https://www.marksteeregames.com/Anaash_rules.pdf)
- Author's website: [marksteeregames.com](http://marksteeregames.com/)
- [SICStus Prolog](https://www.swi-prolog.org/)
- [Geeks for geeks - minimax algorithm](https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-1-introduction/)
- [Video - minimax algortihm](https://www.youtube.com/watch?v=l-hh51ncgDI)
- [SICStus Manual](https://sicstus.sics.se/sicstus/docs/latest4/pdf/sicstus.pdf)

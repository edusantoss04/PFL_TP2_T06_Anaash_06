# PFL_TP2_T06_Anaash_06

The game we choose was Anaash.

### Group and Contributions: (falta escrever as contribuições e decrição)
indication of the contribution (in percentages, adding up to 100%, and a brief description of tasks performed) of each member of the group to the assignment.
- Eduardo Narciso Rodrigues Santos, up202207521
- Pedro dos Santos Sousa Pedro, up202206961
- Renata Soares Bandeira Simão, up202205124

● Installation and Execution: include all the necessary steps for the correct execution of the game in both Linux and Windows environments (in addition to the installation of SICStus Prolog 4.9).

### Description of Anaash
Anaash is a two-player abstract strategy game created by Mark Steere in February 2021. It is played on a 6x6, 8x8, or larger checkerboard, which starts fully populated with a checkered pattern of red and blue checkers. Each checker stack consists of one piece, known as a "singleton." Players take turns moving and combining stacks, aiming to outmaneuver their opponent through strategic positioning and control of the board.

### Objective
The goal of the game is to capture all enemy checkers. Draws are not possible.

### Gameplay
Players alternate turns, starting with Red. During their turn, players can make one move using one of their stacks. If a player cannot make a valid move, they must skip their turn and wait until a move becomes available. At least one player will always have a move available at any point in the game.

### Types of Moves
#### 1. Positional Moves:
- A stack is moved orthogonally (horizontally or vertically) to an adjacent unoccupied square.
- The move must reduce the Manhattan distance to the stack's nearest neighbor, regardless of color or height.
- Only stacks with no orthogonal neighbors are eligible for positional moves.

#### 2. Stacking Moves:
- A stack is moved onto an orthogonally adjacent friendly stack of equal or greater height.

#### 3. Capturing Moves:
- A stack captures an orthogonally adjacent enemy stack of equal or smaller height.

### Notable Rules
- The game always progresses towards one player capturing all the opponent's pieces, making it a deterministic game with no chance of ties.
- The name "Anaash" originates from the Mongolian word for giraffe.

### References
- Game rules source:[ Mark Steere's official rule sheet](https://www.marksteeregames.com/Anaash_rules.pdf)
- Author's website: [marksteeregames.com](http://marksteeregames.com/)

● Considerations for game extensions: describe the considerations taken into account when extending the game design, namely when considering variable-sized boards, optional rules (e.g., simplified rules for novice players, additional rules for expert players), and other aspects.

● Game Logic: Describe the main design decisions regarding the implementation of the game logic in Prolog (do not copy the source code). This section should have information on the following topics, among others:

o Game Configuration Representation: describe the information required to represent the game configuration, how it is represented internally and how it is used by the initial_state/2 predicate.
  
o Internal Game State Representation: describe the information required to represent the game state, how it is represented internally, including an indication of the meaning of each atom (i.e. how different pieces are represented). Include examples of representations of initial, intermediate, and final game states.
  
o Move Representation: describe the information required to represent a move, and how it is
represented internally (e.g., the coordinates of a board location, and/or other information
necessary to represent a move) and how it is used by the move/3 predicate.

o User Interaction: briefly describe the game menu system, as well as how interaction with the user
is performed, focusing on input validation (e.g., when reading a move).


● Conclusions: Conclusions about the work carried out, including limitations of the program (and known
issues), as well as possible improvements (future developments roadmap).

● Bibliography: List of books, papers, web pages and other resources used during the development of the
assignment. If you used tools such as ChatGPT, list the queries used.

You can also include one or more imagesillustrating the execution of the game, showing initial, intermediate
and final game states, and interaction with the game.
The entire document should not exceed four pages (including images and references).

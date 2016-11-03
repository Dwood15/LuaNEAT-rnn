## LuaNEAT-rnn

This is an implementation of a NEAT Neural Network, designed to play _Super Mario World (SNES)_. Based on MarI/O by SethBling, as seen here: https://www.youtube.com/watch?v=qv6UVOQ0F44 . And using Rodriguez Amiral's smw-bizhawk lua code from here: https://github.com/rodamaral/smw-tas

Take these scripts and place them in: `\lua\snes\` Select a starting point and create a save state, name it `DP1.state` (where should the save state be saved?)

#### SethBling's MarI/O has some issues, and this project is an attempt to remedy that weaknesses:
1. Weak fitness function 
   - Rightmost only - can't discriminate between getting bonuses and alternate routes.
   - Decreasing fitness over time, even if an alternate route.
   - Penalizes smarter moves which may extend time (ducking in a hole while a Bullet Bill passes).
   - Does not take into account being in the main menu after a successful level run. 
2. Limited inputs (cannot distinguish difference between different enemies and other tiles) 
3. Memorization of the level 
4. Slow, takes a long time to evaluate each specific generation and their species
   - This is an easily parallelizable task

#### This program looks to solve these problems by introducing some new features
1. Improving fitness function 
   - Basing fitness as a function of their novelty
   - Improve Breeding/Selection methods Track each AI's movements to determine its uniqueness in combination with the Neuron layout.
2. Increasing the number of Input Neurons 
   - Add more input tile types
   - For awareness, give Neural Net
3. Add level randomization/cycling to encourage mario to learn how to handle different situations.

COMP 6411: CSPL Project - Rock Paper Scissors Game

- Overview:
    This project is a solo development effort to create a Rock Paper Scissors game simulation using Erlang.

- Developer
    Name: Mohammed Huzaifa
    Student ID: 40242080

- Instructions to Run the Program
    1. Compile the Code - To compile the Erlang files, run the following command:
        -> erlc game.erl player.erl

    2. Execute the Program - To start the game, use the command below. 
        -> erl -noshell -s game start players.txt -s init stop

    This will run the game using the player data from players.txt and stop the shell after execution.
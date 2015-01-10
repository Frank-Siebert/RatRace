from __future__ import print_function

from board import Board
from random import Random
from specimen import Specimen
from player import Player
import sys
import trap
import itertools
import coordinates

if sys.version_info >= (3,):
    xrange = range

NUMBER_OF_BOARDS = 20
BOARD_WIDTH = 50
BOARD_HEIGHT = 200

TRAP_FREQUENCY = .2

SPECIMEN_LIFESPAN = 500

NUMBER_OF_TURNS = 100000

INITIAL_SPECIMENS = 50

DNA_LENGTH = 50
LIFE_SPAN = 200

VISION_WIDTH = 5

VISION_DISTANCE = VISION_WIDTH/2

VISION = [coordinates.Coordinate(x, y)
          for x in xrange(-VISION_DISTANCE, VISION_DISTANCE)
          for y in xrange(VISION_DISTANCE, -VISION_DISTANCE-1, -1)]

SEED = 13722829

NUMBER_COLORS = 50  # needs to be bigger than 27


def initialize_board():
    board = Board(random.randrange(0, 100000000))
    #Build each type of trap
    colors = range(NUMBER_COLORS)
    random.shuffle(colors)
    all_traps = itertools.product(coordinates.directions, trap.trap_types)
    traps = [trap_type(direction, board, color)
             for (direction, trap_type), color in zip(all_traps, colors)]
    board.unused_colors = \
        colors[len(coordinates.directions)*len(trap.trap_types):]

    #add specimens
    for __ in xrange(INITIAL_SPECIMENS):
        board.add_specimen(Specimen(player, random.getrandbits(DNA_LENGTH), 0),
                           coordinates.Coordinate(
                               random.randrange(0, BOARD_WIDTH),
                               0))
    #add traps
    for height in xrange(BOARD_HEIGHT):
        for width in xrange(BOARD_WIDTH):
            if random.random() < TRAP_FREQUENCY:
                board.add_trap(random.choice(traps),
                               coordinates.Coordinate(width, height))
    return board


def take_turn(board, turn_number):
    points = 0
    for coordinate, specimens in board.specimens.items():
        for specimen in specimens:
            if turn_number == specimen.birth + SPECIMEN_LIFESPAN:
                points += coordinate.y/BOARD_HEIGHT
            else:
                vision = [board.get_color(coordinate+offset)
                          for offset in VISION]
                direction = player.take_turn(specimen, vision)
                new_location = coordinate+direction
                board.next_specimens[new_location] = specimen
    board.specimens = board.next_specimens
    board.next_specimens.clear()
    return points

if __name__ == "__main__":
    random = Random(SEED)
    player = Player()
    total_points = 0
    for __ in xrange(NUMBER_OF_BOARDS):
        board = initialize_board()
        for turn_number in xrange(NUMBER_OF_TURNS):
            total_points += take_turn(board, turn_number)
    print("Your bot got "+str(total_points)+" points")

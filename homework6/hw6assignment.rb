# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [
    rotations([[0,0], [1,0], [0,1], [1,1], [2,1]]), # 5-block-blob ext1
    [[[0,0], [-1,0], [-2,0], [1,0], [2,0]], # 5-block-long (needs two)
     [[0,0], [0,-1], [0,-2], [0,1], [0,2]]], # ext2
    rotations([[0,0], [0,1], [1,0]]) # short L. ext3
  ];
  # your enhancements here
  # update to My* and extent cheating
  def self.next_piece (board)
    if (board.cheatcount > 0)
      MyPiece.new([[[0,0]]], board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

  # returns true if thie piece is a cheat piece
  def isCheat
    @all_rotations == [[[0,0]]]
  end
end

class MyBoard < Board
  # your enhancements here
  # update to My* and extent cheating
  def initialize (game)
    @cheatcount = 0 # counter of the upcoming cheat pieces
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # rotates the current piece by 180 degrees if possible
  def rotate_around
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # update to My*
  def next_piece
    @current_block = MyPiece.next_piece(self)
    if (cheatcount > 0)
      @cheatcount -= 1
    end
    @current_pos = nil
  end

  # extent cheat method
  def cheat
    if (score >= 100 and # score >= 100.  2 cases where chearing is available. 
        ( (@current_block.isCheat) or # 1 is that the current block is the cheat block (after the first cheating) 
          ((not @current_block.isCheat) and cheatcount == 0))) # 2 is the first cheating
      @score -= 100
      @game.update_score
      @cheatcount += 1
    end
  end

  def cheatcount
    @cheatcount
  end

  # expand the range of index to [0,4]; take care of different-sized pieces
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..4).each{|index|
      current = locations[index];
      if (current != nil)
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
          @current_pos[index]
      end
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  # update to My*
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  # update 'u' and 'c'
  def key_bindings  
    super
    @root.bind('u' , proc {@board.rotate_around})
    @root.bind('c' , proc {@board.cheat})
  end
end


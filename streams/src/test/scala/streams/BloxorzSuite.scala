package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait NoSolution extends GameDef with Solver with StringParserTerrain {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooo---
        |-ooooooo--
        |------oTo-
        |------ooo-""".stripMargin
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")

      //mine
      assert(!terrain(Pos(4,4)), "4,4")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val inputBlock = Block(Pos(1,1),Pos(1,1))
      val expectedOutput = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(neighborsWithHistory(inputBlock, List(Left,Up)).toSet == expectedOutput)
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val newNeighbors = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )
      val expectedResult = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      assert(newNeighbors.toSet == expectedResult)
    }
  }

  test("neighbors") {
    new InfiniteTerrain {
      val startPos = Pos(0, 0)
      val goal = Pos(0, 0)
      val neighbors = startBlock.neighbors.map(_._1).toSet
      val left = Block(Pos(0, -2), Pos(0, -1))
      val right = Block(Pos(0, 1), Pos(0, 2))
      val up = Block(Pos(1, 0), Pos(2, 0))
      val down = Block(Pos(-2, 0), Pos(-1, 0))
      assert(neighbors == Set(left, right, up, down))
    }
  }

  test("no solution") {
    new NoSolution {
      assert(this.solution.length == 0)
    }
  }

}

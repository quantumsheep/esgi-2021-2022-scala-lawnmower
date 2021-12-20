package fr.esgi.al.funprog

import fr.esgi.al.funprog.exceptions.DonneesIncorectesException
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec

class LawnMowerSpec extends AnyFunSpec with GivenWhenThen {
  describe("In a 5x5 lawn") {
    val lawn = Lawn(5, 5)

    describe("a LawnMower") {
      it("should load from List(\"0 0 N\", \"\")") {
        Given("a LawnMower loaded from said data")
        val lawnMower = LawnMower.load(lawn, List("0 0 N", ""))

        Then("its lawn should be the passed lawn")
        assert(lawnMower.lawn == lawn)

        And("its starting position should be (x: 0, y: 0, d: N)")
        assert(lawnMower.start == Orientation(Coordinates(0, 0), 'N'))

        And("its current position should be equal to its starting position")
        assert(lawnMower.current == lawnMower.start)

        And("its instructions should be empty")
        assert(lawnMower.instructions == List())
      }

      it("should not load from List(\"0 0 X\", \"\")") {
        assertThrows[DonneesIncorectesException] {
          LawnMower.load(lawn, List("0 0 X", ""))
        }
      }

      describe("starting from (x: 0, y: 0, d: N)") {
        it("should change to (x: 0, y: 1, d: N) if passed \"A\" instructions") {
          val lawnMower = LawnMower(lawn, Orientation(Coordinates(0, 0), 'N'), "A").run()
          assert(lawnMower.current == Orientation(Coordinates(0, 1), 'N'))
        }

        it("should change to (x: 0, y: 0, d: E) if passed \"D\" instructions") {
          val lawnMower = LawnMower(lawn, Orientation(Coordinates(0, 0), 'N'), "D").run()
          assert(lawnMower.current == Orientation(Coordinates(0, 0), 'E'))
        }

        it("should change to (x: 0, y: 0, d: W) if passed \"G\" instructions") {
          val lawnMower = LawnMower(lawn, Orientation(Coordinates(0, 0), 'N'), "G").run()
          assert(lawnMower.current == Orientation(Coordinates(0, 0), 'W'))
        }

        it("should change to (x: 0, y: 0, d: S) if passed \"GG\" instructions") {
          val lawnMower = LawnMower(lawn, Orientation(Coordinates(0, 0), 'N'), "GG").run()
          assert(lawnMower.current == Orientation(Coordinates(0, 0), 'S'))
        }

        it("should change to (x: 3, y: 1, d: N) if passed \"ADAAA\" instructions") {
          val lawnMower = LawnMower(lawn, Orientation(Coordinates(0, 0), 'N'), "ADAAA").run()
          assert(lawnMower.current == Orientation(Coordinates(3, 1), 'E'))
        }
      }
    }
  }
}

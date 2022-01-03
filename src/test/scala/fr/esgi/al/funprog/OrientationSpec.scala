package fr.esgi.al.funprog

import fr.esgi.al.funprog.exceptions.DonneesIncorectesException
import org.scalatest.funspec.AnyFunSpec

class OrientationSpec extends AnyFunSpec {
  describe("An orientation with y: 3, x: 5 and directed to North (N)") {
    it("should rotate to the east when rotating to the right") {
      val orientation = Orientation(Coordinates(3, 5), 'N')
      val orientationTurned = orientation.rotateRight()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'E')
    }

    it("should rotate to the west when rotating to the left") {
      val orientation = Orientation(Coordinates(3, 5), 'N')
      val orientationTurned = orientation.rotateLeft()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'W')
    }
  }

  describe("An orientation with y: 3, x: 5 and directed to East (E)") {
    it("should rotate to the north when rotating to the right") {
      val orientation = Orientation(Coordinates(3, 5), 'E')
      val orientationTurned = orientation.rotateRight()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'S')
    }

    it("should rotate to the south when rotating to the left") {
      val orientation = Orientation(Coordinates(3, 5), 'E')
      val orientationTurned = orientation.rotateLeft()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'N')
    }
  }

  describe("An orientation with y: 3, x: 5 and directed to South (S)") {
    it("should rotate to the west when rotating to the right") {
      val orientation = Orientation(Coordinates(3, 5), 'S')
      val orientationTurned = orientation.rotateRight()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'W')
    }

    it("should rotate to the east when rotating to the left") {
      val orientation = Orientation(Coordinates(3, 5), 'S')
      val orientationTurned = orientation.rotateLeft()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'E')
    }
  }

  describe("An orientation with y: 3, x: 5 and directed to West (W)") {
    it("should rotate to the south when rotating to the right") {
      val orientation = Orientation(Coordinates(3, 5), 'W')
      val orientationTurned = orientation.rotateRight()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'N')
    }

    it("should rotate to the north when rotating to the left") {
      val orientation = Orientation(Coordinates(3, 5), 'W')
      val orientationTurned = orientation.rotateLeft()
      assert(orientationTurned.coordinates == Coordinates(3, 5))
      assert(orientationTurned.direction == 'S')
    }
  }

  describe("An orientation with y: 0, x: 0 and directed to X") {
    it("should throw an exception") {
      assertThrows[DonneesIncorectesException] {
        Orientation(Coordinates(0, 0), 'X')
      }
    }
  }
}

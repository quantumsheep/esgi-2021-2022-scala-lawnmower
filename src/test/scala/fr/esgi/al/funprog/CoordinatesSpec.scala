package fr.esgi.al.funprog

import org.scalatest.funspec.AnyFunSpec

class CoordinatesSpec extends AnyFunSpec {
  describe("A Coordinates pair") {
    it("should load from x and y") {
      val coords = Coordinates(1, 2)

      assert(coords.x == 1)
      assert(coords.y == 2)
    }
  }
}

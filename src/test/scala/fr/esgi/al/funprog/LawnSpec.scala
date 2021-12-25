package fr.esgi.al.funprog

import fr.esgi.al.funprog.exceptions.DonneesIncorectesException
import org.scalatest.funspec.AnyFunSpec

class LawnSpec extends AnyFunSpec {
  describe("A Lawn") {
    it("should load from List(\"5 3\")") {
      val lawn = Lawn.load(List("5 3"))

      assert(lawn.limitX == 5)
      assert(lawn.limitY == 3)
    }

    it("should not load from List(\"a 3\")") {
      assertThrows[DonneesIncorectesException] {
        Lawn.load(List("a 3"))
      }
    }

    it("should not load from List(\"5\")") {
      assertThrows[DonneesIncorectesException] {
        Lawn.load(List("5"))
      }
    }

    it("should not load from List(\"5 3\", \"a\")") {
      assertThrows[DonneesIncorectesException] {
        Lawn.load(List("5 3", "a"))
      }
    }

    it("should not load from List()") {
      assertThrows[DonneesIncorectesException] {
        Lawn.load(List())
      }
    }
  }
}

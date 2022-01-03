package fr.esgi.al.funprog

import org.scalatest.funspec.AnyFunSpec

class UtilsSpec extends AnyFunSpec {
  describe("a function that trims the spaces of the right part of a string") {
    it("should remove the spaces at the end") {
      assert(Utils.trimRight("hello  ") == "hello")
    }

    it("should keep the spaces at the beginning") {
      assert(Utils.trimRight("  hello  ") == "  hello")
    }
  }
}

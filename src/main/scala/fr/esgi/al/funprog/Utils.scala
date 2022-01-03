package fr.esgi.al.funprog

object Utils {
  def trimRight(s: String) = s.reverse.dropWhile(_ == ' ').reverse
}

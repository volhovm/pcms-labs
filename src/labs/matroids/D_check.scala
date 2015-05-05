package labs.matroids

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object D_check {
  def main(args: Array[String]) {
    def write(s: String) = Files.write(Paths.get("check.out"), s.getBytes(StandardCharsets.UTF_8))
    val S: Set[Set[Int]] = scala.io.Source.fromFile("check.in").getLines().toList
      .tail.map(s => s.split(" ").toList.tail.map(_.toInt).toSet).toSet
    write(if (!S.contains(Set()) ||
              !S.forall(subset => subset.subsets.forall(S.contains)) ||
              !S.subsets(2).filter(s => s.head.size > s.tail.head.size)
                .forall(p => (p.head -- p.tail.head).exists(x => S.contains(p.tail.head + x)))) "NO"
          else "YES")
  }
}
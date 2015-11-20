package labs.sortsearch

import scala.io.Source

/**
 * @author volhovm
 *         Created on 5/26/14
 */
object E_radix {
  def main(args: Array[String]) {
    val in = Source.fromFile("radixsort.in").getLines
    val out = new java.io.PrintWriter(new java.io.File("radixsort.out"))
    val k = in.next().split(" +")(2).toInt
    val arr = in.toList.sortWith((a : String, b : String) => a.takeRight(k) < b.takeRight(k))
    out.print(arr.mkString("\n"))
    out.close()
  }
}

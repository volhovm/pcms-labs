package labs.sortsearch

import java.io.{PrintWriter, FileReader, BufferedReader, File}

/**
 * @author volhovm
 *         Created on 5/24/14
 */

object F_antiQuicksort {
  def main(args: Array[String]) {
    val scin = new BufferedReader(new FileReader("antiqs.in"))
    val scout = new PrintWriter(new File("antiqs.out"))
    val n : Int = scin.readLine().toInt
    var args: Array[Int] = Range(1, n + 1).toArray
    for (x <- 2 to n - 1) {
      args = swap(args, x, x / 2)
    }
    for (x <- args) {
      scout.print(x + " ")
    }
    scout.close()
  }

  def swap(arr: Array[Int], a: Int, b: Int) : Array[Int] = {
    val temp = arr(a)
    arr(a) = arr(b)
    arr(b) = temp
    arr
  }
}

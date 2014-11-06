#!/bin/sh
exec scala "$0" "$@"
!#
object HelloWorld extends App {
	var n = args.length
	if (n < 2) System.exit(1)
	var dir: java.io.File = new java.io.File(args(0))
	if (!dir.mkdir()) print("Could not create dir")
	else {
       var template = scala.io.Source.fromFile("pattern.cpp").getLines.toList
	   for (i <- 1 until n) {
		   val cppWriter = new java.io.PrintWriter(new java.io.File(args(0) + "/" + args(i) + ".cpp"))
		   try {
		       template.map(a => a.replace("$$name$$", args(i))).foreach(cppWriter.println(_))
		   } finally { cppWriter.close() } 
	   } 
	}
}
HelloWorld.main(args)

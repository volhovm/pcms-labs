antlr = antlr-4.5.3-complete.jar

all: antlr javac

antlr: Functional.g
	java -jar $(antlr) Functional.g

javac: antlr
	javac -classpath "$(antlr):." Functional*.java Helpers.java

test: antlr primtest.sf
	java -classpath "$(antlr):." org.antlr.v4.gui.TestRig Functional topLevel -gui primtest.sf

clean:
	rm -rf Functional*java
	rm -rf *tokens
	rm -rf *class

import java.io.*;
import java.util.*;
import java.util.stream.*;
import org.antlr.v4.runtime.*;

public class Helpers {
    public static int lol = 5;
    public static int indentLvl = 0;
    public static String indent(String s) {
        String ind = "  ";
        return ind + s.trim().replaceAll("\n", "\n" + ind);
    }
    public static void showResult(List<FunctionalParser.FuncContext> topLvl) {
        System.out.println(topLvl.stream().map(a -> a.res).collect(Collectors.joining("\n")));
    }
    public static String genFunc(String fooname,
                                 List<FunctionalParser.DeclContext> decls) {
        String ret = "";
        List<String> args = decls.get(0).args.res;
        ret += "def " + fooname + "(";
        for (int i = 0; i < args.size(); i++) {
            ret += "_" + i;
            if (i != args.size() - 1) ret += ", ";
        }
        ret += "):\n";
        for (int i = 0; i < decls.size(); i++) {
            //            System.out.println("Lolwhat? " + decls.get(i).res);
            ret += indent(decls.get(i).res);
            if (i != decls.size() - 1) ret += "\n";
        }
        return ret;
    }
    public static List<String> genArgs(List<Token> args) {
        return args.stream()
            .map(x -> { if (x.getText().equals("_")) return null; else return x.getText(); })
            .collect(Collectors.toList());
    }
    public static void checkArgs(String fooname, int expected, int got) {
        if (expected != got) throw new RuntimeException("Function " + fooname +
                                                        " expected " + String.valueOf(expected) +
                                                        " arguments, got " + String.valueOf(got));
    }
    public static String genDecl(List<String> args, String mainTerm) {
        String ret = "";
        for (int i = 0; i < args.size(); i++) {
            if (args.get(i) == null) continue;
            ret += args.get(i) + " = _" + i + "\n";
        }
        ret += mainTerm;
        return ret;
    }
    public static String genDecl(List<String> args, String condition, String mainTerm) {
        String ret = "";
        for (int i = 0; i < args.size(); i++) {
            if (args.get(i) == null) continue;
            ret += args.get(i) + " = _" + i + "\n";
        }
        ret += "if " + condition + ":\n";
        ret += indent(mainTerm);
        return ret;
    }
    public static String genScoped(String term, List<FunctionalParser.FuncContext> sub) {
        String ret = "";
        for (int i = 0; i < sub.size(); i++) {
            ret += sub.get(i).res + "\n";
        }
        ret += term + "\n";
        return ret;
    }
    public static String genIfThenElse(String cond, String l, String r) {
        return "if " + cond + ":\n" + indent(l) + "else:\n" + indent(r);
    }
    public static String genInfix(String infix, String lhs, String rhs) {
        return lhs + " " + infix + " " + rhs;
    }
    public static String genListConstr(List<FunctionalParser.PrimValueContext> values) {
        return "[" + values.stream().map(a -> a.res).collect(Collectors.joining(",")) + "]";
    }
    public static String genApplication(String a, List<FunctionalParser.PrimValueContext> values) {
        String ret = a + "(";
        for (int i = 0; i < values.size(); i++) {
            ret += values.get(i).res;
            if (i < values.size() - 1) ret += ", ";
        }
        ret += ")";
        return ret;
    }
    public static String genFuncOrVar(String a, List<String> freeVars, boolean asArgs) {
        if (asArgs) return a;
        for (int i = 0; i < freeVars.size(); i++) {
            if (freeVars.get(i) == null) continue;
            if (freeVars.get(i).equals(a)) {
                return a;
            }
        }
        return a + "()";
    }
}

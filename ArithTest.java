
/**
 * @author chris stephenson
 *
 */
public class ArithTest {
   /**
    * @param args
    */
   public static int calcAnswer(String op, int a, int b) {
      if (op.compareTo("+") == 0) {
         return a + b; 
      } else if (op.compareTo("x") == 0) {
         return a * b;
      } else {
         return 0;
      }
   }
   public static void main(String[] args) {
      if (args.length == 3) {
         System.out.println(args[0] + " " + args[1] + " " + args[2] + " = " +
          calcAnswer(args[1], Integer.parseInt(args[0]),Integer.parseInt(args[2])));     
      } else {
         System.out.println("Wrong number of arguments");
      }
   }
}

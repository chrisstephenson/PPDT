package binomialcoefficient;
import acm.graphics.*;
import acm.program.*;
import java.math.*;

/*
* File: BinomialProgram.java (Exactly as printed in the book as Combinations.java)
* -----------------------
* This program computes the mathematical combinations function
* C(n, k), which is the number of ways of selecting k objects
* from a set of n distinct objects.
*/
public class BinomialProgram extends ConsoleProgram
{
   /*
    * Returns the mathematical combinations function C(n, k),
    * which is the number of ways of selecting k objects
    * from a set of n distinct objects.
    */
   public int combinations(int n, int k) {
      return factorial(n) / (factorial(k) * factorial(n - k));
   }

   /*
    * Returns the factorial of n, which is defined as the
    * product of all integers from 1 up to n.
    */
   private int factorial(int n) {
      int result = 1;
      for (int i = 1; i <= n; i++) {
         result *= i;
      }
      return result;
   }


/*
 * Done better by yours truly
 */
/*
 * computeBinomialCoefficient take n k as ints returns nCk
 * Method: since nCk and nC(n-k) are the same number, minimise the calculation by choosing the smaller of the two.
 * Eliminate the common factors top and bottom, then perform multiplications by factors on top  and divisions by factors on the bottom alternately
 * thus significantly reducing the risk of integer overflow. 
 * Note: even this method may still overflow on some cases where the correct answer is < MAXINT. 
 */
public int computeBinomialCoefficient(int n, int k) 
{
   int ans = 1;
   k = Math.min(k, n - k);
   for (int i = 0; i < k; i++)
   {
      ans = ans * (n - k + 1 + i);
      ans = ans / (i + 1);
   }
   return ans;
}

public void printFailure(String name, int n, int k, int ans, int calculated) {
   println (name + " got it wrong for n=" + n + ", k = " + k + ", correct answer was " + ans + ", calculated answer was "  + calculated);
}

public void singleTest(int n, int k, int ans) {
   if (k < n) {
      if (combinations(n,k) != ans) {
         printFailure("Roberts", n, k , ans, combinations(n,k));
      }
      if (computeBinomialCoefficient(n,k) != ans) {
         printFailure("Stephenson", n, k , ans, computeBinomialCoefficient(n,k));
      }
   }
}

public void tests(int [][] testData) {
   for (int i = 0; i < testData.length; i++ ) {
      singleTest (testData[i][0],testData[i][1],testData[i][2]);
   }
}

public void run() {
   int [][]testData = {{10,1,10},{15,1,15},{20,1,20},{10,2,45},{15,2,105},{20,2,190},{10,3,120},{15,3,455},{20,3,1140}};
   //tests(testData);
   int n = readInt("Enter number of objects in the set (n): ");
   int k = readInt("Enter number to be chosen (k): ");
   while (n > 0) { 
      /*println("Roberts thinks that C(" + n + ", " + k + ") = " + combinations(n, k));
      println("Stephenson thinks that C(" + n + ", " + k + ") = " + computeBinomialCoefficient(n, k));*/
      println("C(" + n + ", " + k + ") = " + combinations(n, k));
      n = readInt("Enter number of objects in the set (n): ");
      k = readInt("Enter number to be chosen (k): ");
      }
}



}

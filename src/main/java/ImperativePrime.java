import java.util.ArrayList;
import java.util.Collection;

public class ImperativePrime {
    static public boolean isPrime(int n) {
        return getFactors(n).size() <= 2;
    }

    private static Collection<Integer> getFactors(int n) {
        double sqrt = Math.sqrt((double) n);
        Collection<Integer> c = new ArrayList<>();
        for(int i = 1; i <= sqrt; ++i) {
            double j = (double)n/(double)i;
            if (j - (int)j < 0.00001d) {
                c.add(i);
                c.add((int)j);
            }
        }
        return c;
    }
}

package primes;


public class PrimeSieveJ {

    public static int[] primes(int n) {

        // initially assume all integers are prime
        boolean[] isPrime = new boolean[n + 1];

        for (int i = 2; i <= n; i++) {
            isPrime[i] = true;
        }

        // mark non-primes <= N using Sieve of Eratosthenes
        for (int i = 2; i*i <= n; i++) {

            // if i is prime, then mark multiples of i as nonprime
            // suffices to consider mutiples i, i+1, ..., N/i
            if (isPrime[i]) {
                for (int j = i; i * j <= n; j++) {
                    isPrime[i * j] = false;
                }
            }
        }

        // count primes
        int primeCount = 0;
        for (int i = 2; i <= n; i++) {
            if (isPrime[i]) primeCount++;
        }

        System.out.println("The number of primes <= " + n + " is " + primeCount);

        int[] primes = new int[primeCount];
        int idx = 0;
        for (int i = 0; i < n; i++) {
            if (isPrime[i] == true) {

                primes[idx] = i;
                idx++;
            }
        }

        return primes;
    }

    public static void main(String[] args) {

        int n = 100;

        if (args.length > 0 && args[0] != null) {
            n = Integer.parseInt(args[0]);
        }

        int[] primes = primes(n);

        for (int p : primes) {
            System.out.println("Prime: " + p);
        }
    }
}

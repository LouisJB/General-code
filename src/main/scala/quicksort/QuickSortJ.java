package quicksort;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class QuickSortJ {

    public static <E extends Comparable> List<E> sortI(List<E> xs)
    {
        if (xs.size() <= 1)
            return xs;
        else {

          E pivot = (E)xs.get(xs.size() / 2);

          // initialize lists
          List<E> lows = new ArrayList<E>();
          List<E> mids = new ArrayList<E>();
          List<E> highs = new ArrayList<E>();

          for (E item : xs) {

            // sort item into low, mid or high list
            int c = item.compareTo(pivot);
            if (c == 0)
                mids.add(item);
            else if (c < 0)
                lows.add(item);
            else
                highs.add(item);
          }

          // return sorted list appending chunks

          //sortI(lows) ::: mids ::: sort(highs)
          List<E> res = new ArrayList<E>();
          res.addAll(sortI(lows));
          res.addAll(mids);
          res.addAll(sortI(highs));
          
          return res;
        }
    }

    // main - test cases
    public static void main(String[] args) {

        List<String> r1 = sortI(Arrays.asList("Berlin", "Paris", "Barcelona", "Amsterdam"));

        assert(r1.equals(Arrays.asList("Amsterdam", "Barcelona", "Berlin", "Paris")));

        List<String> r2 = sortI(Arrays.asList("caa", "cab", "baa", "aaa", "aab"));

        assert(r2.equals(Arrays.asList("aaa", "aab", "baa", "caa", "cab")));

        List<Integer> r3 = sortI(Arrays.asList(77, 66, 78, 44, 55, 33, 22, 55, 22, 11, 1));

        assert(r3.equals(Arrays.asList(1, 11, 22, 22, 33, 44, 55, 55, 66, 77, 78 )));
    }
}

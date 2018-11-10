# haskalgs

This repository has/will have implementations of various algorithms which range from basic to advanced and all are implemented in [Haskell](https://www.haskell.org/). Obviously, most, if not all, algorithms use/will use recursion.

---

_One of the primary goals of the repository is to have minimal, yet complete, implementations of algorithms._

---

## Current implementations
_Searching algorithms_
* [Binary search](https://github.com/oniani/haskalgs/blob/master/src/searching/BinarySearch.hs)
* [Exponential search](https://github.com/oniani/haskalgs/blob/master/src/searching/ExponentialSearch.hs)
* [Interpolation search](https://github.com/oniani/haskalgs/blob/master/src/searching/InterpolationSearch.hs)
* [Linear search](https://github.com/oniani/haskalgs/blob/master/src/searching/LinearSearch.hs)

_Sorting algorithms_
* [Bubble sort](https://github.com/oniani/haskalgs/blob/master/src/sorting/BubbleSort.hs)
* [Insertion sort](https://github.com/oniani/haskalgs/blob/master/src/sorting/InsertionSort.hs)
* [Merge sort](https://github.com/oniani/haskalgs/blob/master/src/sorting/MergeSort.hs)
* [Quicksort](https://github.com/oniani/haskalgs/blob/master/src/sorting/Quicksort.hs)
* [Selection Sort](https://github.com/oniani/haskalgs/blob/master/src/sorting/SelectionSort.hs)

_Sequence generation algorithms_
* [Calkin-Wilf Sequence Generator](https://github.com/oniani/haskalgs/blob/master/src/sequence-generation/CalkinWilfGenerator.hs)
* [Catalan Sequence Generator](https://github.com/oniani/haskalgs/blob/master/src/sequence-generation/CatalanGenerator.hs)
* [Collatz Sequence Generator](https://github.com/oniani/haskalgs/blob/master/src/sequence-generation/CollatzGenerator.hs)
* [Stern's Diatomic Sequence Generator](https://github.com/oniani/haskalgs/blob/master/src/sequence-generation/DiatomicGenerator.hs)
* [Fibonacci Sequence Generator](https://github.com/oniani/haskalgs/blob/master/src/sequence-generation/FibonacciGenerator.hs)
* [Prime Sequence Generator](https://github.com/oniani/haskalgs/blob/master/src/sequence-generation/PrimeGenerator.hs)

_Miscellaneous algorithms_
* [Base Converter](https://github.com/oniani/haskalgs/blob/master/src/miscellaneous/BaseConverter.hs)


It should be noted that current algorithms do not have accompanying [QuickCheck](http://hackage.haskell.org/package/QuickCheck) tests which are the must!  
I will try to update the list and add the tests as I have time.

## License
[MIT](https://github.com/oniani/haskalgs/blob/master/LICENSE)

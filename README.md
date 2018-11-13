# Algorithms

This repository has the implementations of various algorithms which range from basic to advanced and all are implemented in [Haskell](https://www.haskell.org/). Obviously, most, if not all, algorithms use recursion. As of now, the primary goal of the repository is to have a curated list of high-performance implementations of the well-known and lesser-known algorithms.

### Why Haskell?
Haskell allows us to embrace the beautiful ideas of function reusability and complexity reduction. In other words, functions
can be treated as variables which opens up a whole new world of algorithms. Besides, its lazy evaluation makes the algorithms extremely fast by not computing the values which are not going to be used.

## Current implementations
_Geometric algorithms_
* [Line Algorithms](https://github.com/oniani/algorithms/blob/master/src/geometry/Line.hs)
    * Initial value
    * Slope
    * Function generation
    * Parallelism
    * Intersection
* [Triangle algorithms](https://github.com/oniani/algorithms/blob/master/src/geometry/Triangle.hs)
    * Existence
    * Perimeter
    * Area
    * Bisectors
    * Heights
    * Medians
    * Inradius
    * Circumradius
    * Exradii
    * Sines
    * Cosines
    * Angles (degrees)
    * Angles (radians)

_Searching algorithms_
* [Binary search](https://github.com/oniani/algorithms/blob/master/src/searching/BinarySearch.hs)
* [Exponential search](https://github.com/oniani/algorithms/blob/master/src/searching/ExponentialSearch.hs)
* [Interpolation search](https://github.com/oniani/algorithms/blob/master/src/searching/InterpolationSearch.hs)
* [Linear search](https://github.com/oniani/algorithms/blob/master/src/searching/LinearSearch.hs)

_Sorting algorithms_
* [Bubble sort](https://github.com/oniani/algorithms/blob/master/src/sorting/BubbleSort.hs)
* [Insertion sort](https://github.com/oniani/algorithms/blob/master/src/sorting/InsertionSort.hs)
* [Merge sort](https://github.com/oniani/algorithms/blob/master/src/sorting/MergeSort.hs)
* [Quicksort](https://github.com/oniani/algorithms/blob/master/src/sorting/Quicksort.hs)
* [Selection Sort](https://github.com/oniani/algorithms/blob/master/src/sorting/SelectionSort.hs)

_Sequence generation algorithms_
* [Calkin-Wilf Sequence Generator](https://github.com/oniani/algorithms/blob/master/src/sequence-generation/CalkinWilfGenerator.hs)
* [Catalan Sequence Generator](https://github.com/oniani/algorithms/blob/master/src/sequence-generation/CatalanGenerator.hs)
* [Collatz Sequence Generator](https://github.com/oniani/algorithms/blob/master/src/sequence-generation/CollatzGenerator.hs)
* [Stern's Diatomic Sequence Generator](https://github.com/oniani/algorithms/blob/master/src/sequence-generation/DiatomicGenerator.hs)
* [Fibonacci Sequence Generator](https://github.com/oniani/algorithms/blob/master/src/sequence-generation/FibonacciGenerator.hs)
* [Prime Sequence Generator](https://github.com/oniani/algorithms/blob/master/src/sequence-generation/PrimeGenerator.hs)

_Miscellaneous algorithms_
* [Base Converter](https://github.com/oniani/algorithms/blob/master/src/miscellaneous/BaseConverter.hs)

## License
[MIT](https://github.com/oniani/algorithms/blob/master/LICENSE)

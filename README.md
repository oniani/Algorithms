# Algorithms

This repository has the implementations of various algorithms and data structures which range from basic to advanced and all are implemented in [Haskell](https://www.haskell.org/). Obviously, most, if not all, algorithms and data structures use recursion in some way. As of now, the primary goal of the repository is to have a curated list of high-performance implementations of the well-known and lesser-known algorithms
and data structures.

### Why Haskell?
Haskell allows us to embrace the beautiful ideas of function reusability and complexity creation. In other words, functions
can be treated as variables which opens up a whole new world of algorithms and data structures. By treating the functions as the first class citizens, Haskell evangelizes the idea of breaking down a problem into simple steps and dealing with the smaller sub-problems making the problem-solving a lot more efficient. Besides, its lazy evaluation makes the algorithms extremely fast by not computing the values which are not going to be used.

## Current implementations

### Data Structures
* [Stack](https://github.com/oniani/purity/blob/master/src/data-structures/Stack.hs)
* [Queue](https://github.com/oniani/purity/blob/master/src/data-structures/Queue.hs)
* [Binary tree node](https://github.com/oniani/purity/blob/master/src/data-structures/BinaryTreeNode.hs)


### Algorithms

_Geometric algorithms_
* [Line Algorithms](https://github.com/oniani/purity/blob/master/src/algorithms/geometry/Line.hs)
    * Initial value
    * Slope
    * Function generation
    * Parallelism
    * Intersection
* [Triangle algorithms](https://github.com/oniani/purity/blob/master/src/algorithms/geometry/Triangle.hs)
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
* [Binary search](https://github.com/oniani/purity/blob/master/src/algorithms/searching/BinarySearch.hs)
* [Exponential search](https://github.com/oniani/purity/blob/master/src/algorithms/searching/ExponentialSearch.hs)
* [Interpolation search](https://github.com/oniani/purity/blob/master/src/algorithms/searching/InterpolationSearch.hs)
* [Linear search](https://github.com/oniani/purity/blob/master/src/algorithms/searching/LinearSearch.hs)

_Sorting algorithms_
* [Bubble sort](https://github.com/oniani/purity/blob/master/src/algorithms/sorting/BubbleSort.hs)
* [Insertion sort](https://github.com/oniani/purity/blob/master/src/algorithms/sorting/InsertionSort.hs)
* [Merge sort](https://github.com/oniani/purity/blob/master/src/algorithms/sorting/MergeSort.hs)
* [Quicksort](https://github.com/oniani/purity/blob/master/src/algorithms/sorting/Quicksort.hs)
* [Selection Sort](https://github.com/oniani/purity/blob/master/src/algorithms/sorting/SelectionSort.hs)

_Sequence generation algorithms_
* [Arithmetic Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/ArithmeticGenerator.hs)
* [Calkin-Wilf Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/CalkinWilfGenerator.hs)
* [Catalan Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/CatalanGenerator.hs)
* [Collatz Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/CollatzGenerator.hs)
* [Stern's Diatomic Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/DiatomicGenerator.hs)
* [Fibonacci Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/FibonacciGenerator.hs)
* [Geometric Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/GeometricGenerator.hs)
* [Prime Sequence Generator](https://github.com/oniani/purity/blob/master/src/algorithms/sequence-generation/PrimeGenerator.hs)

_Miscellaneous algorithms_
* [Base Converter](https://github.com/oniani/purity/blob/master/src/algorithms/miscellaneous/BaseConverter.hs)

## License
[MIT](https://github.com/oniani/algorithms/blob/master/LICENSE)

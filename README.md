# AADS 2022 - Project 1

In this mini-project, you will be provided less structure
for the code you write. The objective is to evaluate
the level of understanding of the concepts presented
throughout the course so far, and your ability to
apply these concepts in solving new problems.

The main concepts tested in this project are:
* Immutable (persistent) data structures
* Amortized complexity

## Problem Statememt

Design an **immutable array data structure** that exhibits logarithmic time
amortized complexity for the `append` operation, while retaining
logarithmic (or near-logarithmic) complexity for the 
access and update operations.

That is, assuming we have *k*
arrays of size *n*, and that we perform *k-1* append operations to append
all arrays together, the total time complexity is at most *O(log(k\*n))*.

## Deliverables

* Your immutable array implementation
* Testing code that evaluates the performance of your solution
* A **report** explaining your solution, justifying your choices,
  and interpreting the results of your tests. As you explain your solution,
  you should try to critique it objectively, highlighting both its
  advantages and its downsides.

## Hints
* Use Assignment 3 as a starting point. Limit your bucket sizes to 4, 
  so your code doesn't grow overly tedious (otherwise you'll spend a 
  lot of time just dealing with boilerplate code)
* In Assignment 3, subtrees always have equal height. Is that really
  necessary, or can this constraint be relaxed?
* The trick for efficient amortized complexity is to create fast operations
  (say constant time) that nevertheless allow the data structure to **deviate
  a little** from its ideal shape. 
   * With each such deviation, the complexity
     of the next operation **may increase a bit**, adding up, after a few 
     (or more than a few) such
     operations, to **significant performance degradation**.
   * Thus, after executing the operation several times, it is important
     to trigger a **clean-up action**.
   * The clean-up action may be of worse complexity, but when divided
     up by the number of operations performed so far, it may still result
     in **good amortized complexity**.
   * You then need to find the **right trade-off on the frequency** of triggering
     the clean-up action, so as to **maximize the amortized complexity**.

## Submission

A GitHub template has been provided for this project.
Please fill your solution code in the `Solution.scala` file,
and your tests in the `Test.scala` file.

Ideally, you would write your report as a Markdown document,
that would be readable directly on the GitHub site.
A `/doc/Report.md` has been provided as a template for this purpose.
You may upload images into the same folder and refer to them
via the standard image link for MD docs (see the template).

Nevertheless, it would also be acceptable to submit
your report as a PDF document, into the same folder.

When you are ready to submit, simply tag your latest commit as a release. 
There is nothing else you need to do.

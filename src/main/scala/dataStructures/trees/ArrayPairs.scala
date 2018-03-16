package dataStructures.trees
//
//import scala.annotation.meta.getter
//import scala.annotation.tailrec
//import scala.collection.Iterator
//import scala.collection.immutable.{RedBlackTree, TreeMap, TreeSet}

import java.util

import struct.BinaryTree

object ArrayPairs {

  util.TreeSet


  def canPair(num: Int, max: Int) = max / num

  def solution(array: Seq[Int]) = {
    val max = array.max
    val (tree, sum) = array.reverse.foldLeft((null: BinaryTree.Empty[Int]) -> 0) {
      case ((tree, sum), num) =>
        val pair = canPair(num, max)
        val newSum = RedBlackTree.count(RedBlackTree.to(tree, pair)) + sum
        val newTree = RedBlackTree.update(tree, num, (), false)
        newTree -> newSum
    }
    sum
  }
}

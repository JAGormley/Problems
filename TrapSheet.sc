// Scala solution to trapping rainwater problem https://leetcode.com/problems/trapping-rain-water/

object Trap {

  def trapWater(landscape: List[Int]): Int = {
    if (landscape.length < 3) {
      return 0
    }
    if (landscape.head <= landscape.tail.head) {
      return trapWater(landscape.tail)
    }
    else return getWater(landscape)
  }

  def getWater(landscape: List[Int]): Int = {
    val cutIndex = findCutIndex(landscape, 0)
    if (cutIndex == 0) {
      return 0
    }
    return getWaterAmt(landscape.take(cutIndex+1)) + trapWater(landscape.drop(cutIndex - 1))
  }

  def findCutIndex(landscape: List[Int], indexAcc: Int): Int = {
    if (landscape.tail.isEmpty) {
      return 0
    }
    if (landscape.head > landscape.tail.head) {
      return findCutIndex(landscape.tail, indexAcc + 1);
    }
    else return findNextPeak(landscape.tail, indexAcc + 1)
  }

  def findNextPeak(landscape: List[Int], indexAcc: Int): Int = {
    if (landscape.tail.isEmpty || landscape.head > landscape.tail.head) {
      return indexAcc
    }
    else return findNextPeak(landscape.tail, indexAcc+1)
  }

  def getWaterAmt(well: List[Int]): Int = {
    val wellHeight = (List(well.head,well.last)).min
    return getWaterAmtHelper(well.tail, wellHeight, 0)
  }

  def getWaterAmtHelper(well: List[Int], wellHeight: Int, acc: Int) : Int = {
    if (well.tail.isEmpty){
      return acc
    }
    else return getWaterAmtHelper(well.tail, wellHeight, acc+(wellHeight-well.head))
  }
}


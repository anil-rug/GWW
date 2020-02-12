package rentomatic

import java.util.Calendar

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Rent {

  def main(args: Array[String]): Unit = {
    val available = mutable.Map(
      VehicleType.car -> 10,
      VehicleType.bike -> 5,
      VehicleType.motorbike -> 5,
      VehicleType.boat -> 3
    )
    val now = Calendar.getInstance()

    val rented = mutable.Map(
      VehicleType.car -> (0, List()),
      VehicleType.bike -> (0, now.get(Calendar.MINUTE)),
      VehicleType.motorbike -> (0, now.get(Calendar.MINUTE)),
      VehicleType.boat -> (0, now.get(Calendar.MINUTE))
    )

    val rentedList = mutable.ListBuffer[(Int, Int)]()

    /* The duration of time the vehicle is rented*/
    val threshold = 0;

    printWithHeader("SELECT THE VEHICLE YOU WANT TO RENT")
    printWithHeader("AVAILABLE")
    printVehicles(available)
    printWithHeader("SELECTION = \n 1 -> Car \n 2 -> Bike\n 3 -> Motorbike\n 4 -> Boat\n 0-> REFRESH\n 100 -> display vehicles available for rent\n 999 -> QUIT \n")
    try {
      var choice = scala.io.StdIn.readLine().toInt
      while (choice != 999) {
        choice match {
          case 100 => printVehicles(available)
          case 0 => refreshTheList(available, rentedList, threshold)
                    printVehicles(available)
          case VehicleType.car => {
            if (available.getOrElse(VehicleType.car, 0) > 0) {
              printWithHeader("RENTED A CAR FOR 2550 EURO")
              println("Enter the amount = ")
              //rented.update(VehicleType.car, (rented.getOrElse(VehicleType.car, (0, 0))._1 + 1, now.get(Calendar.MINUTE)))
              takeMoney(2550)
              rentedList.append((VehicleType.car, now.get(Calendar.MINUTE)))
              available.update(VehicleType.car, (available.getOrElse(VehicleType.car, 0) - 1))
            }
            else {
              printWithHeader("NO CARS AVAILABLE")
            }
          }
          case VehicleType.bike => {
            if (available.getOrElse(VehicleType.bike, 0) > 0) {
              printWithHeader("RENTED A bike FOR 875 EURO")
              //rented.update(VehicleType.bike, (rented.getOrElse(VehicleType.bike, (0, 0))._1 + 1, now.get(Calendar.MINUTE)))
              takeMoney(875)
              rentedList.append((VehicleType.bike, now.get(Calendar.MINUTE)))
              available.update(VehicleType.bike, (available.getOrElse(VehicleType.bike, 0) - 1))
            }
            else {
              printWithHeader("NO bikes AVAILABLE")
            }
          }
          case VehicleType.motorbike => {
            if (available.getOrElse(VehicleType.motorbike, 0) > 0) {
              printWithHeader("RENTED A motorbike FOR 2145 EURO")
              //rented.update(VehicleType.motorbike, (rented.getOrElse(VehicleType.motorbike, (0, 0))._1 + 1, now.get(Calendar.MINUTE)))
              takeMoney(2145)
              rentedList.append((VehicleType.motorbike, now.get(Calendar.MINUTE)))
              available.update(VehicleType.motorbike, (available.getOrElse(VehicleType.motorbike, 0) - 1))
            }
            else {
              printWithHeader("NO motorbikes AVAILABLE")
            }
          }
          case VehicleType.boat => {
            if (available.getOrElse(VehicleType.boat, 0) > 0) {
              printWithHeader("RENTED A boat FOR 5875 EURO")
              //rented.update(VehicleType.boat, (rented.getOrElse(VehicleType.boat, (0, 0))._1 + 1, now.get(Calendar.MINUTE)))
              takeMoney(5875)
              rentedList.append((VehicleType.boat, now.get(Calendar.MINUTE)))
              available.update(VehicleType.boat, (available.getOrElse(VehicleType.boat, 0) - 1))
            }
            else {
              printWithHeader("NO boat AVAILABLE")
            }
          }
          case _ => printWithHeader("WRONG CHOICE")
        }
        printWithHeader("SELECTION = \n 1 -> Car \n 2 -> Bike\n 3 -> Motorbike\n 4 -> Boat\n 0-> REFRESH\n 100 -> display vehicles available for rent\n 999 -> QUIT\n")
        choice = scala.io.StdIn.readLine().toInt
      }
    }
    catch {
      case e: NumberFormatException => println("Invalid Choice, Not a numeric option")
    }

    def refreshTheList(av: mutable.Map[Int, Int], re: ListBuffer[(Int, Int)], threshold: Int): Unit = {
      printWithHeader("REFRESHING THE VEHICLES AVAILABLE FOR RENT")
      for (i <- 1 until re.length) {
        val difference = (now.get(Calendar.MINUTE) - re(i)._2)
        if (difference >= threshold) {
          available.update(re(i)._1, av.getOrElse(re(i)._1, 0) + 1)
          re.drop(i)
        }
      }
    }

  }

  def takeMoney(expected: Int): Unit = {
    printWithHeader("PROVIDE THE MONEY")
    var cost = scala.io.StdIn.readLine().toInt
    while (cost < expected) {
      printWithHeader("INSUFFICIENT MONEY RE-ENTER")
      cost = scala.io.StdIn.readLine().toInt
    }
    get_change(cost-expected)
  }

  def printWithHeader(str: String): Unit = {
    println("#" * 15 + " " + str + " " + "#" * 15)
  }

  def printVehicles(map: mutable.Map[Int, Int]): Unit = {
    map.keys.foreach(key => println(VehicleType.reverseMap(key) + " = " + map.getOrElse(key, 0)))
  }

  def get_change(n: Int): Unit = {
    val coins = Array(1, 2, 5, 10, 20, 50, 100, 200, 500)
    var r = n
    val myMapCoins = mutable.Map(
      (1 -> 0),
      (2 -> 0),
      (5 -> 0),
      (10 -> 0),
      (20 -> 0),
      (50 -> 0),
      (100 -> 0),
      (200 -> 0),
      (500 -> 0)
    )
    var i = coins.length - 1
    while (i > 0) {
      if ((r / coins(i)) > 0) myMapCoins.update(coins(i), myMapCoins.getOrElse(coins(i), 0) + (r / coins(i)).toInt)
      r = r % coins(i)
      i -= 1
    }
    printWithHeader("RETURN CHANGE")
    myMapCoins.foreach(entry => println("Coin = " + entry._1 + " Count = " + entry._2))
    printWithHeader("")
  }
}

object VehicleType {
  val car = 1
  val bike = 2
  val motorbike = 3
  val boat = 4
  val refresh = 100
  val reverseMap = Map(
    car -> "Car",
    bike -> "bike",
    motorbike -> "motorbike",
    boat -> "boat"
  )
}
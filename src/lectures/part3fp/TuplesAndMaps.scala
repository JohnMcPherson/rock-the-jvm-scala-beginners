package lectures.part3fp

import scala.annotation.tailrec

class SocialNetwork (val persons: List[String] = List(), val friends : Map[String, String] = Map()) {
  def addPerson(newPerson: String) : SocialNetwork = {
      val newPersons: List[String] = persons.prepended(newPerson)
      new SocialNetwork(newPersons, friends)
  }
}

object TuplesAndMaps extends App {
  val jimMap = Map("JIM" -> 900, "Jim" -> 555)
  println(jimMap)
  val newJimMap: Map[String, Int] = jimMap.map(pair => pair._1.toLowerCase -> pair._2)
  println(newJimMap)

  def addPerson(network :Map[String, Set[String]], newPerson : String) :  Map[String, Set[String]]= {
     network + (newPerson ->  Set())
  }

  val emptyNetwork : Map[String, Set[String]] = Map()
  val nwJohnBillMary = addPerson(addPerson(addPerson(emptyNetwork, "John"), "Bill"), "Mary")
  println("nwJohnBillMary = " + nwJohnBillMary)

  def removePerson(network :Map[String, Set[String]], removedPerson : String) :  Map[String, Set[String]]= {
    @tailrec
    def unfriendRemainingFriendsOfPersonToRemove(remainingFriends : Set[String], interimNetwork : Map[String, Set[String]]) : Map[String, Set[String]] =
      if (remainingFriends.isEmpty) interimNetwork
      else unfriendRemainingFriendsOfPersonToRemove(remainingFriends.tail, unfriend(interimNetwork, remainingFriends.head, removedPerson))

    val nwWithPersonToRemoveCompletelyUnfriended = unfriendRemainingFriendsOfPersonToRemove(network(removedPerson), network)
    nwWithPersonToRemoveCompletelyUnfriended - removedPerson
  }

  def johnAndBillOnly : Map[String, Set[String]] = removePerson(nwJohnBillMary, "Mary")
  println("J & B only: "+ johnAndBillOnly)

  def friend (network :Map[String, Set[String]], person1 : String, person2 : String) :  Map[String, Set[String]] = {
    if (network.contains(person1) && network.contains(person2)) {
      val friends1with2Added = network(person1) + person2
      val friends2with1Added = network(person2) + person1
      network + (person1 -> friends1with2Added) + (person2 -> friends2with1Added)
    } else network
  }

  def unfriend (network :Map[String, Set[String]], person1 : String, person2 : String) :  Map[String, Set[String]] = {
    if (network.contains(person1) && network.contains(person2)) {
      val friends1with2Removed = network(person1) - person2
      val friends2with1Removed = network(person2) - person1
      network + (person1 -> friends1with2Removed) + (person2 -> friends2with1Removed)
    }
    else network
  }


  /*{
    def conditionalMapIntoList: ((String, Set[String]), String, String) => (String, Set[String]) =
      (mapItem, stringToMatch, stringToAdd) =>
        if (mapItem._1.equals(stringToMatch)) (mapItem._1, mapItem._2.prepended(stringToAdd))
        else mapItem

    def mapUnconditional = {
      network
        .map(it =>
          conditionalMapIntoList(it, firstPerson, secondPerson)
        )
        .map(it =>
          conditionalMapIntoList(it, secondPerson, firstPerson)
        )
    }

    if (isInNetwork(network, firstPerson) && isInNetwork(network, secondPerson))
    mapUnconditional
    else network
  }

   */

/*  def unFriend (network :Map[String, List[String]], firstPerson : String, secondPerson : String) :  Map[String, List[String]] = {
    def conditionalUnMapFromList: ((String, List[String]), String, String) => (String, List[String]) =
      (mapItem, stringToMatch, stringToRemove) =>
        if (mapItem._1.equals(stringToMatch)) (mapItem._1, mapItem._2.filter(it => !it.equals(stringToRemove)))
        else mapItem

    network
      .map(it =>
        conditionalUnMapFromList(it, firstPerson, secondPerson)
      )
      .map(it =>
        conditionalUnMapFromList(it, secondPerson, firstPerson)
      )
  }

  def numFriendsOfPerson (network :Map[String, List[String]], person : String) : Int = {
    if (isInNetwork(network, person)) network.find(_._1.equals(person)).map(_._2.length).get
    else 0
  }

  def personWithMostFriends (network :Map[String, List[String]]) : String =
    network.maxBy(it => numFriendsOfPerson(network, it._1))._1

  def isInNetwork(network: Map[String, List[String]], testName : String ) : Boolean =
    network.contains(testName)

  def numPeopleNoFriends(network: Map[String, List[String]]) : Int =
    network.count(it => numFriendsOfPerson(network, it._1)==0)

  def hasSocialConnection(network: Map[String, List[String]], firstPerson: String, secondPerson: String): Boolean = {
    @tailrec
    def hasSocialConnectionHelper(personToTest: String, remainingDirectConnectonsToTest: List[String], peopleChecked: List[String] = List()): Boolean = {
      if (personToTest.equals(secondPerson)) true else {
        if (remainingDirectConnectonsToTest.isEmpty) return false
        val itemToTest = remainingDirectConnectonsToTest.head
        if (itemToTest.equals(secondPerson)) return true
        if (peopleChecked.contains(itemToTest)) {
          hasSocialConnectionHelper(personToTest, remainingDirectConnectonsToTest.tail, peopleChecked)
        } else {
          hasSocialConnectionHelper(itemToTest, network.find(_._1 == itemToTest).get._2, peopleChecked)
        }
      }
    }

    if (isInNetwork(network, firstPerson) && isInNetwork(network, secondPerson)) {
      val networkItemToTest = network.find(_._1 == firstPerson).get
      hasSocialConnectionHelper(networkItemToTest._1, networkItemToTest._2)
    } else false
    // for each direct connection not in peopleChecked list
    // if target in direct connection return true
    // else
    // add to peopleChecked list
    // check in extended network (target)
    // false
  }
*/
  val johnFriendedWithMary = friend(nwJohnBillMary, "John", "Mary")
  println("John friended with Mary: " + johnFriendedWithMary)
//  println("Num John's friends: " + numFriendsOfPerson(johnFriendedWithMary,"John"))
  val maryAlsoFriendedWithBill = friend(johnFriendedWithMary, "Mary", "Bill")
  println(maryAlsoFriendedWithBill)
  val mbjkhs_interim = addPerson(addPerson(addPerson(maryAlsoFriendedWithBill, "Kevin"), "Harry"), "Sally")
  val mbjkhs = friend(mbjkhs_interim, "Bill", "Sally")
  println("mbjkhs = " + mbjkhs)
  val manWhoDoesntExist = "Mr Nobody"
  val womanWhoDoesntExist = "Mrs Nobody"
//  val linkBetweenNobodies = hasSocialConnection(mbjkhs, manWhoDoesntExist, womanWhoDoesntExist)
//  println("link nobodies = " + linkBetweenNobodies)
//  val linkBetweenNobodyAndSomebody = hasSocialConnection(mbjkhs, manWhoDoesntExist, "Mary")
//  println("link nobody and somebody = " + linkBetweenNobodyAndSomebody)
//  val linkBetweenTwoUnconnectedPeople = hasSocialConnection(mbjkhs, "Harry", "Kevin")
//  println("link 2 unconnected people = " + linkBetweenTwoUnconnectedPeople)
//  val linkBetweenConnectedAndUnconnectedPeople = hasSocialConnection(mbjkhs, "Harry", "Mary")
//  println("link connected with unconnected = " + linkBetweenConnectedAndUnconnectedPeople)
//  val linkDirectlyConnected = hasSocialConnection(mbjkhs, "John", "Mary")
//  println("link directly connected = " + linkDirectlyConnected)
//  val linkIndirectlyConnected = hasSocialConnection(mbjkhs, "John", "Sally")
//  println("link indirectly connected = " + linkIndirectlyConnected)

//  val thePersonWithMostFriends = personWithMostFriends(mbjkh)
//  println("Most friends = " + thePersonWithMostFriends)
//  println("Num people with no friends = " + numPeopleNoFriends(mbjkh))
  val john1FriendedWithMary = friend(nwJohnBillMary, "John1", "Mary")
  println("John1 friended with Mary: " + john1FriendedWithMary)
//  println("Num John1's friends: " + numFriendsOfPerson(john1FriendedWithMary,"John1"))
  val johnUnfriendedWithMary = unfriend(johnFriendedWithMary, "Mary", "John" )
  println("johnUnfriendedWithMary = " + johnUnfriendedWithMary)
  val jFmRemovedJ = removePerson(johnFriendedWithMary, "John")
  println ("jFmRemovedJ = " + jFmRemovedJ)

}

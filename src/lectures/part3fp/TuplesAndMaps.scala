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

  def addPerson_New (network : (Set[String], Set[List[String]]), newPerson : String) : (Set[String], Set[List[String]]) =
    (network._1 + newPerson, network._2)

  def friend_new (network : (Set[String], Set[List[String]]), personA : String, personB : String) : (Set[String], Set[List[String]]) = {
    if (network._1.contains(personA) && network._1.contains(personB)) {
      (network._1, network._2 + List(personA, personB))
    } else network
  }

/*  def nwEquivalent (people : Set[String], friendings : Set[List[String]]) : Map[String, Set[String]] = {
    def nwEquivalentHelper (remainingPeopleToProcess : Set[String], interimNWequivalent : Map[String, Set[String]])
  }

*/  val emptyNetwork : Map[String, Set[String]] = Map()
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

  def numFriendsOfPerson (network :Map[String, Set[String]], person : String) : Int = {
    if (network.contains(person)) network(person).size
    else -1
  }

 def personWithMostFriends (network :Map[String, Set[String]]) : String =
    network.maxBy(_._2.size)._1

  def numPeopleNoFriends(network: Map[String, Set[String]]) : Int =
    network.count(_._2.isEmpty)

  def hasSocialConnection(network : Map[String, Set[String]], personA: String, personB: String) = {
    @tailrec
    def socialConnectionHelper(peopleToTest : Set[String] = network(personA), peopleTested : Set[String] = Set()) : Boolean = {
      if (peopleToTest.isEmpty) false
      else {
        val testPerson = peopleToTest.head
        if (testPerson.equals(personB)) true
        else if (peopleTested.contains(testPerson)) socialConnectionHelper(peopleToTest.tail, peopleTested)
        else socialConnectionHelper(peopleToTest.tail ++ network(testPerson), peopleTested + testPerson)
      }
    }
    if (network.contains(personA) && network.contains(personB)) socialConnectionHelper()
    else false
  }

  val johnFriendedWithMary = friend(nwJohnBillMary, "John", "Mary")
  println("John friended with Mary: " + johnFriendedWithMary)
  println("Num John's friends: " + numFriendsOfPerson(johnFriendedWithMary,"John"))
  val maryAlsoFriendedWithBill = friend(johnFriendedWithMary, "Mary", "Bill")
  println(maryAlsoFriendedWithBill)
  val mbjkhs_interim = addPerson(addPerson(addPerson(maryAlsoFriendedWithBill, "Kevin"), "Harry"), "Sally")
  val mbjkhs = friend(mbjkhs_interim, "Bill", "Sally")
  println("mbjkhs = " + mbjkhs)
  val manWhoDoesntExist = "Mr Nobody"
  val womanWhoDoesntExist = "Mrs Nobody"
  val linkBetweenNobodies = hasSocialConnection(mbjkhs, manWhoDoesntExist, womanWhoDoesntExist)
  println("link nobodies = " + linkBetweenNobodies)
  val linkBetweenNobodyAndSomebody = hasSocialConnection(mbjkhs, manWhoDoesntExist, "Mary")
  println("link nobody and somebody = " + linkBetweenNobodyAndSomebody)
  val linkBetweenTwoUnconnectedPeople = hasSocialConnection(mbjkhs, "Harry", "Kevin")
  println("link 2 unconnected people = " + linkBetweenTwoUnconnectedPeople)
  val linkBetweenConnectedAndUnconnectedPeople = hasSocialConnection(mbjkhs, "Harry", "Mary")
  println("link connected with unconnected = " + linkBetweenConnectedAndUnconnectedPeople)
  val linkDirectlyConnected = hasSocialConnection(mbjkhs, "John", "Mary")
  println("link directly connected = " + linkDirectlyConnected)
  val linkIndirectlyConnected = hasSocialConnection(mbjkhs, "John", "Sally")
  println("link indirectly connected = " + linkIndirectlyConnected)

  val thePersonWithMostFriends = personWithMostFriends(mbjkhs)
  println("Most friends = " + thePersonWithMostFriends)
  println("Num people with no friends = " + numPeopleNoFriends(mbjkhs))
  val john1FriendedWithMary = friend(nwJohnBillMary, "John1", "Mary")
  println("John1 friended with Mary: " + john1FriendedWithMary)
  println("Num John1's friends: " + numFriendsOfPerson(john1FriendedWithMary,"John1"))
  val johnUnfriendedWithMary = unfriend(johnFriendedWithMary, "Mary", "John" )
  println("johnUnfriendedWithMary = " + johnUnfriendedWithMary)
  val jFmRemovedJ = removePerson(johnFriendedWithMary, "John")
  println ("jFmRemovedJ = " + jFmRemovedJ)
}

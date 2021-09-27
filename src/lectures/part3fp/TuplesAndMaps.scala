package lectures.part3fp

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

  def addPerson(network :Map[String, List[String]], newPerson : String) :  Map[String, List[String]]= {
     network + (newPerson ->  List())
  }

  val emptyNetwork : Map[String, List[String]] = Map()
  val nwJohnBillMary = addPerson(addPerson(addPerson(emptyNetwork, "John"), "Bill"), "Mary")
  println(nwJohnBillMary)

  def removePerson(network :Map[String, List[String]], removedPerson : String) :  Map[String, List[String]]=
    network.view.filterKeys(keyValue => !keyValue.equals(removedPerson)).toMap

  def johnAndBillOnly : Map[String, List[String]] = removePerson(nwJohnBillMary, "Mary")
  println("J & B only: "+ johnAndBillOnly)

  def friend (network :Map[String, List[String]], firstPerson : String, secondPerson : String) :  Map[String, List[String]] = {
    def conditionalMapIntoList: ((String, List[String]), String, String) => (String, List[String]) =
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

  def unFriend (network :Map[String, List[String]], firstPerson : String, secondPerson : String) :  Map[String, List[String]] = {
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

  val johnFriendedWithMary = friend(nwJohnBillMary, "John", "Mary")
  println("John friended with Mary: " + johnFriendedWithMary)
  println("Num John's friends: " + numFriendsOfPerson(johnFriendedWithMary,"John"))
  val maryAlsoFriendedWithBill = friend(johnFriendedWithMary, "Mary", "Bill")
  println(maryAlsoFriendedWithBill)
  val thePersonWithMostFriends = personWithMostFriends(maryAlsoFriendedWithBill)
  println("Most friends = " + thePersonWithMostFriends)
  val john1FriendedWithMary = friend(nwJohnBillMary, "John1", "Mary")
  println("John1 friended with Mary: " + john1FriendedWithMary)
  println("Num John1's friends: " + numFriendsOfPerson(john1FriendedWithMary,"John1"))
  val johnUnfriendedWithMary = unFriend(johnFriendedWithMary, "Mary", "John" )
  println(johnUnfriendedWithMary)
}

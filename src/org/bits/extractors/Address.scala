package org.bits.extractors

/**
 * Scala Extractors does pattern matching on its parameters rather than the Case Class.
 * While Case Classes can eliminate type or subtype of class.
 * Extractors can do pattern matching through its unapply implementation.
 */
object Address {

  def apply(houseNo: String, addr: String, place: String,
    city: String, pincode: Int) =
    houseNo + " " + addr + " " + place + " " + city + " " + pincode.toString

  /**
   * unapply function returns an Option with matching patterns for its parameters. 
   */
  def unapply(address: String): Option[(String, String, String, String, String)] = {
    val addressParts = address split " "
    if (addressParts.length == 5 && isValid(addressParts(4))) 
      Some(addressParts(0), addressParts(1), addressParts(2), addressParts(3), addressParts(4))
    else None
  }
  
  /**
   * We can also match individual parameter during the process. 
   */
  def isValid(pincode: String): Boolean = pincode.length match {
    case 6 => true
    case _ => false
  }
}


object Test extends Application {
	val addStr = "E-106 Roseland PimpleSaudagar Pune 411026"
	  
	println(Address.unapply(addStr))
	
	println(Address.unapply(Address.apply("E-106", "Roseland", "PimpleSaudagar", "Pune", 411027)))
	
	Address.unapply(addStr) match {
	  case Some(("E-106", "Roseland", "PimpleSaudagar", "Pune", "411026")) => Address.apply("E-106", "Roseland", "PimpleSaudagar", "Pune", 411026)
	  case _ =>  println("None")
	}
	
	
	def matchAddress(add: String): Boolean = add match {
		case Address(addStr) => true
		case _ => false
	}
	
	println("Address matched " + matchAddress(addStr))
}


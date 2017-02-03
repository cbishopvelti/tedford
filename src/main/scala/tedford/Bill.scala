package tedford

class Item (val name: String, val price: BigDecimal) {}

object Item {
	def apply (name: String, price: BigDecimal): Item = new Item(name, price)
}

object Bill {

	val items: Map[String, Item] = Map(
		"Cola" -> Item("Cola", BigDecimal("0.50")),
		"Coffie" -> Item("Coffie", BigDecimal("1.00")),
		"Cheese Sandwich" -> Item("Cheese Sandwich", BigDecimal("2.00")),
		"Steak Sandwich" -> Item("Steak Sandwich", BigDecimal("4.50"))
	)

	def main(args: Array[String]): Unit = {
		println("Hello World")
	}

	/**
	Takes a list of items as string which can include "Cola" or "Coffie"
	Performes validation to ensure that the list only contains valid items
	Returns the sum of the price
	*/
	def cacluratePriceInput (cart: List[String]): Option[BigDecimal] = {

		val cartTyped: Option[List[Item]] = lookup(cart);

		cartTyped match {
			case None => None
			case Some(items) => Some(calculatePrice(items))
		}
	}

	protected def lookup (cart: List[String]): Option[List[Item]] = {
		val cartTyped: List[Option[Item]] = cart.map(itemString => items.get(itemString))

		return allValid(cartTyped)
	}

	/**
	Converts List[Option] to Option[List] if any items ane None
	*/
	protected def allValid[T](items: List[Option[T]]): Option[List[T]] = {

		items.contains(None) match {
			case true => None
			case false => Some(items.map(itemOption => itemOption match {
				case Some(item) => item
				// probably a better way of doing this, could fold instead
				case None => throw new RuntimeException("returned None")
			}))
		}
	}

	/**
	Caculates the price
	*/
	protected def calculatePrice (cart: List[Item]): BigDecimal = {
		val cartPrice: BigDecimal = cart.foldRight[BigDecimal](BigDecimal("0.0")) {
			(item, a) => item.price + a
		}
		return cartPrice
	}

}
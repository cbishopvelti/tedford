package tedford


abstract class FoodType
case class Drink() extends FoodType
case class Food() extends FoodType
case class HotFood() extends FoodType


class Item (val name: String, val price: BigDecimal, val foodType: FoodType ) {}


object Item {
	def apply (name: String, price: BigDecimal, foodType: FoodType): Item = 
		new Item(name, price, foodType)
}

object Bill {

	val items: Map[String, Item] = Map(
		"Cola" -> Item("Cola", BigDecimal("0.50"), Drink()),
		"Coffie" -> Item("Coffie", BigDecimal("1.00"), Drink()),
		"Cheese Sandwich" -> Item("Cheese Sandwich", BigDecimal("2.00"), Food()),
		"Steak Sandwich" -> Item("Steak Sandwich", BigDecimal("4.50"), HotFood())
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
			case Some(items) => Some(calculatePriceWithServiceChange(items))
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
	Calculates the price including the service change
	*/
	protected def calculatePriceWithServiceChange (cart: List[Item]): BigDecimal = {
		val basePrice = calculatePrice (cart);

		val calculatedServiecChange = calculateServiceChange(cart)

		return basePrice + calculatedServiecChange
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

	/**
	Caclulates the service change
	*/
	protected def calculateServiceChange (cart: List[Item]): BigDecimal = {
		val basePrice = calculatePrice(cart)

		val isHotFood = cart.find(item => 
			item.foodType.isInstanceOf[HotFood]
		) 
		isHotFood match {
			case Some(_) => if ((basePrice * BigDecimal("0.2")) <= BigDecimal(20)) {
				return basePrice * 0.2
			} else {
				return BigDecimal("20")
			}
			case None => 
		}

		cart.find(item => item.foodType.isInstanceOf[Food]) match {
			case Some(_) => return basePrice * BigDecimal("0.1")
			case None => 
		}

		return 0.0
	}

}
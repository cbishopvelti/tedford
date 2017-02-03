package tedford

import org.specs2._
import tedford.Bill._
import scala.collection.immutable

class BillSpec extends Specification {
	def is = s2"""

	Bill

	Empty bill should return 0 					$emptyBill

	[Cola] bill should return 0.50 				$colaBill

	[Coffie] bill should return 1.00			$coffieBill

	[Cheese Sandwich] Bill should return 2.00 	$cheeseSandwichBill

	[Steak Sandwich] Bill should return 4.50 	$steakSandwichBill

	[Cola, Coffie, Cheese Sandwich] Bill should return 3.5 	$mixtureBill

	[Steak Sandwich * 100] Bill should return 470	$maxBill


	Invalid bill should return None				$invalidBill
	"""

	def emptyBill = {
		Bill.cacluratePriceInput(List()) must beEqualTo(Some(BigDecimal("0.0")))
	}

	def colaBill = {
		Bill.cacluratePriceInput(List("Cola")) must beEqualTo(Some(BigDecimal("0.5")))
	}

	def coffieBill = {
		Bill.cacluratePriceInput(List("Coffie")) must beEqualTo(Some(BigDecimal("1.0")))
	}

	def cheeseSandwichBill = {
		Bill.cacluratePriceInput(List("Cheese Sandwich")) must beEqualTo(Some(BigDecimal("2.2")))
	}

	def steakSandwichBill = {
		Bill.cacluratePriceInput(List("Steak Sandwich")) must beEqualTo(Some(BigDecimal("5.4")))
	}

	def mixtureBill = {
		Bill.cacluratePriceInput(List("Cola", "Coffie", "Cheese Sandwich")) must beEqualTo(Some(BigDecimal("3.85")))
	}

	def maxBill = {
		Bill.cacluratePriceInput(List.fill(100)("Steak Sandwich")) must beEqualTo(Some(BigDecimal("470")))
	}

	def invalidBill = {
		Bill.cacluratePriceInput(List("a rock")) must beEqualTo(None)
	}


}
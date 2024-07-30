object Ass6q1{
    case class Product(id: String, name: String, quantity: Int, price: Double)

    type Inventory = Map[String, Product]
  
    def getProductNames(inventory: Inventory): List[String] = {
        inventory.values.map(_.name).toList
    }

    def getTotalValue(inventory:Inventory):Double ={
        inventory.values.map(product =>product.quantity * product.price).sum
    }

    def isEmpty(inventory:Inventory): Boolean={
        inventory.isEmpty
    }

    def mergeInventory(inv1:Inventory, inv2:Inventory):Inventory ={
        inv2.foldLeft(inv1){ case (acc,(id,newProduct)) =>
            acc.updatedWith(id){
                case Some(existingProduct) =>
                    Some(existingProduct.copy(
                        quantity = existingProduct.quantity + newProduct.quantity,
                        price = Math.max(existingProduct.price, newProduct.price)
                    ))
                case None => Some(newProduct)
            }

        }
    }

    def printProductDetails(inventory: Inventory, productId: String): Unit = {
  inventory.get(productId) match {
    case Some(product) =>
      println(s"Product ID: ${product.id}, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}")
    case None =>
      println(s"Product with ID $productId does not exist.")
  }
}

    def main(args: Array[String]):Unit ={

    val product1 = Product("1", "Widget", 100, 2.50)
    val product2 = Product("2", "Gadget", 200, 5.00)
    val product3 = Product("3", "Doodad", 150, 3.75)

    val inventory1: Inventory = Map(
    product1.id -> product1,
    product2.id -> product2
    )

    val inventory2: Inventory = Map(
    product2.id -> product2.copy(quantity = 50, price = 4.50),
    product3.id -> product3
    )

    println ("\nRetrieve all product names")
    println(getProductNames(inventory1))  

    println("\nCalculate total value of all products")
    println(getTotalValue(inventory1))

    println("\nCheck if inventory is empty")
    println(isEmpty(inventory1))

    println("\nMerge two inventories")
    val mergedInventory = mergeInventory(inventory1, inventory2)
    println(mergedInventory)                

    println("\nCheck if a product with a specific ID exists and print its details")
    printProductDetails(mergedInventory, "2") 
    printProductDetails(mergedInventory, "4")  

    }

}
 
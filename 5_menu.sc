
//список имеющихся продуктов
val products = Array(("Bread", 2), ("Egg", 2), ("Ham", 3), ("Flour", 10))
//список рецептов - название и список необходимых продуктов
val recipes = List(("Sandwich", List(("Bread", 1), ("Ham", 2))), ("Omelet", List(("Egg", 1))), ("Ham Omelet", List(("Egg", 2), ("Ham", 2))))
//вспомогательное значение
val head = recipes.head

//проверка, хватает ли в списке продуктов нужного ингридиента
def check_products(need: (String, Int), products: Array[(String, Int)], i: Int = 0): Boolean = {
  if (i < products.length) {
    if (need._1 == products(i)._1 && need._2 <= products(i)._2) {
      true
    } else {
      check_products(need, products, i + 1)
    }
  } else {
    false
  }
}

//проверяет, хватает ли в списке продуктов ингридиентов на конкретное блюдо
def check_ingredients(dish: List[(String, Int)], products: Array[(String, Int)], i: Int = 0): Boolean = {
  if (i < dish.length) {
    if (check_products(dish(i), products)) {
      check_ingredients(dish, products, i + 1)
    } else {
      false
    }
  } else {
    true
  }
}

//добавление строки в меню
def add_in_menu(menu: Array[String], dish: String): Array[String] = {
  menu :+ dish
}

//добавление продуктов в список с изменением количества
def add_change_count(product: (String, Int), count: Int,
                     new_list: Array[(String, Int)]): Array[(String, Int)] = {
  new_list :+ (product._1, product._2 - count)
}

//добавление продуктов в список без изменения количества (продукт не нужен для рецепта)
def just_add(product: (String, Int),
             new_list: Array[(String, Int)]): Array[(String, Int)] = {
  new_list :+ (product)
}

//изменение списка продуктов после приготовления блюда
def change_product_count(product: (String, Int), old_list: Array[(String, Int)],
                         new_list: Array[(String, Int)] = Array.empty[(String, Int)], i: Int = 0): Array[(String, Int)] = {
  if (i < old_list.length) {
    if (product._1 == old_list(i)._1) {
      if (product._2 < old_list(i)._2) {
        change_product_count(product, old_list, add_change_count(old_list(i), product._2, new_list), i + 1)
      } else {
        change_product_count(product, old_list, new_list, i + 1)
      }
    } else {
      change_product_count(product, old_list, just_add(old_list(i), new_list), i + 1)
    }
  } else {
    new_list
  }
}

//изменение списка продуктов в процессе приготовления разных блюд
def change_product_list(dish: List[(String, Int)], list: Array[(String, Int)],
                        i: Int = 0): Array[(String, Int)] = {
  if (i < dish.length) {
    change_product_list(dish, change_product_count(dish(i), list), i + 1)
  } else {
    list
  }
}

//добавление блюда в меню с изменением списка продуктов и записью
//только после проверки возможности готовки
def create_menu(recipes: List[(String, List[(String, Int)])], products: Array[(String, Int)],
                menu: Array[String] = Array.empty[String], i: Int = 0): Array[String] = {
  if (i < recipes.length) {
    if (check_ingredients(recipes(i)._2, products)) {
      //print("I can cook " + recipes(i)._1)
      create_menu(recipes, change_product_list(recipes(i)._2, products), add_in_menu(menu, recipes(i)._1), i + 1)
    } else {
      //print("I can't cook " + recipes(i)._1 + "; ")
      create_menu(recipes, products, menu, i + 1)
    }
  } else {
    menu
  }
}

//лобавление рецепта в список
def add_recipe(recipes: List[(String, List[(String, Int)])], rec: (String, List[(String, Int)])): List[(String, List[(String, Int)])] ={
  recipes :+ rec
}

//сохранение одного из вариантов меню в список всех возможных
def add_var(variants: Array[Array[String]], menu: Array[String]): Array[Array[String]] = {
  variants :+ menu
}

//изменение списка рецептов для изменения возможного меню
def shuffle_menu(recipes: List[(String, List[(String, Int)])], products: Array[(String, Int)],
                 variants: Array[Array[String]] = Array.empty[Array[String]]): Array[Array[String]] = {
  if (recipes.head != head) {
    shuffle_menu(add_recipe(recipes.tail, recipes.head), products, add_var(variants, create_menu(recipes, products)))
  } else {
    variants
  }
}

//вывод результата
def print_variants(variants: Array[Array[String]], i: Int = 0): Unit ={
  if (i < variants.length){
    print("Menu " + i + ": " + variants(i).mkString(",") + "\n")
    print_variants(variants, i+1)
  }
}

print_variants(shuffle_menu(add_recipe(recipes.tail, recipes.head), products))


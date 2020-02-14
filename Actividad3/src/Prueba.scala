import sun.security.util.Length


object Prueba {
  
  def sumatoriaRecursivaConLimites(li : Int, num : Int) : Int = {
    if(num == li) li else num + sumatoriaRecursivaConLimites(li, num-1)
  }
  
  def divisores(inicio: Int, num : Int) : Unit = {
    if(inicio<=num){
      if(num%inicio==0){
        print(inicio+",")
      }
      divisores(inicio+1, num)
    }
  }
  
  def factoriales(li: Int, num : Int) : Int = {
    if(li==num) li else li*factoriales(li+1, num)
  }
  
  def divisoresEntre2(inicio: Int, num : Int) : Unit = {
    if(inicio<=num){
      if(num%inicio==0){
        print(inicio+",")
      }
      divisores(inicio+1, num)
    }
  }
  
  def cocientes(num1 : Int, num2 : Int) : Int = {
    if(num1>=num2){
      cocientes(num1-num2, num2)
    }else{
      num1
    }
  }
  
  def binario(num : Int):Unit={
    if(num<2){
      print(num)
    }else{
      binario(num/2)
      print(num%2)
    }
  }
  
  def contadorVocales(inicio : Int, sum : Int, cad : String) : Int = {
    var cont = sum
    if(inicio==cad.length()){
      sum
    }else{
      if(cad.substring(inicio, inicio+1).equalsIgnoreCase("a") || cad.substring(inicio, inicio+1).equalsIgnoreCase("e") || cad.substring(inicio, inicio+1).equalsIgnoreCase("i") || cad.substring(inicio, inicio+1).equalsIgnoreCase("o") || cad.substring(inicio, inicio+1).equalsIgnoreCase("u")){
        cont+=1
      }
      contadorVocales(inicio+1, cont, cad)
    }
  }
  
  
  def main(args: Array[String]): Unit = {
    println("Sumatoria")
    println(sumatoriaRecursivaConLimites(0, 5))
    
    println("Divisores")
    divisores(1, 20)
    println()
    
    println("Factoriales")
    println(factoriales(1, 10))
    
    println("Divisores entre 2")
    divisoresEntre2(5, 20)
    println()
    
    println("Cocientes")
    println(cocientes(60, 2))
    
    println("Convertir Binario")
    binario(50)
    println()
    
    println("Contador vocales")
    println(contadorVocales(0, 0, "Hola esta es una cadena de prueba aeiou"))
  }
  
}
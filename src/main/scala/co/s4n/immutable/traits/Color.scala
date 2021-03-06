package co.s4n.immutable.traits

class Color(val red: Int, val green: Int, val blue: Int) {

}

object Color {

  //Constructor for custom colors
  def apply(red: Int, green: Int, blue: Int): Color = {
    new Color(red, green, blue)
  }

  def apply(predefined: String): Color = predefined match {
    case "Rojo" => new Color(255, 0, 0)
    case "Amarillo" => new Color(255, 255, 0)
    case "Rosa" => new Color(255, 0, 255)
    case _ => new Color(0, 0, 0)
  }
}
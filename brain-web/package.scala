package object cws extends cws.GameImplicits with cws.BattleImplicits {
    def tomeNumToRoman(n : Int) : String = n match {
        case 1 => "I"
        case 2 => "II"
        case 3 => "III"
        case 4 => "IV"
        case 5 => "V"
        case 6 => "VI"
        case 7 => "VII"
        case 8 => "VIII"
        case 9 => "IX"
        case 10 => "X"
        case 11 => "XI"
        case _ => n.toString
    }
}

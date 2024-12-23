object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val grouped = inputLines.flatMap:
      case s"$first-$second" => List((first, second), (second, first))
    .groupBy(_._1)
    
    val tComputers = grouped.keys.filter(_.contains("t"))
    
    grouped.filter(_._1.contains("t")).flatMap:
      case list =>
        list.

    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")

def isConnected(list: List[String])


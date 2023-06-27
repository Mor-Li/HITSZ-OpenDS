import org.apache.spark.{SparkConf, SparkContext}

/**
  * 1,数据处理--查看是否有访问次数为0的记录
  */
object Counts {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("Counts")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")
    val data=sc.textFile("inputdata/data.csv")
    println(data.count())
    val data2=data.map{x=>val l=x.split(",");val ll=l.map(_.toDouble);(ll(0),ll.sum-ll(0))}
    val data3=data2.filter(_._2==0)
    println(data3.count)

  }

}

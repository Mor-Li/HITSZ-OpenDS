import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.{SparkConf, SparkContext}

object TT {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("logistic")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val data = sc.textFile("inputdata/data.csv")
    //print(data.collect().foreach(println))
    val spiltDate = data.map{x=> val lines = x.split(",");
      //println(lines(0).toDouble)
      //println(lines.length)
      println(Vectors.dense(lines.slice(1,lines.length).map(_.toDouble)))
    }
    spiltDate.collect()
    //println("===========")
    //val spiltDate1 = data.map(_.split(","))
    //spiltDate1.collect()
  }



}

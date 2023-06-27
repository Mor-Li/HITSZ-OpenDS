import org.apache.spark.ml.recommendation.ALS.Rating
import org.apache.spark.{SparkConf, SparkContext}

object RatingType {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("RatingType")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    //加载数据  user id::movie id::rating:: timestamp
    val data = sc.textFile("inputdata/rating.txt")
    //data中每条数据经过map的split后会是一个数组，模式匹配后，会new一个Rating对象
    val ratings = data.map(_.split("::") match { case Array(user, item, rate, ts) =>
      Rating(user.toInt, item.toInt,rate.toInt)
    })
    ratings.foreach(println)

  }

}

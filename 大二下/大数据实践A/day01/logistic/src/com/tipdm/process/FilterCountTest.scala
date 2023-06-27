package com.tipdm.process
import org.apache.spark.{SparkConf, SparkContext}
/**
 * 2,数据处理--过滤总访问次数为0的用户
 */
object FilterCountTest {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("FilterCount")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val data=sc.textFile("inputdata/data.csv")
    val data2=data.map{x=>val l=x.split(",");val ll=l.map(_.toDouble);ll}
    val data3=data2.filter(x=>(x.sum-x(0))!=0).map{x=>val y=x.map(_.toInt);y.mkString(",")}
    //data3.foreach(println)
    data3.repartition(1).saveAsTextFile("inputdata/data3")
    sc.stop()
  }

}

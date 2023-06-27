package com.tipdm.training

import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.stat.Statistics
import org.apache.spark.{SparkConf, SparkContext}

object StatisticsTest {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("Statistics")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val data = sc.textFile("inputdata/stat.txt")
      .map(_.split(" ")).map(f=>f.map(f=>f.toDouble))
    val data1 = data.map(f=>Vectors.dense(f))
    data1.foreach(println)
    val stat1 = Statistics.colStats(data1)
    println("最大值:"+stat1.max)  //最大值
    println("最小值:"+stat1.min)  //最小值
    println("均值:"+stat1.mean) //均值
    println("方差:"+stat1.variance) //方差
    println("L1范数:"+stat1.normL1)  //L1范数
    println("L2范数:"+stat1.normL2)  //L2范数   平方和 后开方
    //val corr1=Statistics.corr(data1,"pearson")
    //println(corr1)
  }

}

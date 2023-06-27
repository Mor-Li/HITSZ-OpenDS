package com.tipdm

import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.mllib.recommendation.ALS
import org.apache.spark.mllib.recommendation.Rating

/**
 * 创建ALS模型，需要输入以下参数
 * trainDataPath: 输入原始数据，包含（用户，项目，评分）
 * modelPath：模型存储目录
 * rank ： 用户、项目子矩阵
 * iteration： 循环次数；
 * lambda ： 防止过拟合参数；
 * splitter： 输入原始数据分隔符；
 */
object ALSModelCreate {
  val appName = "Create ALS Model "
  val conf = new SparkConf().setAppName(appName)
  val sc = new SparkContext(conf)
  sc.setLogLevel("WARN")

  def main(args: Array[String]) = {
    if (args.length != 6) {
      System.err.println("Usage: ALSModelCreate requires: 6 input fields <trainDataPath> <modelPath>  " +
        "<rank> <iteration> <lambda> <splitter>")
    }
    // 匹配输入参数
    val trainDataPath = args(0)
    val modelPath = args(1)
    val rank = args(2).toInt
    val iteration = args(3).toInt
    val lambda = args(4).toDouble
    val splitter = args(5)

    // 加载训练集数据
    val trainData = sc.textFile(trainDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter);
      (fields(0).toInt,fields(1).toInt,fields(2).toDouble)}
    val trainDataRating= trainData.map(x=>Rating(x._1,x._2,x._3))

    // 建立ALS模型
    val model = ALS.train(trainDataRating, rank, iteration, lambda)
    // 存储ALS模型
    model.save(sc,modelPath)
    println("Model saved")
    sc.stop()
  }
}
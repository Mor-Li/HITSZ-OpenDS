package com.tipdm

import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.rdd.RDD

/**
 * 评估基于物品的协同过滤模型，需要输入以下参数
 * testDataPath: 测试数据（userid,itemid）
 * modelPath：模型存储目录
 * minRatedNumPerUser:单用户评价菜品的最小次数
 * kList:推荐数量K值列表
 * splitter:输入原始数据分隔符
 * resultPath：评估结果的存储目录
 */
object ItemBasedModelEvaluate {
  val appName = " ItemBased CF Model Evaluate "
  val conf = new SparkConf().setAppName("appName")
  val sc = new SparkContext()

  def main(args: Array[String]) = {
    if (args.length != 6) {
      System.err.println("Usage:com.tipdm.ItemBaseModelEvaluate <trainDataPath> " +
        "<modelPath> <minRatedNumPerUser> <kList> <splitter><resultPath>")
    }
    // 匹配输入参数
    val testDataPath = args(0)
    val modelPath = args(1)
    val minRatedNumPerUser = args(2).toInt
    val kList = args(3).split(",").map(_.toInt)
    val splitter = args(4)
    val resultPath = args(5)

    // 加载模型
    val dataModel:RDD[(Int, List[(Int)])] = sc.objectFile[(Int, List[(Int)])](modelPath)

    // 加载测试集记录
    val dataTest = sc.textFile(testDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter); (fields(0).toInt,fields(1).toInt)
    }.distinct
    println("test records: " + dataTest.count() )

    // 过滤 测试集记录 （user,item）
    val testData = dataTest.groupBy(x=>x._1).filter(x=>x._2.size>=minRatedNumPerUser).flatMap(x=>x._2)
    println("filtered test records: " + testData.count() )
    val sharedUserIds = testData.keys.distinct.intersection(dataModel.keys.distinct).collect.toList
    println("shared user records: " + sharedUserIds.size )
    val testUserRecords = testData.filter(data=>sharedUserIds.contains(data._1))
    val testUserRated = testUserRecords.combineByKey(
      (x:Int) => List(x),
      (c:List[Int], x:Int) => x :: c ,
      (c1:List[Int], c2:List[Int]) => c1 ::: c2).cache()

    // 不同K值下的召回率，准确率
    val results = for ( k <- kList ;
                        // 与测试集比较，获得匹配的推荐记录数：（User,TestNum,RecommendNum,MatchedNum）
                        val finalResult = testUserRated.join(dataModel.map(x=>(x._1,x._2.take(k)))).
                          map(x => (x._1,x._2._1.size,x._2._2.size,x._2._1.intersect(x._2._2).size));
                        val matchedNum = finalResult.map(x=>(x._4)).sum.toInt;
                        val recall_precesion = finalResult.collect.map(x => (x._4.toDouble/x._2,x._4.toDouble/x._3));
                        val real_recall_precesion = recall_precesion.reduceLeft((x,y) => (x._1+y._1,x._2+y._2))
    ) yield (k, recall_precesion.size,matchedNum,
        real_recall_precesion._1 * 100/ recall_precesion.size ,
        real_recall_precesion._2 * 100/ recall_precesion.size )

    //存储结果
    sc.parallelize(results.map(x => x._1 + "," + x._2 + ","+x._3+ ","+x._4+ "," + x._5)).repartition(1).saveAsTextFile(resultPath)
    println("Evaluation completed.")
    sc.stop()
  }
}

package com.tipdm

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

/**
  * 评估基于用户的协同过滤模型，需要输入以下参数
  * testDataPath: 测试数据（userid,itemid）
  * modelPath：模型存储目录
  * minRatedNumPerUser:单用户评价菜品的最小次数
  * kList:推荐数量K值列表
  * resultPath：评估结果的存储目录
  * splitter:输入原始数据分隔符
  */

object UserBasedModelEvaluate {
  val appName = "UserBased CF Model Evaluate"
  val conf = new SparkConf().setAppName(appName)
  val sc = new SparkContext(conf)
  sc.setLogLevel("WARN")

   def main(args: Array[String]) = {
     if (args.length != 6) {
       System.err.println("Usage:ModelEvaluate requires:<testDataPath>,<modelPath>,<minRatedNumPerUser>,<kList>,<resultPath>,<splitter>")
     }
     // 匹配输入参数
     val testDataPath = args(0)
     val modelPath = args(1)
     val minRatedNumPerUser = args(2).toInt
     val kList = args(3).split(",").map(_.toInt)
     val resultPath = args(4)
     val splitter = args(5)

     // 加载推荐模型,为每个user抽取前K（最大值）个item记录
     val dataModel:RDD[(Int, List[(Int)])] = sc.objectFile[(Int, List[(Int)])](modelPath)
     val searchResult = dataModel.map(x=>(x._1,x._2.take(kList.max))).cache()
     // 加载测试集记录
     val dataTest = sc.textFile(testDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter); (fields(0).toInt,fields(1).toInt)
     }.distinct
     println("test records: " + dataTest.count() )

     // 过滤测试集记录 （user,item）
     val testData = dataTest.groupBy(x=>x._1).filter(x=>x._2.size>=minRatedNumPerUser).flatMap(x=>x._2)
     val sharedUserIds = testData.keys.distinct.intersection(dataModel.keys.distinct).collect.toList
     println("shared user records: " + sharedUserIds.size )
     val testUserRecords = testData.filter(data=>sharedUserIds.contains(data._1))
     println("filtered test records: " + testUserRecords.count() )
     val testUserRated = testUserRecords.combineByKey(
       (x:Int) => List(x),
       (c:List[Int], x:Int) => x :: c ,
       (c1:List[Int], c2:List[Int]) => c1 ::: c2).cache()

     // 以模型的推荐item与测试集的item，进行匹配比较
     // 计算不同的K值下的recall, precision值
     val results = for ( k <- kList ;
                         // 与测试集比较，获得matched记录数：
                         val initResult = testUserRated.join(searchResult.map(x=>(x._1,x._2.take(k))));
                         val finalResult = initResult.map(x => (x._1,x._2._1.size,x._2._2.size,x._2._1.intersect(x._2._2).size));
                         val matchedNum = finalResult.map(x=>(x._4)).sum.toInt;
                         val recall_precesion = finalResult.collect.map(x => (x._4.toDouble/x._2,x._4.toDouble/x._3));
                         val real_recall_precesion = recall_precesion.reduceLeft((x,y) => (x._1+y._1,x._2+y._2))
     ) yield (k,recall_precesion.size,matchedNum,
         real_recall_precesion._1 * 100/ recall_precesion.size ,
         real_recall_precesion._2 * 100/ recall_precesion.size )

     //存储结果
     sc.parallelize(results.map(x => x._1 + "," + x._2 + "," + x._3+ "," + x._4+ "," + x._5)).repartition(1).saveAsTextFile(resultPath)
     println("Evaluation completed.")
     sc.stop()
   }
 }

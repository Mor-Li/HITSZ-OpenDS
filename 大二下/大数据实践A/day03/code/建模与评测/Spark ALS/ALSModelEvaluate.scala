package com.tipdm

import org.apache.spark.mllib.recommendation.{MatrixFactorizationModel, Rating}
import org.apache.spark.{SparkConf, SparkContext}

/**
  * 对ALS推荐模型进行评测
  * 输入参数： <trainDataPath> <testDataPath><modelPath><minRatedNumPerUser><kList><resultPath><splitter>
  * trainDataPath： 过滤后的训练数据存储路径
  * testDataPath： 测试数据存储路径
  * modelPath： ALS模型路径
  * minRatedNumPerUser:单用户评价菜品的最小次数
  * kList: 推荐数量K值列表
  * resultPath：结果输出路径
  * splitter：原始数据分隔符；
  */
object ALSModelEvaluate {
   val appName = "Evaluate Spark ALS Model"
   val conf = new SparkConf().setAppName(appName)
   val sc = new SparkContext(conf)
   sc.setLogLevel("WARN")

   def main(args:Array[String]) = {
     if (args.length != 7) {
       System.err.println("Usage:ALSModelEvaluate requires: <trainDataPath> <testDataPath><modelPath><minRatedNumPerUser><kList><resultPath><splitter>")
     }
     // 匹配输入参数
     val trainDataPath = args(0)
     val testDataPath = args(1)
     val modelPath = args(2)
     val minRatedNumPerUser = args(3).toInt
     val kList = args(4).split(",").map(_.toInt)
     val resultPath = args(5)
     val splitter = args(6)

     // 加载 ALS model
     val model = MatrixFactorizationModel.load(sc, modelPath)
     model.productFeatures.cache
     model.userFeatures.cache
     println("model retrieved.")

     // 加载训练集数据
     val trainData = sc.textFile(trainDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter);
       (fields(0).toInt,fields(1).toInt,fields(2).toDouble)}

     // 加载测试集数据
     val testData = sc.textFile(testDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter); (fields(0).toInt,fields(1).toInt)}
     val testDataFiltered = testData.groupBy(_._1).filter(data=>data._2.toList.size>=minRatedNumPerUser).flatMap(_._2).distinct()
     println("filtered test records  count : " + testDataFiltered.count) //
     val testUserList = testData.keys.distinct().collect().toList
     val testRecordSet = testData.distinct().collect.toSet
     println("All test users: " + testUserList.size)
     println("All test records: " + testRecordSet.size)

     // 计算训练集与测试集中的共有用户，以及测试集中的相关记录。
     val sharedUserList = trainData.map(_._1).distinct.collect.toList.intersect(testDataFiltered.map(_._1).distinct.collect.toList)
     val testSharedSet = testDataFiltered.filter(data=>sharedUserList.contains(data._1)).collect.toSet
     println("ShareUsers: " + sharedUserList.size)
     println("Test Records: " + testSharedSet.size)

     // 创建评测结果集
     val evaluationRecords = new scala.collection.mutable.ListBuffer[(Int,Double,Double)]()

     // 评测model推荐的结果
     // 计算不同Ｋ值下的recall,precision
     for (k <- kList){
       println("================== K="+k+" ==================")
       var recommendNumSum=0
       var matchedNumSum = 0
       for(uid <- sharedUserList){
         val initRecommendRecords = model.recommendProducts(uid, k).map { case Rating(user, item, rating) => (user, item) }
         // 在剩余的推荐数据集中，匹配测试集中的总数
         val matchedNum = initRecommendRecords.toSet.intersect(testSharedSet).size
         matchedNumSum += matchedNum
       }
       recommendNumSum = sharedUserList.size*k
       val recall: Double = (matchedNumSum.toDouble / testSharedSet.size) * 100
       val precision: Double = (matchedNumSum.toDouble / recommendNumSum) * 100
       evaluationRecords +=((k,recall,precision))
       println(k + ": "+ sharedUserList.size + ": " + matchedNumSum + ": " + recall + ": "+ precision)
     }
     val result = sc.parallelize(evaluationRecords.toSeq).repartition(1)
     result.saveAsTextFile(resultPath)
     sc.stop()
   }
 }
